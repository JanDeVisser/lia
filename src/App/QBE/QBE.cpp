/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <cstdio>
#include <cstring>
#include <expected>
#include <filesystem>
#include <format>
#include <fstream>
#include <ranges>
#include <string>
#include <string_view>
#include <variant>

#include <Util/Align.h>
#include <Util/Logging.h>
#include <Util/Options.h>
#include <Util/Pipe.h>
#include <Util/Process.h>
#include <Util/Utf8.h>

#include <App/Config.h>
#include <App/Operator.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>
#include <App/Value.h>

#include <App/Parser.h>
#include <App/QBE/QBE.h>

namespace Lia::QBE {

using GenResult = std::expected<ILValue, std::wstring>;

Local Local::value(int var)
{
    return (var > 0) ? Local { LocalType::Value, var } : Local {};
}

Local Local::ref(int var)
{
    return Local { LocalType::Reference, var };
}

namespace QBEType {

ILType qbe_type(IntType const &descr, pType const &)
{
    return (descr.width_bits < 64) ? ILBaseType::W : ILBaseType::L;
}

ILType qbe_type(BoolType const &, pType const &)
{
    return ILBaseType::W;
}

ILType qbe_type(FloatType const &descr, pType const &)
{
    return (descr.width_bits < 64) ? ILBaseType::S : ILBaseType::D;
}

ILType qbe_type(ZeroTerminatedArray const &, pType const &)
{
    return ILBaseType::L;
}

ILType qbe_type(SliceType const &, pType const &)
{
    return L":slice_t";
}

ILType qbe_type(StructType const &strukt, pType const &type)
{
    return std::format(L":struct{}", *(type.id));
}

ILType qbe_type(auto const &, pType const &)
{
    return ILBaseType::L;
}

ILType qbe_type(pType const &type)
{
    return std::visit(
        [&type](auto const &descr) -> ILType {
            return qbe_type(descr, type);
        },
        type->description);
}

}

struct QBEContext {
    int       next_label;
    int       next_var;
    fs::path  file_name;
    bool      is_export { false };
    ILProgram program {};
    size_t    current_file;
    size_t    current_function;

    ILValue add_string(std::wstring_view s)
    {
        auto &file { program.files[current_file] };
        for (auto const &[ix, str] : std::ranges::views::enumerate(file.strings)) {
            if (str == s) {
                return ILValue::string(ix + 1);
            }
        }
        file.strings.emplace_back(s);
        return ILValue::string(file.strings.size());
    }

    ILValue add_cstring(std::string_view s)
    {
        auto &file { program.files[current_file] };
        for (auto const &[ix, str] : std::ranges::views::enumerate(file.cstrings)) {
            if (str == s) {
                return ILValue::cstring(ix + 1);
            }
        }
        file.cstrings.emplace_back(s);
        return ILValue::cstring(file.cstrings.size());
    }

    ILType qbe_type(pType const &type)
    {
        auto ret = QBEType::qbe_type(type);
        if (std::holds_alternative<std::wstring>(ret)) {
            auto &file { program.files[current_file] };
            auto  found { false };
            for (auto const &t : file.types) {
                if (type == t) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                file.types.emplace_back(type);
            }
        }
        return ret;
    }

    void add_operation(ILInstructionImpl impl)
    {
        auto &file { program.files[current_file] };
        auto &function { file.functions[current_function] };
        if (std::holds_alternative<LabelDef>(impl)) {
            auto const &label_def = std::get<LabelDef>(impl);
            if (function.labels.size() < label_def.label + 1) {
                function.labels.resize(label_def.label + 1);
            }
            function.labels[label_def.label] = function.instructions.size();
        }
        if (trace_on()) {
            std::wstringstream s;
            std::visit(
                [&s](auto const &i) -> void {
                    s << i;
                },
                impl);
            trace(L"Adding QBE operation {}", s.str());
        }
        function.instructions.emplace_back(std::move(impl));
    }
};

using QBEContexts = std::vector<QBEContext>;

GenResult generate_qbe_node(ASTNode const &, QBEContext &);
GenResult generate_qbe_nodes(ASTNodes const &, QBEContext &);

bool qbe_first_class_type(pType const &type)
{
    auto const &t = type->value_type();
    return std::visit(
        overloads {
            [](IntType const &) -> bool {
                return true;
            },
            [](FloatType const &) -> bool {
                return true;
            },
            [](BoolType const &) -> bool {
                return true;
            },
            [](ZeroTerminatedArray const &) -> bool {
                return true;
            },
            [](auto const &) -> bool {
                return false;
            },
        },
        t->description);
}

ILBaseType qbe_type_code(IntType const &type)
{
    switch (type.width_bits) {
    case 8:
        return ILBaseType::B;
    case 16:
        return ILBaseType::H;
    case 32:
        return ILBaseType::W;
    case 64:
        return ILBaseType::L;
    default:
        UNREACHABLE();
    }
}

ILBaseType qbe_type_code(FloatType const &type)
{
    switch (type.width_bits) {
    case 32:
        return ILBaseType::S;
    case 64:
        return ILBaseType::D;
    default:
        UNREACHABLE();
    }
}

ILBaseType qbe_type_code(ZeroTerminatedArray const &)
{
    return ILBaseType::L;
}

ILBaseType qbe_type_code(BoolType const &)
{
    return ILBaseType::W;
}

ILBaseType qbe_type_code(StructType const &)
{
    return ILBaseType::L;
}

ILBaseType qbe_type_code(auto const &type)
{
    int                   status;
    std::type_info const &ti = typeid(type);
    auto                 *realname = abi::__cxa_demangle(ti.name(), NULL, NULL, &status);
    warning("Assuming qbe_type_code(`{}') is `l`", realname);
    return ILBaseType::L;
}

ILBaseType qbe_type_code(pType const &type)
{
    return std::visit(
        [](auto const &descr) -> ILBaseType {
            return qbe_type_code(descr);
        },
        type->description);
}

ILBaseType qbe_load_code(pType const &type)
{
    return std::visit(
        overloads {
            [](IntType const &int_type) -> ILBaseType {
                switch (int_type.width_bits) {
                case 8:
                    return (int_type.is_signed) ? ILBaseType::SB : ILBaseType::UB;
                case 16:
                    return (int_type.is_signed) ? ILBaseType::SH : ILBaseType::UH;
                case 32:
                    return (int_type.is_signed) ? ILBaseType::SW : ILBaseType::UW;
                case 64:
                    return ILBaseType::L;
                default:
                    UNREACHABLE();
                }
            },
            [](BoolType const &) -> ILBaseType {
                return ILBaseType::W;
            },
            [](FloatType const &float_type) -> ILBaseType {
                return qbe_type_code(float_type);
            },
            [](EnumType const &enum_type) -> ILBaseType {
                return qbe_load_code(enum_type.underlying_type);
            },
            [](auto const &) -> ILBaseType {
                return ILBaseType::L;
            } },
        type->description);
}

struct QBEOperand {
    ILValue value;
    ASTNode node;
};

struct QBEBinExpr {
    QBEOperand lhs;
    Operator   op;
    QBEOperand rhs;
};

struct QBEUnaryExpr {
    Operator   op;
    QBEOperand operand;
};

#define QBE_ASSERT(expr, ctx)                                            \
    do {                                                                 \
        if (!(expr)) {                                                   \
            fatal("{}:{} Assertion failed: " #expr, __FILE__, __LINE__); \
        }                                                                \
    } while (0)

#define TRY_GENERATE(n, ctx)                                          \
    (                                                                 \
        {                                                             \
            ASTNode __n = (n);                                        \
            ILValue __var;                                            \
            if (auto __res = generate_qbe_node(__n, (ctx)); !__res) { \
                return __res;                                         \
            } else {                                                  \
                __var = __res.value();                                \
                if (__var.align == 0) {                               \
                    __var.align = __n->bound_type->align_of();        \
                }                                                     \
                if (__var.size == 0) {                                \
                    __var.size = __n->bound_type->size_of();          \
                }                                                     \
            }                                                         \
            (QBEOperand) { __var, __n };                              \
        })

GenResult generate_qbe_nodes(ASTNodes const &nodes, QBEContext &ctx)
{
    ILValue var { ILValue::null() };
    for (auto const &n : nodes) {
        auto operand { TRY_GENERATE(n, ctx) };
        var = operand.value;
    }
    return var;
}

#define TRY_GENERATE_NODES(n, ctx)                                     \
    (                                                                  \
        {                                                              \
            Alloc __var {};                                            \
            if (auto __res = generate_qbe_nodes((n), (ctx)); !__res) { \
                return __res;                                          \
            } else {                                                   \
                __var = __res.value();                                 \
            }                                                          \
            (__var);                                                   \
        })

static ILValue dereference(QBEOperand const &operand, QBEContext &ctx)
{
    auto value = operand.value;
    auto type = operand.node->bound_type;
    if (is<ReferenceType>(type)) {
        type = get<ReferenceType>(type).referencing;
    }
    return std::visit(
        overloads {
            [&ctx, &value, &operand, &type](Local local) -> ILValue {
                if (local.type == LocalType::Reference) {
                    if (qbe_first_class_type(operand.node->bound_type)) {
                        value = ILValue::local(++ctx.next_var, ctx.qbe_type(type));
                        ctx.add_operation(
                            LoadDef {
                                .pointer = ILValue::local(local.var, ILBaseType::L),
                                .target = value,
                            });
                    }
                }
                return value;
            },
            [&value, &ctx, &operand, &type](ILValue::Variable variable) -> ILValue {
                if (qbe_first_class_type(operand.node->bound_type)) {
                    value = ILValue::local(++ctx.next_var, ctx.qbe_type(type));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = ILValue::variable(variable.name, ILBaseType::L),
                            .target = value,
                        });
                }
                return value;
            },
            [&value](auto) -> ILValue {
                return value;
            } },
        value.inner);
}

static ILValue binexpr(QBEBinExpr const &expr, ILOperation op, ILType type, QBEContext &ctx)
{
    auto    lhs_value = dereference(expr.lhs, ctx);
    auto    rhs_value = dereference(expr.rhs, ctx);
    int     var = ++ctx.next_var;
    ILValue ret = ILValue::local(var, type);
    ctx.add_operation(
        ExprDef {
            lhs_value,
            rhs_value,
            op,
            ret,
        });
    return ret;
}

template<class TLeft, class TRight = TLeft>
static GenResult qbe_operator(QBEBinExpr const &expr, TLeft const &lhs, TRight const &rhs, QBEContext &ctx)
{
    fatal(
        L"Invalid operator `{}` `{}` `{}`",
        expr.lhs.node->bound_type->to_string(),
        as_wstring(Operator_name(expr.op)),
        expr.rhs.node->bound_type->to_string());
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, BoolType const &lhs, BoolType const &rhs, QBEContext &ctx)
{
    ILOperation op;
    switch (expr.op) {
    case Operator::LogicalAnd:
        op = ILOperation::And;
        break;
    case Operator::LogicalOr:
        op = ILOperation::Or;
        break;
    default:
        NYI("QBE mapping for bool operator `{}`", Operator_name(expr.op));
        break;
    }
    return binexpr(expr, op, ILBaseType::W, ctx);
}

static constexpr wchar_t const *division_by_zero { L"Division by zero" };

static void check_division_by_zero(ILValue const &rhs, QBEContext &ctx)
{
    auto zero { ILValue::local(++ctx.next_var, ILBaseType::W) };
    int  carry_on = ++ctx.next_label;
    int  abort_mission = ++ctx.next_label;
    ctx.add_operation(
        ExprDef {
            ILValue::integer(0, rhs.type),
            rhs,
            ILOperation::Equals,
            zero,
        });
    ctx.add_operation(
        JnzDef {
            zero,
            abort_mission,
            carry_on,
        });
    ctx.add_operation(LabelDef { abort_mission });
    ILValue div_by_zero_id = ctx.add_string(division_by_zero);
    CallDef call_def = {
        L"libliart:lia$abort",
        ILValue::null(),
    };
    call_def.args.push_back(div_by_zero_id);
    call_def.args.push_back(ILValue::integer(wcslen(division_by_zero), ILBaseType::L));
    ctx.add_operation(call_def);
    ctx.add_operation(LabelDef { carry_on });
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, IntType const &lhs, IntType const &rhs, QBEContext &ctx)
{
    auto lhs_value = dereference(expr.lhs, ctx);
    auto rhs_value = dereference(expr.rhs, ctx);
    if (expr.op == Operator::Divide || expr.op == Operator::Modulo) {
        check_division_by_zero(rhs_value, ctx);
    }
    ILOperation op;
    ILType      type { lhs_value.type };
    switch (expr.op) {
    case Operator::Add:
        op = ILOperation::Add;
        break;
    case Operator::BinaryAnd:
        op = ILOperation::And;
        break;
    case Operator::BinaryOr:
        op = ILOperation::Or;
        break;
    case Operator::BinaryXor:
        op = ILOperation::Xor;
        break;
    case Operator::Divide:
        op = (lhs.is_signed) ? ILOperation::Div : ILOperation::UDiv;
        break;
    case Operator::Equals:
        op = ILOperation::Equals;
        type = ILBaseType::W;
        break;
    case Operator::Greater:
        op = ILOperation::Greater;
        type = ILBaseType::W;
        break;
    case Operator::GreaterEqual:
        op = ILOperation::GreaterEqual;
        type = ILBaseType::W;
        break;
    case Operator::Less:
        op = ILOperation::Less;
        type = ILBaseType::W;
        break;
    case Operator::LessEqual:
        op = ILOperation::LessEqual;
        type = ILBaseType::W;
        break;
    case Operator::Modulo:
        op = (lhs.is_signed) ? ILOperation::Mod : ILOperation::UMod;
        type = ILBaseType::W;
        break;
    case Operator::Multiply:
        op = ILOperation::Mul;
        type = ILBaseType::W;
        break;
    case Operator::NotEqual:
        op = ILOperation::NotEqual;
        type = ILBaseType::W;
        break;
    case Operator::ShiftLeft:
        op = ILOperation::Shl;
        break;
    case Operator::ShiftRight:
        op = ILOperation::Shr;
        break;
    case Operator::Subtract:
        op = ILOperation::Sub;
        break;
    default:
        NYI("QBE mapping for int operator `{}`", Operator_name(expr.op));
        break;
    }
    return binexpr(expr, op, type, ctx);
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, FloatType const &lhs, FloatType const &rhs, QBEContext &ctx)
{
    auto lhs_value = dereference(expr.lhs, ctx);
    auto rhs_value = dereference(expr.rhs, ctx);
    if (expr.op == Operator::Divide || expr.op == Operator::Modulo) {
        check_division_by_zero(rhs_value, ctx);
    }
    ILOperation op;
    switch (expr.op) {
    case Operator::Add:
        op = ILOperation::Add;
        break;
    case Operator::Divide:
        op = ILOperation::Div;
        break;
    case Operator::Equals:
        op = ILOperation::Equals;
        break;
    case Operator::Greater:
        op = ILOperation::Greater;
        break;
    case Operator::GreaterEqual:
        op = ILOperation::GreaterEqual;
        break;
    case Operator::Less:
        op = ILOperation::Less;
        break;
    case Operator::LessEqual:
        op = ILOperation::LessEqual;
        break;
    case Operator::Multiply:
        op = ILOperation::Mul;
        break;
    case Operator::NotEqual:
        op = ILOperation::NotEqual;
        break;
    case Operator::Subtract:
        op = ILOperation::Sub;
        break;
    default:
        NYI("QBE mapping for float operator `{}`", Operator_name(expr.op));
        break;
    }
    return binexpr(expr, op, lhs_value.type, ctx);
}

GenResult qbe_operator(QBEBinExpr const &expr, StructType const &lhs, auto const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::MemberAccess) {
        auto id = get<Identifier>(expr.rhs.node);
        auto ret = ILValue::local_ref(++ctx.next_var);
        ctx.add_operation(
            ExprDef {
                expr.lhs.value,
                ILValue::integer(lhs.offset_of(id.identifier), ILBaseType::L),
                ILOperation::Add,
                ret,
            });
        return ret;
    }
    NYI("QBE mapping for struct operator `{}`", Operator_name(expr.op));
}

static GenResult qbe_operator(QBEBinExpr const &expr, QBEContext &ctx)
{
    auto const &lhs_value_type = expr.lhs.node->bound_type->value_type();
    auto const &rhs_value_type = expr.rhs.node->bound_type->value_type();
    return std::visit(
        [&expr, &ctx](auto const &lhs_descr, auto const &rhs_descr) {
            return qbe_operator(expr, lhs_descr, rhs_descr, ctx);
        },
        lhs_value_type->description, rhs_value_type->description);
}

template<class T>
static GenResult qbe_operator(QBEUnaryExpr const &expr, T const &operand, QBEContext &ctx)
{
    fatal(L"Invalid operator `{}` `{}` ",
        expr.operand.node->bound_type->to_string(),
        as_wstring(Operator_name(expr.op)));
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, BoolType const &bool_type, QBEContext &ctx)
{
    auto operand = dereference(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.type);
    switch (expr.op) {
    case Operator::LogicalInvert: {
        ctx.add_operation(
            ExprDef {
                operand,
                ILValue::integer(1, ILBaseType::W),
                ILOperation::Xor,
                var,
            });
        auto ret = ILValue::local(++ctx.next_var, operand.type);
        ctx.add_operation(
            ExprDef {
                var,
                ILValue::integer(1, ILBaseType::W),
                ILOperation::And,
                ret,
            });
        return ret;
    }
    default:
        NYI("QBE mapping for bool operator `{}`", Operator_name(expr.op));
        break;
    }
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, IntType const &int_type, QBEContext &ctx)
{
    auto operand = dereference(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.type);
    switch (expr.op) {
    case Operator::Negate:
        ctx.add_operation(
            ExprDef {
                operand,
                ILValue::null(),
                ILOperation::Neg,
                var,
            });
        return var;
    case Operator::BinaryInvert: {
        ctx.add_operation(
            ExprDef {
                operand,
                ILValue::literal(L"~0", operand.type),
                ILOperation::Xor,
                var,
            });
        auto ret = ILValue::local(++ctx.next_var, operand.type);
        ctx.add_operation(
            ExprDef {
                var,
                ILValue::literal(L"~0", operand.type),
                ILOperation::And,
                ret,
            });
        return ret;
    }
    default:
        NYI("QBE mapping for int operator `{}`", Operator_name(expr.op));
        break;
    }
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, FloatType const &float_type, QBEContext &ctx)
{
    auto operand = dereference(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.type);
    switch (expr.op) {
    case Operator::Negate:
        ctx.add_operation(
            ExprDef {
                operand,
                ILValue::null(),
                ILOperation::Neg,
                var,
            });
        return var;
    default:
        NYI("QBE mapping for float operator `{}`", Operator_name(expr.op));
        break;
    }
}

static GenResult qbe_operator(QBEUnaryExpr const &expr, QBEContext &ctx)
{
    auto const &value_type = expr.operand.node->bound_type->value_type();
    if (expr.op == Operator::Idempotent) {
        return expr.operand.value;
    }
    if (expr.op == Operator::AddressOf) {
        return std::visit(
            overloads {
                [&expr](Local const &local) -> ILValue {
                    if (local.type == LocalType::Reference) {
                        return expr.operand.value;
                    }
                    UNREACHABLE();
                    return ILValue::null();
                },
                [&expr](ILValue::Global const &) -> ILValue {
                    return expr.operand.value;
                },
                [&expr](ILValue::Variable const &variable) -> ILValue {
                    return ILValue::variable(variable.name, ILBaseType::L);
                },
                [](auto const &) -> ILValue {
                    UNREACHABLE();
                    return ILValue::null();
                }

            },
            expr.operand.value.inner);
        QBE_ASSERT(std::holds_alternative<Local>(expr.operand.value.inner), ctx);
        auto const &local = std::get<Local>(expr.operand.value.inner);
        QBE_ASSERT(local.type == LocalType::Reference, ctx);
        return expr.operand.value;
    }
    return std::visit(
        [&expr, &ctx](auto const &descr) {
            return qbe_operator(expr, descr, ctx);
        },
        value_type->description);
}

static void assign(ILValue const &lhs, ILValue const &rhs, QBEContext &ctx)
{
    std::visit(
        overloads {
            [&ctx, &lhs](ILValues const &values) {
                auto    lhs_ptr { lhs };
                ILValue prev_lhs;
                auto    prev_size { 0 };
                for (auto const &value : values) {
                    if (prev_size > 0) {
                        lhs_ptr = ILValue::local(++ctx.next_var, ILBaseType::L);
                        ctx.add_operation(
                            ExprDef {
                                prev_lhs,
                                ILValue::integer(alignat(prev_size, value.align), ILBaseType::L),
                                ILOperation::Add,
                                lhs_ptr,
                            });
                    }
                    assign(lhs_ptr, value, ctx);
                    prev_size = value.size;
                    prev_lhs = lhs_ptr;
                }
            },
            [&ctx, &lhs, &rhs](auto const &v) {
                ctx.add_operation(
                    StoreDef {
                        rhs,
                        lhs,
                    });
            } },
        rhs.inner);
}

static GenResult assign(QBEOperand const &lhs, ASTNode const &rhs, QBEContext &ctx)
{
    auto rhs_value { TRY_GENERATE(rhs, ctx) };
    std::visit(
        overloads {
            [&ctx, &lhs, &rhs_value](OptionalType const &lhs_opt, OptionalType const &rhs_opt) {
                assert(lhs_opt.type == rhs_opt.type);
                assign(lhs.value, rhs_value.value, ctx);
            },
            [&ctx, &lhs](OptionalType const &optional, VoidType const &) {
                auto flag_ptr = ILValue::local(++ctx.next_var, ILBaseType::L);
                ctx.add_operation(
                    ExprDef {
                        lhs.value,
                        ILValue::integer(optional.type->size_of(), ILBaseType::L),
                        ILOperation::Add,
                        flag_ptr,
                    });
                ctx.add_operation(
                    StoreDef {
                        ILValue::integer(0, ILBaseType::B),
                        flag_ptr,
                    });
            },
            [&ctx, &lhs, &rhs_value](OptionalType const &optional, auto const &) {
                assign(lhs.value, rhs_value.value, ctx);
                auto flag_ptr = ILValue::local(++ctx.next_var, ILBaseType::L);
                ctx.add_operation(
                    ExprDef {
                        lhs.value,
                        ILValue::integer(optional.type->size_of(), ILBaseType::L),
                        ILOperation::Add,
                        flag_ptr,
                    });
                ctx.add_operation(
                    StoreDef {
                        ILValue::integer(1, ILBaseType::B),
                        flag_ptr,
                    });
            },
            [&ctx, &lhs, &rhs_value](auto const &, auto const &) {
                assign(lhs.value, rhs_value.value, ctx);
            } },
        lhs.node->bound_type->description, rhs->bound_type->description);
    return lhs.value;
}

template<class Node>
GenResult generate_qbe_node(ASTNode const &, Node const &, QBEContext &ctx)
{
    NYI("Unimplemented QBE serialization for {}", typeid(Node).name());
}

template<>
GenResult generate_qbe_node(ASTNode const &n, BinaryExpression const &impl, QBEContext &ctx)
{
    auto const &rhs_type { impl.rhs->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    if (impl.op == Operator::Assign) {
        return assign(TRY_GENERATE(impl.lhs, ctx), impl.rhs, ctx);
    }
    QBEOperand lhs_operand { TRY_GENERATE(impl.lhs, ctx) };
    QBEOperand rhs_operand = { ILValue::null(), impl.rhs };
    if (impl.op != Operator::MemberAccess) {
        rhs_operand = { TRY_GENERATE(impl.rhs, ctx) };
    };
    QBEBinExpr expr { lhs_operand, impl.op, rhs_operand };
    auto       var = qbe_operator(expr, ctx);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Block const &impl, QBEContext &ctx)
{
    if (ctx.program.name.empty()) {
        ctx.program.name = std::format(L"anonymous-{}", n.id.value_or(0));
    }
    if (ctx.program.files.empty()) {
        auto &file = ctx.program.files.emplace_back(ctx.program.name);
        file.id = ctx.program.files.size() - 1;
        ctx.current_file = file.id;
        ctx.current_function = 0;
    }
    auto &file = ctx.program.files[ctx.current_file];
    if (ctx.current_function >= file.functions.size()) {
        auto &file { ctx.program.files[ctx.current_file] };
        auto &function = file.functions.emplace_back(
            ctx.current_file,
            file.name,
            ctx.qbe_type(n->bound_type),
            false);
        function.id = file.functions.size() - 1;
        ctx.is_export = false;
        ctx.current_function = function.id;
        ctx.next_var = 0;
        ctx.next_label = 0;
    }
    ctx.add_operation(
        LabelDef {
            ++ctx.next_label,
        });
    for (auto const &s : impl.statements) {
        if (auto res = generate_qbe_node(s, ctx); !res) {
            return res;
        }
    }
    return {};
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Call const &impl, QBEContext &ctx)
{
    auto def = get<FunctionDefinition>(impl.function);
    auto decl = get<FunctionDeclaration>(def.declaration);

    std::vector<ILValue> args;
    for (auto const &[arg, param] : std::ranges::views::zip(get<ExpressionList>(impl.arguments).expressions, decl.parameters)) {
        if (is<ReferenceType>(param->bound_type)) {
            QBE_ASSERT(is<ReferenceType>(arg->bound_type), ctx);
            args.emplace_back(TRY_GENERATE(arg, ctx).value);
        } else {
            args.emplace_back(dereference(TRY_GENERATE(arg, ctx), ctx));
        }
    }
    ILValue ret { ILValue::null() };
    if (n->bound_type != TypeRegistry::void_) {
        ret = ILValue::local(++ctx.next_var, ctx.qbe_type(n->bound_type));
    }
    auto name = std::visit(
        overloads {
            [](ExternLink const &link) -> std::wstring_view {
                return link.link_name;
            },
            [&def](auto const &) -> std::wstring_view {
                return def.name;
            } },
        def.implementation->node);
    ctx.add_operation(
        CallDef {
            std::wstring { name },
            ret,
            args,
        });
    return ret;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Comptime const &impl, QBEContext &ctx)
{
    return generate_qbe_node(impl.statements, ctx);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Constant const &impl, QBEContext &ctx)
{
    return std::visit(
        overloads {
            [](VoidType const &) -> ILValue {
                return ILValue::null();
            },
            [&ctx, &impl](BoolType const &) -> ILValue {
                return ILValue::integer(as<bool>(*impl.bound_value) ? 1 : 0, ILBaseType::W);
            },
            [&ctx, &impl](IntType const &int_type) -> ILValue {
                ILBaseType t;
                int64_t    v;
                switch (int_type.width_bits) {
                case 8:
                    t = (int_type.is_signed) ? ILBaseType::SB : ILBaseType::UB;
                    v = (int_type.is_signed) ? as<int8_t>(*impl.bound_value) : as<uint8_t>(*impl.bound_value);
                    break;
                case 16:
                    t = (int_type.is_signed) ? ILBaseType::SH : ILBaseType::UH;
                    v = (int_type.is_signed) ? as<int16_t>(*impl.bound_value) : as<uint16_t>(*impl.bound_value);
                    break;
                case 32:
                    t = (int_type.is_signed) ? ILBaseType::SW : ILBaseType::UW;
                    v = (int_type.is_signed) ? as<int32_t>(*impl.bound_value) : as<uint32_t>(*impl.bound_value);
                    break;
                case 64:
                    t = ILBaseType::L;
                    v = (int_type.is_signed) ? as<int64_t>(*impl.bound_value) : as<uint64_t>(*impl.bound_value);
                    break;
                }
                return ILValue::integer(v, t);
            },
            [&ctx, &impl](FloatType const &float_type) -> ILValue {
                ILBaseType t;
                double     dbl;
                switch (float_type.width_bits) {
                case 32:
                    t = ILBaseType::S;
                    dbl = as<float>(*impl.bound_value);
                    break;
                case 64:
                    t = ILBaseType::S;
                    dbl = as<float>(*impl.bound_value);
                    break;
                }
                return ILValue::float_val(dbl, t);
            },
            [&ctx, &impl](ZeroTerminatedArray const &zta) -> ILValue {
                assert(zta.array_of == TypeRegistry::u8);
                return ctx.add_cstring(static_cast<char const *>(as<void *>(*impl.bound_value)));
            },
            [&ctx, &impl](SliceType const &slice_type) -> ILValue {
                int var = ++ctx.next_var;
                assert(slice_type.slice_of == TypeRegistry::u32);
                auto slice = as<Slice>(*impl.bound_value);
                auto len_id = ++ctx.next_var;
                ctx.add_operation(
                    AllocDef {
                        16, 16,
                        ILValue::local(var, ILBaseType::L) });
                ctx.add_operation(
                    StoreDef {
                        ctx.add_string(
                            std::wstring_view {
                                static_cast<wchar_t *>(slice.ptr),
                                static_cast<size_t>(slice.size),
                            }),
                        ILValue::local(var, ILBaseType::L),
                    });
                ctx.add_operation(
                    ExprDef {
                        ILValue::local(var, ILBaseType::L),
                        ILValue::integer(8, ILBaseType::L),
                        ILOperation::Add,
                        ILValue::local(len_id, ILBaseType::L),
                    });
                ctx.add_operation(
                    StoreDef {
                        ILValue::integer(slice.size, ILBaseType::L),
                        ILValue::local(len_id, ILBaseType::L),
                    });
                return ILValue::local(var, ctx.qbe_type(impl.bound_value->type));
            },
            [](auto const &descr) -> ILValue {
                UNREACHABLE();
            } },
        impl.bound_value->type->description);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Dummy const &impl, QBEContext &ctx)
{
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, ExpressionList const &impl, QBEContext &ctx)
{
    ILValues values;
    int      align = 0;
    int      size = 0;
    for (auto const &expr : impl.expressions) {
        values.emplace_back(dereference(TRY_GENERATE(expr, ctx), ctx));
        if (size > 0) {
            size = alignat(size, values.back().align);
        }
        size += values.back().size;
        align = std::max(align, values.back().align);
    }
    return ILValue::sequence(values, align, size);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDefinition const &impl, QBEContext &ctx)
{
    if (!is<ExternLink>(impl.implementation)) {
        if (auto res = generate_qbe_node(impl.declaration, ctx); !res) {
            return res;
        }
        TRY_GENERATE(impl.implementation, ctx);
    }
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDeclaration const &impl, QBEContext &ctx)
{
    auto &file { ctx.program.files[ctx.current_file] };
    auto &function = file.functions.emplace_back(
        ctx.current_file,
        impl.name,
        ctx.qbe_type(impl.return_type->bound_type),
        ctx.is_export);
    function.id = file.functions.size() - 1;
    ctx.is_export = false;
    ctx.current_function = function.id;
    file.has_exports |= function.exported;
    file.has_main = impl.name == L"main";
    auto first = true;
    for (auto const &param : impl.parameters) {
        auto p = get<Parameter>(param);
        function.parameters.emplace_back(p.name, ctx.qbe_type(param->bound_type));
    }
    ctx.next_var = 0;
    ctx.next_label = 0;
    auto _ = generate_qbe_nodes(impl.parameters, ctx);
    return ILValue::null();
}

std::wostream &operator<<(std::wostream &os, ILFunction const &function)
{
    if (function.exported) {
        os << "export ";
    }
    os << "function " << function.return_type << " $" << function.name << '(';
    auto first = true;
    for (auto const &param : function.parameters) {
        if (!first) {
            os << ", ";
        }
        first = false;
        std::visit(
            overloads {
                [&os](ILBaseType const &bt) {
                    os << bt;
                },
                [&os](std::wstring const &t) {
                    os << t;
                } },
            param.type);
        os << " %" << param.name << "$$";
    }
    os << R"() {
@start
)";
    for (auto const &instruction : function.instructions) {
        os << instruction;
    }
    os << R"(}

)";
    return os;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Identifier const &impl, QBEContext &ctx)
{
    auto  t = ctx.qbe_type(n->bound_type);
    auto &file = ctx.program.files[ctx.current_file];
    auto &function = file.functions[ctx.current_function];
    for (auto const &param : function.parameters) {
        if (param.name == impl.identifier) {
            return ILValue::parameter(param.name, t);
        }
    }
    return ILValue::variable(impl.identifier, t);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, IfStatement const &impl, QBEContext &ctx)
{
    auto if_block = ++ctx.next_label;
    auto after_if = ++ctx.next_label;
    int  else_block = 0;
    auto cond_false = after_if;
    if (impl.else_branch != nullptr) {
        else_block = ++ctx.next_label;
        cond_false = else_block;
    }
    auto condition_var = TRY_GENERATE(impl.condition, ctx);
    ctx.add_operation(
        JnzDef {
            condition_var.value,
            if_block,
            cond_false,
        });
    ctx.add_operation(LabelDef { if_block });
    TRY_GENERATE(impl.if_branch, ctx);
    if (impl.else_branch != nullptr) {
        ctx.add_operation(JmpDef { after_if });
        ctx.add_operation(LabelDef { else_block });
        TRY_GENERATE(impl.else_branch, ctx);
    }
    ctx.add_operation(LabelDef { after_if });
    return ILValue::null();
}

bool flatten_type(pType const &type, bool first, std::wostream &os);

bool flatten_type(BoolType const &, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << 'w';
    return false;
}

bool flatten_type(FloatType const &flt_type, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << qbe_type_code(flt_type);
    return false;
}

bool flatten_type(IntType const &int_type, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << qbe_type_code(int_type);
    return false;
}

bool flatten_type(PointerType const &strukt, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << 'l';
    return false;
}

bool flatten_type(SliceType const &, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << "l, l";
    return false;
}

bool flatten_type(StructType const &strukt, bool first, std::wostream &os)
{
    for (auto const &field : strukt.fields) {
        first = flatten_type(field.type, first, os);
    }
    return first;
}

bool flatten_type(auto const &descr, bool first, std::wostream &os)
{
    NYI("flatten_type for {}", typeid(descr).name());
}

bool flatten_type(pType const &type, bool first, std::wostream &os)
{
    return std::visit(
        [&os, &first](auto const &descr) {
            return flatten_type(descr, first, os);
        },
        type->description);
}

std::wostream &operator<<(std::wostream &os, ILFile const &file)
{
    for (auto const type : file.types) {
        std::visit(
            overloads {
                [&os](SliceType const &) {
                    os << "type :slice_t = { l, l }\n";
                },
                [&os, &type](StructType const &strukt) {
                    os << "type :struct" << *(type.id) << " = { ";
                    flatten_type(strukt, true, os);
                    os << " }\n";
                },
                [](auto const &) {
                    UNREACHABLE();
                } },
            type->description);
    }
    if (!file.types.empty()) {
        os << '\n';
    }
    for (auto const &function : file.functions) {
        os << function;
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.strings)) {
        os << "data $str_" << ix + 1 << " = { ";
        for (auto ch : s) {
            os << std::format(L"w {:d}, ", ch);
        }
        os << "w 0 }\n";
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.cstrings)) {
        os << "data $cstr_" << ix + 1 << " = { ";
        for (auto ch : s) {
            os << std::format(L"b {:d}, ", ch);
        }
        os << L"b 0 }\n";
    }
    return os;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Module const &impl, QBEContext &ctx)
{
    auto &file = ctx.program.files.emplace_back(impl.name);
    file.id = ctx.program.files.size() - 1;
    ctx.current_file = file.id;
    ctx.current_function = std::numeric_limits<size_t>::max();
    if (auto res = generate_qbe_nodes(impl.statements, ctx); !res) {
        return res;
    }
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Parameter const &impl, QBEContext &ctx)
{
    std::visit(
        overloads {
            [&ctx, &impl, &n](IntType const &int_type) {
                ctx.add_operation(CopyDef {
                    ILValue::parameter(impl.name, QBEType::qbe_type(int_type, n->bound_type)),
                    ILValue::variable(impl.name, QBEType::qbe_type(int_type, n->bound_type)),
                });
            },
            [&ctx, &impl, &n](FloatType const &flt_type) {
                ctx.add_operation(CopyDef {
                    ILValue::parameter(impl.name, QBEType::qbe_type(flt_type, n->bound_type)),
                    ILValue::variable(impl.name, QBEType::qbe_type(flt_type, n->bound_type)),
                });
            },
            [&ctx, &impl](auto const &) {
                ctx.add_operation(CopyDef {
                    ILValue::parameter(impl.name, ILBaseType::L),
                    ILValue::variable(impl.name, ILBaseType::L),
                });
            },
        },
        n->bound_type->description);
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Program const &impl, QBEContext &ctx)
{
    ctx.program.name = impl.name;
    for (auto const &[mod_name, mod] : impl.modules) {
        if (auto res = generate_qbe_node(mod, ctx); !res) {
            return res;
        }
    }
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, PublicDeclaration const &impl, QBEContext &ctx)
{
    ctx.is_export = true;
    auto ret = generate_qbe_node(impl.declaration, ctx);
    ctx.is_export = false;
    return ret;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Return const &impl, QBEContext &ctx)
{
    RetDef ret {};
    if (impl.expression != nullptr) {
        ret.expr = TRY_GENERATE(impl.expression, ctx).value;
    }
    ctx.add_operation(ret);
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Struct const &impl, QBEContext &ctx)
{
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, VariableDeclaration const &impl, QBEContext &ctx)
{
    size_t size = n->bound_type->size_of();
    auto   var_ref = ILValue::variable(impl.name, ILBaseType::L);
    ctx.add_operation(
        AllocDef {
            (size < 8) ? 4u : 8u,
            size,
            var_ref,
        });
    if (impl.initializer != nullptr) {
        return assign({ var_ref, n }, impl.initializer, ctx);
    }
    return var_ref;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, UnaryExpression const &impl, QBEContext &ctx)
{
    auto const &rhs_type { impl.operand->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    QBEOperand   operand { TRY_GENERATE(impl.operand, ctx) };
    QBEUnaryExpr expr { impl.op, operand };
    auto         var = qbe_operator(expr, ctx);
    return var;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, WhileStatement const &impl, QBEContext &ctx)
{
    auto top_loop = ++ctx.next_label;
    auto cont_loop = ++ctx.next_label;
    auto end_loop = ++ctx.next_label;
    ctx.add_operation(LabelDef { top_loop });
    auto condition_var = TRY_GENERATE(impl.condition, ctx);
    ctx.add_operation(
        JnzDef {
            condition_var.value,
            cont_loop,
            end_loop,
        });
    ctx.add_operation(LabelDef { cont_loop });
    TRY_GENERATE(impl.statement, ctx);
    ctx.add_operation(JmpDef { top_loop });
    ctx.add_operation(LabelDef { end_loop });
    return ILValue::null();
}

GenResult generate_qbe_node(ASTNode const &n, QBEContext &ctx)
{
    trace("Generating {}", SyntaxNodeType_name(n->type()));
    return std::visit(
        [&n, &ctx](auto const &impl) -> GenResult {
            return generate_qbe_node(n, impl, ctx);
        },
        n->node);
}

std::expected<ILProgram, std::wstring> generate_qbe(ASTNode const &node)
{
    QBEContext ctx {};
    if (auto res = generate_qbe_node(node, ctx); res.has_value()) {
        return ctx.program;
    } else {
        return std::unexpected(res.error());
    }
}

std::expected<void, std::wstring> compile_qbe(ILProgram const &program)
{
    fs::path dot_lia { ".lia" };
    fs::create_directory(dot_lia);
    std::vector<fs::path> o_files;

    for (auto const &file : program.files) {
        if (file.has_exports) {
            fs::path file_path = dot_lia / file.name;
            file_path.replace_extension("ssa");
            {
                fs::path file_path = dot_lia / file.name;
                file_path.replace_extension("ssa");
                std::wofstream os { file_path.string() };
                os << file;
            }
            auto s_file { file_path };
            s_file.replace_extension("s");
            auto o_file { file_path };
            o_file.replace_extension("o");

            info("[QBE] Compiling `{}`", file_path.string());
            Util::Process qbe { "qbe", "-o", s_file.string(), file_path.string() };
            if (auto res = qbe.execute(); !res.has_value()) {
                return std::unexpected(std::format(L"qbe execution failed: {}", as_wstring(res.error().description)));
            } else if (res.value() != 0) {
                return std::unexpected(std::format(L"qbe failed: {}", as_wstring(qbe.stderr())));
            }
            Util::Process as { "as", "-o", o_file.string(), s_file.string() };
            if (auto res = as.execute(); !res.has_value()) {
                return std::unexpected(std::format(L"as execution failed: {}", as_wstring(res.error().description)));
            } else if (res.value() != 0) {
                return std::unexpected(std::format(L"as failed: {}", as_wstring(as.stderr())));
            }
            if (!has_option("keep-assembly")) {
                fs::remove(file_path);
            }
            o_files.push_back(o_file);
        }
    }

    if (!o_files.empty()) {
        fs::path program_path { as_utf8(program.name) };
        program_path.replace_extension();
        info("[QBE] Linking `{}`", program_path.string());

        std::vector<std::string> ld_args {
            "-o",
            fs::path { as_utf8(program.name) }.replace_extension("").string(),
            // "-no-pie",
            std::format("-L{}/lib", Lia::lia_dir().string()),
            "-lliart", "-lm", "-lpthread", "-ldl"
        };
        if (has_option("L")) {
            for (auto const &lib_path : get_option_values("L")) {
                ld_args.push_back(std::format("-L{}", lib_path));
            }
        }
        if (has_option("l")) {
            for (auto const &lib : get_option_values("l")) {
                ld_args.push_back(std::format("-l{}", lib));
            }
        }
        for (auto const &o : o_files) {
            ld_args.push_back(o.string());
        }

        Util::Process link { "cc", ld_args };
        if (auto res = link.execute(); !res.has_value()) {
            return std::unexpected(std::format(L"Linker execution failed: {}", as_wstring(res.error().description)));
        } else if (res.value() != 0) {
            return std::unexpected(std::format(L"Linking failed: {}", as_wstring(link.stderr())));
        }
        if (!has_option("keep-objects")) {
            for (auto const &o : o_files) {
                fs::remove(o);
            }
        }
    }
    return {};
}
}
