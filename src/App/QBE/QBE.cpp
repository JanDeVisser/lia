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
#include <sstream>
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

ILValue QBEContext::add_string(std::wstring_view s)
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

ILValue QBEContext::add_cstring(std::string_view s)
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

ILType QBEContext::qbe_type(pType const &type)
{
    auto ret = Lia::QBE::qbe_type(type);
    if (std::holds_alternative<ILStructType>(ret)) {
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

void QBEContext::add_operation(ILInstructionImpl impl)
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
        trace(L"QBE {}", s.str());
    }
    function.instructions.emplace_back(std::move(impl));
}

std::optional<ILBinding> QBEContext::find(std::wstring_view name)
{
    return function().find(name);
}

ILBinding const &QBEContext::add(std::wstring_view name, pType const &type)
{
    return function().add(name, type);
}

ILBinding const &QBEContext::add_parameter(std::wstring_view name, pType const &type)
{
    return function().add_parameter(name, type);
}

void QBEContext::push()
{
    function().push();
}

void QBEContext::pop()
{
    function().pop();
}

ILFunction &QBEContext::function()
{
    return program.files[current_file].functions[current_function];
}

using QBEContexts = std::vector<QBEContext>;

GenResult generate_qbe_node(ASTNode const &, QBEContext &);
GenResult generate_qbe_nodes(ASTNodes const &, QBEContext &);

#define TRY_GENERATE(n, ctx)                                \
    (                                                       \
        {                                                   \
            ASTNode    __n = (n);                           \
            QBEOperand __op { n, __n->bound_type };         \
            if (auto __res = __op.get_value(ctx); !__res) { \
                return std::unexpected(__res.error());      \
            }                                               \
            (__op);                                         \
        })

#define TRY_ENSURE_GENERATED(op, ctx)                       \
    (                                                       \
        {                                                   \
            QBEOperand __op { op };                         \
            if (auto __res = __op.get_value(ctx); !__res) { \
                return std::unexpected(__res.error());      \
            }                                               \
            (__op);                                         \
        })

GenResult QBEOperand::get_value(QBEContext &ctx)
{
    if (!value) {
        if (auto res = generate_qbe_node(node, ctx); !res) {
            return res;
        } else {
            *this = res.value();
        }
    }
    return *this;
}

GenResult QBEOperand::dereference(QBEContext &ctx) const
{
    auto ret_op { *this };
    if (is<ReferenceType>(ptype)) {
        assert(value.has_value());
        ret_op.ptype = get<ReferenceType>(ptype).referencing;
        if (qbe_first_class_type(ret_op.ptype)) {
            ret_op.set_value(ILValue::local(++ctx.next_var, ctx.qbe_type(ret_op.ptype)));
            ctx.add_operation(
                LoadDef {
                    .pointer = get_value(),
                    .target = ret_op.get_value(),
                });
        }
        return ret_op;
    }
    //    if (is<OptionalType>(ptype)) {
    //        return QBEOperand {
    //            node,
    //            TypeRegistry::the().referencing(get<OptionalType>(ptype).type),
    //            get_value(),
    //        }
    //            .dereference(ctx);
    //   }
    std::visit(
        overloads {
            [&ctx, &ret_op, this](ILValue::Variable const &variable) {
                if (qbe_first_class_type(ret_op.ptype)) {
                    ret_op.set_value(ILValue::local(++ctx.next_var, ctx.qbe_type(ret_op.ptype)));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = get_value(),
                            .target = ret_op.get_value(),
                        });
                }
            },
            [&ctx, &ret_op, this](ILValue::Parameter const &param) {
                if (qbe_first_class_type(ret_op.ptype)) {
                    ret_op.set_value(ILValue::local(++ctx.next_var, ctx.qbe_type(ret_op.ptype)));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = get_value(),
                            .target = ret_op.get_value(),
                        });
                }
            },
            [&ret_op](auto const &) {
            } },
        get_value().inner);
    return ret_op;
}

GenResult generate_qbe_nodes(ASTNodes const &nodes, QBEContext &ctx)
{
    QBEOperand operand;
    for (auto const &n : nodes) {
        operand = TRY_GENERATE(n, ctx);
    }
    return operand;
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

#define TRY_GETVALUE(op, ctx)                             \
    (                                                     \
        {                                                 \
            QBEOperand __op;                              \
            if (auto __res = op.get_value(ctx); !__res) { \
                return __res;                             \
            } else {                                      \
                __op = __res.value();                     \
            }                                             \
            (__op);                                       \
        })

static void raw_assign(ILValue const &lhs, ILValue const &rhs, pType const &type, QBEContext &ctx);

template<class T>
void raw_assign(ILValue const &lhs, ILValue const &rhs, T const &descr, QBEContext &ctx)
{
    ctx.add_operation(
        StoreDef {
            rhs,
            lhs,
        });
}

template<>
void raw_assign(ILValue const &lhs, ILValue const &rhs, std::monostate const &, QBEContext &ctx)
{
}

template<>
void raw_assign(ILValue const &lhs, ILValue const &rhs, StructType const &strukt, QBEContext &ctx)
{
    std::visit(
        overloads {
            [&ctx, &lhs, &strukt](ILValues const &values) {
                auto    lhs_ptr { lhs };
                ILValue prev_lhs;
                auto    prev_size { 0 };
                for (auto const &[ix, value] : std::ranges::views::enumerate(values)) {
                    auto const &fld { strukt.fields[ix] };
                    if (prev_size > 0) {
                        lhs_ptr = ILValue::pointer(++ctx.next_var);
                        ctx.add_operation(
                            ExprDef {
                                prev_lhs,
                                ILValue::integer(alignat(prev_size, align_of(value.type)), ILBaseType::L),
                                ILOperation::Add,
                                lhs_ptr,
                            });
                    }
                    raw_assign(lhs_ptr, value, fld.type, ctx);
                    prev_size = size_of(value.type);
                    prev_lhs = lhs_ptr;
                }
            },
            [&ctx, &lhs, &rhs, &strukt](auto const &pointer) {
                ctx.add_operation(
                    BlitDef {
                        rhs,
                        lhs,
                        static_cast<intptr_t>(strukt.size_of()),
                    });
            } },
        rhs.inner);
}

template<>
void raw_assign(ILValue const &lhs, ILValue const &rhs, OptionalType const &optional, QBEContext &ctx)
{
    ctx.add_operation(
        BlitDef {
            rhs,
            lhs,
            static_cast<intptr_t>(optional.size_of()),
        });
}

template<>
void raw_assign(ILValue const &lhs, ILValue const &rhs, SliceType const &slice, QBEContext &ctx)
{
    ctx.add_operation(
        BlitDef {
            rhs,
            lhs,
            static_cast<intptr_t>(slice.size_of()),
        });
}

static void raw_assign(ILValue const &lhs, ILValue const &rhs, pType const &type, QBEContext &ctx)
{
    std::visit(
        [&ctx, &lhs, &rhs](auto const &descr) {
            raw_assign(lhs, rhs, descr, ctx);
        },
        type->description);
}

GenResult assign(QBEOperand const &lhs, ASTNode const &rhs, QBEContext &ctx)
{
    auto rhs_value = TRY_GENERATE(rhs, ctx);
    rhs_value = TRY_DEREFERENCE(rhs_value, ctx);
    std::visit(
        overloads {
            [&ctx, &lhs, &rhs_value](OptionalType const &lhs_opt, OptionalType const &rhs_opt) {
                assert(lhs_opt.type == rhs_opt.type);
                raw_assign(lhs.get_value(), rhs_value.get_value(), lhs.ptype, ctx);
            },
            [&ctx, &lhs](OptionalType const &optional, VoidType const &) {
                auto flag_ptr = ILValue::pointer(++ctx.next_var);
                ctx.add_operation(
                    ExprDef {
                        lhs.get_value(),
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
                raw_assign(lhs.get_value(), rhs_value.get_value(), optional.type, ctx);
                auto flag_ptr = ILValue::pointer(++ctx.next_var);
                ctx.add_operation(
                    ExprDef {
                        lhs.get_value(),
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
                raw_assign(lhs.get_value(), rhs_value.get_value(), lhs.ptype, ctx);
            } },
        lhs.ptype->description, rhs->bound_type->description);
    return lhs;
}

GenResult cast(QBEOperand value, pType const &target_type, QBEContext &ctx)
{
    if (value.ptype == target_type) {
        return value;
    }
    QBEOperand ret_op { value };
    ret_op.ptype = target_type;
    return std::visit(
        overloads {
            [&value, &ctx, &target_type](ReferenceType const &ref, auto const &rhs_descr) -> GenResult {
                return cast(TRY_DEREFERENCE(value, ctx), target_type, ctx);
            },
            [&ctx, &value, &ret_op](OptionalType const &optional, BoolType const &) -> GenResult {
                auto flag_ptr = ILValue::pointer(++ctx.next_var);
                auto ret_value = ILValue::local(++ctx.next_var, ILBaseType::SB);
                ctx.add_operation(
                    ExprDef {
                        value.get_value(),
                        ILValue::integer(optional.type->size_of(), ILBaseType::L),
                        ILOperation::Add,
                        flag_ptr,
                    });
                ctx.add_operation(
                    LoadDef {
                        flag_ptr,
                        ret_value,
                    });
                ret_op.set_value(ret_value);
                return ret_op;
            },
            [&ctx, &value, &ret_op](IntType const &int_type, BoolType const &) -> GenResult {
                auto ret_value = ILValue::local(++ctx.next_var, ILBaseType::SB);
                ctx.add_operation(
                    ExprDef {
                        value.get_value(),
                        ILValue::integer(0, qbe_type_code(value.ptype)),
                        ILOperation::Equals,
                        ret_value,
                    });
                ret_op.set_value(ret_value);
                return ret_op;
            },
            [&ctx, &value, &ret_op](IntType const &lhs_type, IntType const &rhs_type) -> GenResult {
                auto ret_value = ILValue::local(++ctx.next_var, ILBaseType::SB);
                ctx.add_operation(
                    ExprDef {
                        value.get_value(),
                        ILValue::integer(0, qbe_type_code(value.ptype)),
                        ILOperation::Equals,
                        ret_value,
                    });
                ret_op.set_value(ret_value);
                return ret_op;
            },
            [](auto const &, auto const &) -> GenResult {
                return std::unexpected(L"Invalid cast in QBE");
            } },
        value.ptype->description, target_type->description);
}

template<class Node>
GenResult generate_qbe_node(ASTNode const &, Node const &, QBEContext &ctx)
{
    NYI("Unimplemented QBE serialization for {}", typeid(Node).name());
}

using GenBinExpr = std::expected<QBEBinExpr, std::wstring>;

static GenBinExpr make_binexpr(ASTNode const &n, QBEOperand lhs, Operator op, QBEOperand rhs, QBEContext &ctx)
{
    pType const &lhs_type = lhs.ptype;
    pType const &rhs_type = rhs.ptype;
    if (is<ReferenceType>(lhs_type) || is<ReferenceType>(rhs_type)) {
        return std::visit(
            overloads {
                [&n, &lhs, &op, &rhs, &ctx](ReferenceType const &, auto const &) -> GenBinExpr {
                    lhs = TRY_ENSURE_GENERATED(lhs, ctx);
                    lhs = TRY_DEREFERENCE(lhs, ctx);
                    return make_binexpr(n, lhs, op, rhs, ctx);
                },
                [&n, &lhs, &op, &rhs, &ctx](auto const &, ReferenceType const &) -> GenBinExpr {
                    rhs = TRY_ENSURE_GENERATED(rhs, ctx);
                    rhs = TRY_DEREFERENCE(rhs, ctx);
                    return make_binexpr(n, lhs, op, rhs, ctx);
                },
                [&n, &lhs, &op, &rhs, &ctx](ReferenceType const &, ReferenceType const &) -> GenBinExpr {
                    lhs = TRY_ENSURE_GENERATED(lhs, ctx);
                    lhs = TRY_DEREFERENCE(lhs, ctx);
                    rhs = TRY_ENSURE_GENERATED(rhs, ctx);
                    rhs = TRY_DEREFERENCE(rhs, ctx);
                    return make_binexpr(n, lhs, op, rhs, ctx);
                },
                [](auto const &, auto const &) -> GenBinExpr {
                    UNREACHABLE();
                }

            },
            lhs_type->description, rhs_type->description);
    }
    return std::visit(
        overloads {
            [&n, &lhs, &op, &rhs, &ctx](StructType const &strukt, auto const &) -> GenBinExpr {
                if (op == Operator::MemberAccess) {
                    lhs = TRY_ENSURE_GENERATED(lhs, ctx);
                    auto const &member = get<Identifier>(rhs.node).identifier;
                    rhs.set_value(ILValue::integer(strukt.offset_of(member), ILBaseType::L));
                }
                return QBEBinExpr { n, lhs, op, rhs };
            },
            [&n, &lhs, &op, &rhs, &ctx](TypeType const &meta_type, auto const &) -> GenBinExpr {
                auto type = meta_type.type;
                if (type->kind() == TypeKind::EnumType && op == Operator::MemberAccess) {
                    lhs.set_value(ILValue::null());
                    auto        enoom = get<EnumType>(type);
                    auto const &label = get<Identifier>(rhs.node).identifier;
                    for (auto const &v : enoom.values) {
                        if (v.label == label) {
                            rhs.set_value(ILValue::integer(v.value, qbe_type(enoom.underlying_type)));
                            break;
                        }
                    }
                }
                return QBEBinExpr { n, lhs, op, rhs };
            },
            [&n, &lhs, &op, &rhs, &ctx](IntType const &lhs_int, TypeType const &cast_to) -> GenBinExpr {
                rhs.set_value(ILValue::local(++ctx.next_var, qbe_type(cast_to.type)));
                return QBEBinExpr { n, TRY_ENSURE_GENERATED(lhs, ctx), op, rhs };
            },
            [&n, &lhs, &op, &rhs, &ctx](EnumType const &lhs_int, TypeType const &cast_to) -> GenBinExpr {
                rhs.set_value(ILValue::local(++ctx.next_var, qbe_type(cast_to.type)));
                return QBEBinExpr { n, TRY_ENSURE_GENERATED(lhs, ctx), op, rhs };
            },
            [&n, &lhs, &op, &rhs, &ctx](auto const &, auto const &) -> GenBinExpr {
                return QBEBinExpr { n, TRY_ENSURE_GENERATED(lhs, ctx), op, TRY_ENSURE_GENERATED(rhs, ctx) };
            } },
        lhs_type->description, rhs_type->description);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, BinaryExpression const &impl, QBEContext &ctx)
{
    auto const &rhs_type { impl.rhs->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    trace(L"Generating BinExpr {} {} {}", impl.lhs->bound_type->to_string(), as_wstring(Operator_name(impl.op)), impl.rhs->bound_type->to_string());
    if (impl.op == Operator::Assign) {
        return assign(TRY_GENERATE(impl.lhs, ctx), impl.rhs, ctx);
    }
    QBEOperand lhs_operand { impl.lhs };
    QBEOperand rhs_operand { impl.rhs };
    auto       res = make_binexpr(n, lhs_operand, impl.op, rhs_operand, ctx);
    if (!res) {
        return std::unexpected(res.error());
    }
    trace(L"binexpr {} {} {}", res.value().lhs.get_value(), as_wstring(Operator_name(res.value().op)), res.value().rhs.get_value());
    return qbe_operator(res.value(), ctx);
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
            n->bound_type,
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

    auto &function = file.functions[ctx.current_function];
    function.push();
    for (auto const &s : impl.statements) {
        if (auto res = generate_qbe_node(s, ctx); !res) {
            return res;
        }
    }
    function.pop();
    return {};
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Call const &impl, QBEContext &ctx)
{
    auto def = get<FunctionDefinition>(impl.function);
    auto decl = get<FunctionDeclaration>(def.declaration);
    auto name = std::visit(
        overloads {
            [](ExternLink const &link) -> std::wstring_view {
                return link.link_name;
            },
            [&def](auto const &) -> std::wstring_view {
                return def.name;
            } },
        def.implementation->node);
    ILValue              ret_val { ILValue::null() };
    ILValue              ret_alloc { ILValue::null() };
    size_t               alloc_sz { 0 };
    std::vector<ILValue> args;

    trace(L"Generating call to `{}`", impl.callable);
    if (n->bound_type != TypeRegistry::void_) {
        alloc_sz = std::visit(
            overloads {
                [](BoolType const &) -> size_t {
                    return 0;
                },
                [](IntType const &) -> size_t {
                    return 0;
                },
                [](FloatType const &) -> size_t {
                    return 0;
                },
                [&n](auto const &) -> size_t {
                    return n->bound_type->size_of();
                } },
            n->bound_type->description);
        ret_val = ILValue::local(++ctx.next_var, ctx.qbe_type(n->bound_type));
    }

    if (alloc_sz > 0) {
        ret_alloc = ILValue::local(++ctx.next_var, ILBaseType::L);
        ctx.add_operation(AllocDef {
            (alloc_sz < 8) ? 4u : 8u,
            alloc_sz,
            ret_alloc,
        });
    }

    for (auto const &[arg, param] : std::ranges::views::zip(get<ExpressionList>(impl.arguments).expressions, decl.parameters)) {
        trace(L"  Argument `{}`: {}", get<Parameter>(param).name, arg->bound_type->to_string());
        if (is<ReferenceType>(param->bound_type)) {
            QBE_ASSERT(is<ReferenceType>(arg->bound_type), ctx);
            auto value = TRY_GENERATE(arg, ctx).get_value();
            value.type = ILBaseType::L;
            args.emplace_back(value);
        } else {
            auto operand { TRY_GENERATE(arg, ctx) };
            args.emplace_back(TRY_DEREFERENCE(operand, ctx).get_value());
        }
    }

    ctx.add_operation(
        CallDef {
            std::wstring { name },
            ret_val,
            args,
        });
    if (alloc_sz == 0) {
        return QBEOperand { n, ret_val };
    }
    if (alloc_sz <= 8) {
        ctx.add_operation(StoreDef {
            ret_val,
            ret_alloc,
        });
    } else {
        ctx.add_operation(BlitDef {
            .src = ret_val,
            .dest = ret_alloc,
            .bytes = static_cast<intptr_t>(alloc_sz),
        });
    }
    return QBEOperand { n, ret_alloc };
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
            [&n](VoidType const &) -> GenResult {
                return QBEOperand { n, ILValue::null() };
            },
            [&n, &ctx, &impl](BoolType const &) -> GenResult {
                return QBEOperand { n, ILValue::integer(as<bool>(*impl.bound_value) ? 1 : 0, ILBaseType::W) };
            },
            [&n, &ctx, &impl](IntType const &int_type) -> GenResult {
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
                return QBEOperand { n, ILValue::integer(v, t) };
            },
            [&n, &ctx, &impl](FloatType const &float_type) -> GenResult {
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
                return QBEOperand { n, ILValue::float_val(dbl, t) };
            },
            [&n, &ctx, &impl](ZeroTerminatedArray const &zta) -> GenResult {
                assert(zta.array_of == TypeRegistry::u8);
                return QBEOperand { n, ctx.add_cstring(static_cast<char const *>(as<void const *>(*impl.bound_value))) };
            },
            [&n, &ctx, &impl](SliceType const &slice_type) -> GenResult {
                int var = ++ctx.next_var;
                assert(slice_type.slice_of == TypeRegistry::u32);
                auto slice = as<Slice>(*impl.bound_value);
                auto len_id = ++ctx.next_var;
                auto ret = ILValue::local(var, ctx.qbe_type(TypeRegistry::string));
                ctx.add_operation(
                    AllocDef {
                        16,
                        16,
                        ret,
                    });
                ctx.add_operation(
                    StoreDef {
                        ctx.add_string(
                            std::wstring_view {
                                static_cast<wchar_t *>(slice.ptr),
                                static_cast<size_t>(slice.size),
                            }),
                        ret,
                    });
                ctx.add_operation(
                    ExprDef {
                        ret,
                        ILValue::integer(8, ILBaseType::L),
                        ILOperation::Add,
                        ILValue::pointer(len_id),
                    });
                ctx.add_operation(
                    StoreDef {
                        ILValue::integer(slice.size, ILBaseType::L),
                        ILValue::pointer(len_id),
                    });
                return QBEOperand { n, ret };
            },
            [](auto const &descr) -> GenResult {
                UNREACHABLE();
            } },
        impl.bound_value->type->description);
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Dummy const &impl, QBEContext &ctx)
{
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Enum const &impl, QBEContext &ctx)
{
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, ExpressionList const &impl, QBEContext &ctx)
{
    ILValues values;
    int      align = 0;
    int      size = 0;
    for (auto const &expr : impl.expressions) {
        QBEOperand operand { TRY_GENERATE(expr, ctx) };
        values.emplace_back(TRY_DEREFERENCE(operand, ctx).get_value());
        auto const &type = values.back().type;
        if (size > 0) {
            size = alignat(size, align_of(type));
        }
        size += size_of(type);
        align = std::max(align, align_of(type));
    }
    return QBEOperand { n, ILValue::sequence(values, align, size) };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDefinition const &impl, QBEContext &ctx)
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
    if (!is<ExternLink>(impl.implementation)) {
        TRY_GENERATE(impl.declaration, ctx);
        TRY_GENERATE(impl.implementation, ctx);
    }
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, FunctionDeclaration const &impl, QBEContext &ctx)
{
    trace(L"Generating function `{}` -> {}", impl.name, get<TypeType>(impl.return_type->bound_type).type->to_string());
    auto &file { ctx.program.files[ctx.current_file] };
    auto &function = file.functions.emplace_back(
        ctx.current_file,
        impl.name,
        get<TypeType>(impl.return_type->bound_type).type,
        ctx.is_export);
    if (auto sz = function.return_type->size_of(); sz > 8) {
        function.ret_allocation = sz;
    }
    function.id = file.functions.size() - 1;
    ctx.is_export = false;
    ctx.current_function = function.id;
    file.has_exports |= function.exported;
    file.has_main = impl.name == L"main";
    function.push();
    ctx.next_var = 0;
    ctx.next_label = 0;
    if (function.ret_allocation > 0) {
        ctx.add_operation(
            AllocDef {
                8,
                static_cast<size_t>(function.ret_allocation),
                ILValue::return_value(ILBaseType::L),
            });
    }
    auto _ = generate_qbe_nodes(impl.parameters, ctx);
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Identifier const &impl, QBEContext &ctx)
{
    auto t = ctx.qbe_type(n->bound_type);
    if (auto binding = ctx.find(impl.identifier); binding) {
        auto b = *binding;
        if (b.depth == 0) {
            return QBEOperand { n, ILValue::parameter(b.index, t) };
        }
        return QBEOperand { n, ILValue::variable(b.depth, b.index, t) };
    }
    fatal(L"Could not find variable `{}`", impl.identifier);
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
    if (impl.condition->bound_type != TypeRegistry::boolean) {
        if (auto res = cast(condition_var, TypeRegistry::boolean, ctx); !res.has_value()) {
            return res;
        } else {
            condition_var = res.value();
        }
    }
    ctx.add_operation(
        JnzDef {
            condition_var.get_value(),
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
    return QBEOperand { n, ILValue::null() };
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
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Parameter const &impl, QBEContext &ctx)
{
    auto const &param_binding = ctx.add_parameter(impl.name, n->bound_type);
    if (is<ReferenceType>(n->bound_type)) {
        return QBEOperand { n, ILValue::null() };
    }
    auto const &binding = ctx.add(impl.name, n->bound_type);
    size_t      sz = n->bound_type->size_of();
    auto        align = n->bound_type->align_of();
    auto        param = ILValue::parameter(param_binding.index, ctx.qbe_type(n->bound_type));
    auto        var = ILValue::variable(binding.depth, binding.index, param.type);

    ctx.add_operation(
        AllocDef {
            (align <= 4) ? 4ul : 8ul,
            sz,
            var,
        });
    std::visit(
        overloads {
            [&ctx, &param, &var](BoolType const &) {
                ctx.add_operation(
                    StoreDef {
                        param,
                        var,
                    });
            },
            [&ctx, &param, &var](IntType const &int_type) {
                ctx.add_operation(
                    StoreDef {
                        param,
                        var,
                    });
            },
            [&ctx, &param, &var](FloatType const &flt_type) {
                ctx.add_operation(
                    StoreDef {
                        param,
                        var,
                    });
            },
            [](std::monostate const &) {
                UNREACHABLE();
            },
            [&ctx, &param_binding, &binding, &align](auto const &descr) {
                intptr_t sz = static_cast<intptr_t>(descr.size_of());
                ctx.add_operation(
                    BlitDef {
                        ILValue::parameter(param_binding.index, ILBaseType::L),
                        ILValue::variable(binding.depth, binding.index, ILBaseType::L),
                        sz,
                    });
            },
        },
        n->bound_type->description);
    return QBEOperand { n, ILValue::null() };
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
    return QBEOperand { n, ILValue::null() };
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
        auto &file = ctx.program.files[ctx.current_file];
        auto &function = file.functions[ctx.current_function];
        if (function.ret_allocation > 0) {
            if (auto res = assign(QBEOperand { n, ILValue::return_value(ILBaseType::L) }, impl.expression, ctx); !res.has_value()) {
                return res;
            };
            ret.expr = ILValue::return_value(ILBaseType::L);
        } else {
            ret.expr = TRY_GENERATE(impl.expression, ctx).get_value();
        }
    }
    ctx.add_operation(ret);
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Struct const &impl, QBEContext &ctx)
{
    return QBEOperand { n, ILValue::null() };
}

template<>
GenResult generate_qbe_node(ASTNode const &n, VariableDeclaration const &impl, QBEContext &ctx)
{
    size_t      size = n->bound_type->size_of();
    auto const &binding = ctx.add(impl.name, n->bound_type);
    auto        var_ref = ILValue::variable(binding.depth, binding.index, ILBaseType::L);
    ctx.add_operation(
        AllocDef {
            (size < 8) ? 4u : 8u,
            size,
            var_ref,
        });
    QBEOperand operand { n, var_ref };
    if (impl.initializer != nullptr) {
        return assign(operand, impl.initializer, ctx);
    }
    return operand;
}

template<>
GenResult generate_qbe_node(ASTNode const &n, UnaryExpression const &impl, QBEContext &ctx)
{
    auto const &rhs_type { impl.operand->bound_type };
    auto        rhs_value_type { rhs_type->value_type() };

    QBEOperand   operand { TRY_GENERATE(impl.operand, ctx) };
    QBEUnaryExpr expr { n, impl.op, operand };
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
    if (impl.condition->bound_type != TypeRegistry::boolean) {
        if (auto res = cast(condition_var, TypeRegistry::boolean, ctx); !res.has_value()) {
            return res;
        } else {
            condition_var = res.value();
        }
    }
    ctx.add_operation(
        JnzDef {
            condition_var.get_value(),
            cont_loop,
            end_loop,
        });
    ctx.add_operation(LabelDef { cont_loop });
    TRY_GENERATE(impl.statement, ctx);
    ctx.add_operation(JmpDef { top_loop });
    ctx.add_operation(LabelDef { end_loop });
    return QBEOperand { n, ILValue::null() };
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
    info("[QBE] Generating IL");
    if (auto res = generate_qbe_node(node, ctx); res.has_value()) {
        info("[QBE] IL generated");
        return ctx.program;
    } else {
        log_error(L"[QBE] Error generating IL: {}", res.error());
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
