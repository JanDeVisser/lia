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
        trace(L"Adding QBE operation {}", s.str());
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

#define TRY_GENERATE(n, ctx)                                          \
    (                                                                 \
        {                                                             \
            ASTNode __n = (n);                                        \
            ILValue __var;                                            \
            if (auto __res = generate_qbe_node(__n, (ctx)); !__res) { \
                return __res;                                         \
            } else {                                                  \
                __var = __res.value();                                \
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

ILValue dereference(QBEOperand const &operand, QBEContext &ctx)
{
    auto value = operand.value;
    auto type = operand.node->bound_type;
    if (is<ReferenceType>(type)) {
        type = get<ReferenceType>(type).referencing;
    }
    return std::visit(
        overloads {
            [&ctx, &value, &operand, &type](ILValue::Local local) -> ILValue {
                return value;
            },
            [&ctx, &value, &operand, &type](ILValue::Pointer pointer) -> ILValue {
                if (is<ReferenceType>(operand.node->bound_type) && qbe_first_class_type(type)) {
                    auto ret = ILValue::local(++ctx.next_var, ctx.qbe_type(type));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = value,
                            .target = ret,
                        });
                    return ret;
                }
                return value;
            },
            [&value, &ctx, &operand, &type](ILValue::Variable variable) -> ILValue {
                if (!is<ReferenceType>(operand.node->bound_type) && qbe_first_class_type(type)) {
                    auto ret = ILValue::local(++ctx.next_var, ctx.qbe_type(type));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = value,
                            .target = ret,
                        });
                    return ret;
                }
                return value;
            },
            [&value, &ctx, &operand, &type](ILValue::Parameter param) -> ILValue {
                if (is<ReferenceType>(operand.node->bound_type) && qbe_first_class_type(type)) {
                    auto ret = ILValue::local(++ctx.next_var, ctx.qbe_type(type));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = value,
                            .target = ret,
                        });
                    return ret;
                }
                if (qbe_first_class_type(type)) {
                    auto ret = ILValue::local(++ctx.next_var, ctx.qbe_type(type));
                    ctx.add_operation(
                        LoadDef {
                            .pointer = value,
                            .target = ret,
                        });
                }
                return value;
            },
            [&value](auto) -> ILValue {
                return value;
            } },
        value.inner);
}

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

static void raw_assign(ILValue const &lhs, ILValue const &rhs, pType const &type, QBEContext &ctx)
{
    std::visit(
        [&ctx, &lhs, &rhs](auto const &descr) {
            raw_assign(lhs, rhs, descr, ctx);
        },
        type->description);
}

GenResult assign(QBEOperand const &lhs, ASTNode const &rhs, size_t size, QBEContext &ctx)
{
    auto rhs_value { TRY_GENERATE(rhs, ctx) };
    std::visit(
        overloads {
            [&ctx, &lhs, &rhs_value, &size](OptionalType const &lhs_opt, OptionalType const &rhs_opt) {
                assert(lhs_opt.type == rhs_opt.type);
                raw_assign(lhs.value, rhs_value.value, lhs.node->bound_type, ctx);
            },
            [&ctx, &lhs](OptionalType const &optional, VoidType const &) {
                auto flag_ptr = ILValue::pointer(++ctx.next_var);
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
            [&ctx, &lhs, &rhs_value, &size](OptionalType const &optional, auto const &) {
                raw_assign(lhs.value, rhs_value.value, optional.type, ctx);
                auto flag_ptr = ILValue::pointer(++ctx.next_var);
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
            [&ctx, &lhs, &rhs_value, &size](auto const &, auto const &) {
                raw_assign(lhs.value, rhs_value.value, lhs.node->bound_type, ctx);
            } },
        lhs.node->bound_type->description, rhs->bound_type->description);
    return lhs.value;
}

GenResult cast(QBEOperand value, pType const &target_type, QBEContext &ctx)
{
    if (value.node->bound_type == target_type) {
        return value.value;
    }
    return std::visit(
        overloads {
            [&ctx, &value](OptionalType const &optional, BoolType const &) -> GenResult {
                auto flag_ptr = ILValue::pointer(++ctx.next_var);
                auto ret_value = ILValue::local(++ctx.next_var, ILBaseType::SB);
                ctx.add_operation(
                    ExprDef {
                        value.value,
                        ILValue::integer(optional.type->size_of(), ILBaseType::L),
                        ILOperation::Add,
                        flag_ptr,
                    });
                ctx.add_operation(
                    LoadDef {
                        flag_ptr,
                        ret_value,
                    });
                return ret_value;
            },
            [](auto const &, auto const &) -> GenResult {
                return std::unexpected(L"Invalid cast in QBE");
            } },
        value.node->bound_type->description, target_type->description);
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
        return assign(TRY_GENERATE(impl.lhs, ctx), impl.rhs, impl.lhs->bound_type->size_of(), ctx);
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

    ILValue              ret_val { ILValue::null() };
    ILValue              ret_alloc { ILValue::null() };
    size_t               alloc_sz { 0 };
    std::vector<ILValue> args;
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
        if (is<ReferenceType>(param->bound_type)) {
            QBE_ASSERT(is<ReferenceType>(arg->bound_type), ctx);
            auto value = TRY_GENERATE(arg, ctx).value;
            value.type = ILBaseType::L;
            args.emplace_back(value);
        } else {
            args.emplace_back(dereference(TRY_GENERATE(arg, ctx), ctx));
        }
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
            ret_val,
            args,
        });
    if (alloc_sz == 0) {
        return ret_val;
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
    return ret_alloc;
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
                return ctx.add_cstring(static_cast<char const *>(as<void const *>(*impl.bound_value)));
            },
            [&ctx, &impl](SliceType const &slice_type) -> ILValue {
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
                return ret;
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
        auto const &type = values.back().type;
        if (size > 0) {
            size = alignat(size, align_of(type));
        }
        size += size_of(type);
        align = std::max(align, align_of(type));
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
        impl.return_type->bound_type,
        ctx.is_export);
    if (auto sz = impl.return_type->bound_type->size_of(); sz > 8) {
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
    return ILValue::null();
}

template<>
GenResult generate_qbe_node(ASTNode const &n, Identifier const &impl, QBEContext &ctx)
{
    auto t = ctx.qbe_type(n->bound_type);
    if (auto binding = ctx.find(impl.identifier); binding) {
        auto b = *binding;
        if (b.depth == 0) {
            return ILValue::parameter(b.index, t);
        }
        return ILValue::variable(b.depth, b.index, t);
    }
    UNREACHABLE();
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
            condition_var = QBEOperand { res.value(), n };
        }
    }
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
    auto const &param_binding = ctx.add_parameter(impl.name, n->bound_type);
    if (is<ReferenceType>(n->bound_type)) {
        return ILValue::null();
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
        auto  r = TRY_GENERATE(impl.expression, ctx).value;
        auto &file = ctx.program.files[ctx.current_file];
        auto &function = file.functions[ctx.current_function];
        if (function.ret_allocation > 0) {
            ctx.add_operation(BlitDef {
                r,
                ILValue::return_value(ILBaseType::L),
                function.ret_allocation,
            });
            ret.expr = ILValue::return_value(ILBaseType::L);
        } else {
            ret.expr = r;
        }
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
    size_t      size = n->bound_type->size_of();
    auto const &binding = ctx.add(impl.name, n->bound_type);
    auto        var_ref = ILValue::variable(binding.depth, binding.index, ILBaseType::L);
    ctx.add_operation(
        AllocDef {
            (size < 8) ? 4u : 8u,
            size,
            var_ref,
        });
    if (impl.initializer != nullptr) {
        return assign({ var_ref, n }, impl.initializer, size, ctx);
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
    if (impl.condition->bound_type != TypeRegistry::boolean) {
        if (auto res = cast(condition_var, TypeRegistry::boolean, ctx); !res.has_value()) {
            return res;
        } else {
            condition_var = QBEOperand { res.value(), n };
        }
    }
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
