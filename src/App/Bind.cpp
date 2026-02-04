/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <expected>
#include <ranges>
#include <string>

#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

#include <App/QBE/QBE.h>

namespace Lia {

#define try_bind(expr)                                                 \
    (                                                                  \
        {                                                              \
            auto const &__expr = (expr);                               \
            if (auto const maybe = bind(__expr); !maybe.has_value()) { \
                return BindError { maybe.error() };                    \
            }                                                          \
            ((__expr)->bound_type);                                    \
        })

#define try_bind_member(m)                            \
    (                                                 \
        {                                             \
            auto      &__m = (m);                     \
            auto const __maybe = bind(__m);           \
            __m = __m.hunt();                         \
            if (!__maybe.has_value()) {               \
                return BindError { __maybe.error() }; \
            }                                         \
            ((__m)->bound_type);                      \
        })

template<typename R>
    requires std::ranges::range<R> && std::same_as<std::ranges::range_value_t<R>, ASTNode>
static BindResults bind_nodes(R const &nodes)
{
    pTypes                   types;
    std::optional<ASTStatus> ret {};

    for (auto &n : nodes) {
        auto res = bind(n);
        if (!res.has_value() || res.value() == nullptr) {
            if (!ret.has_value()) {
                if (!res.has_value()) {
                    ret = res.error();
                } else {
                    ret = ASTStatus::Undetermined;
                }
            }
            types.push_back(pType { nullptr });
        } else {
            types.push_back(res.value());
        }
    }
    if (ret.has_value()) {
        return std::unexpected(ret.value());
    }
    return types;
}

static BindResults bind_nodes(ASTNodes nodes)
{
    return bind_nodes(nodes | std::ranges::views::all);
}

#define try_bind_nodes(nodes)                                                 \
    (                                                                         \
        {                                                                     \
            auto const &__nodes = (nodes);                                    \
            pTypes      __types;                                              \
            if (auto const maybe = bind_nodes(__nodes); !maybe.has_value()) { \
                return BindError { maybe.error() };                           \
            } else {                                                          \
                __types = maybe.value();                                      \
            }                                                                 \
            (__types);                                                        \
        })

static BindResult bind_declaration(ASTNode declaration, ASTNode definition)
{
    assert(definition->ns.has_value());
    if (declaration->status == ASTStatus::Normalized) {
        for (auto const &generic_param : get<FunctionDeclaration>(declaration).generics) {
            auto generic_id = get<Identifier>(generic_param);
            if (auto const &generic = definition->ns->find_type(generic_id.identifier); generic != nullptr) {
                assert(is<GenericParameter>(generic));
                continue;
            }
            definition->ns->register_type(generic_id.identifier, TypeRegistry::the().generic_parameter(generic_id.identifier));
        }
    }
    return bind(declaration);
}

static ASTNode instantiate(ASTNode n, std::map<std::wstring, pType> const &generic_args)
{
    assert(n != nullptr);
    assert(is<FunctionDefinition>(n));
    auto const &this_def = get<FunctionDefinition>(n);
    auto       &decl = get<FunctionDeclaration>(this_def.declaration);
    auto       &parser = *(n.repo);

    assert(!decl.generics.empty() && decl.generics.size() == generic_args.size());
    ASTNode new_func = make_node<FunctionDefinition>(n, this_def.name);
    new_func->init_namespace();
    {
        Defer _ { [&n, &parser]() { parser.pop_namespace(); } };
        for (auto const &[name, type] : generic_args) {
            new_func->ns->register_type(name, TypeRegistry::the().alias_for(type));
        }

        auto &def = get<FunctionDefinition>(new_func);
        def.declaration = make_node<FunctionDeclaration>(
            def.declaration,
            decl.name,
            ASTNodes {},
            stamp(decl.parameters),
            stamp(decl.return_type));
        def.implementation = stamp(this_def.implementation);
    }
    auto _ = bind(new_func);
    // assert(new_func->declaration->bound_type != nullptr && !new_func->declaration->bound_type->is<Undetermined>());
    // std::wcout << "\ninstantiated " << name << ":\n";
    // new_func->dump();
    return new_func;
}

static ASTNode instantiate(ASTNode n, std::vector<pType> const &generic_args)
{
    assert(n != nullptr);
    assert(is<FunctionDefinition>(n));
    auto const &def = get<FunctionDefinition>(n);
    auto       &decl = get<FunctionDeclaration>(def.declaration);
    if (generic_args.size() != decl.generics.size()) {
        n.error("Incompatible number of generic arguments");
        return nullptr;
    }
    std::map<std::wstring, pType> generic_args_map;
    for (auto const &[param, arg] : std::ranges::views::zip(decl.generics, generic_args)) {
        auto param_id = get<Identifier>(param);
        generic_args_map[param_id.identifier] = arg;
    }
    return instantiate(n, generic_args_map);
}

template<class N>
BindResult bind(ASTNode n, N &impl)
{
    return nullptr;
}

template<>
BindResult bind(ASTNode n, BinaryExpression &impl)
{
    assert(n != nullptr);
    Parser &parser = *(n.repo);
    auto    lhs_type = try_bind_member(impl.lhs);

    if (impl.op == Operator::MemberAccess) {
        if (lhs_type->kind() != TypeKind::ReferenceType && !is<Identifier>(impl.lhs)) {
            return parser.bind_error(
                n->location,
                L"Left hand side of member access operator must be value reference");
        }
        auto lhs_value_type = lhs_type->value_type();
        if (lhs_value_type->kind() != TypeKind::StructType) {
            return parser.bind_error(
                n->location,
                L"Left hand side of member access operator must have struct type");
        }
        if (auto rhs_id = get_if<Identifier>(impl.rhs); rhs_id == nullptr) {
            return parser.bind_error(
                n->location,
                L"Right hand side of member access operator must be identifier");
        } else {
            auto const &s = std::get<StructType>(lhs_value_type->description);
            for (auto const &f : s.fields) {
                if (f.name == rhs_id->identifier) {
                    impl.rhs->bound_type = TypeRegistry::the().referencing(f.type);
                    impl.rhs->status = ASTStatus::Bound;
                    return impl.rhs->bound_type;
                }
            }
            return parser.bind_error(
                n->location,
                L"Unknown field `{}`", rhs_id->identifier);
        }
    }

    auto lhs_value_type = lhs_type->value_type();
    auto rhs_type = try_bind_member(impl.rhs);
    auto rhs_value_type = rhs_type->value_type();

    if (impl.op == Operator::Assign) {
        if (!lhs_value_type->compatible(rhs_value_type) && !rhs_value_type->assignable_to(lhs_value_type)) {
            return parser.bind_error(
                n->location,
                L"Cannot assign a value of type `{}` to a variable of type `{}`",
                rhs_type->name,
                lhs_type->name);
        }
        return lhs_type;
    }

    if (impl.op == Operator::Call && is<FunctionType>(lhs_value_type) && is<TypeList>(rhs_value_type)) {
        fatal("The `Call` binary operator should be elided during the normalization phase");
    }

    if (impl.op == Operator::Cast) {
        if (auto const lhs_const = get_if<Constant>(impl.lhs); lhs_const != nullptr) {
            if (is<TypeSpecification>(impl.rhs)) {
                if (auto type = resolve(impl.rhs); type != nullptr) {
                    if (auto const &casted = coerce(impl.lhs, type); casted != nullptr) {
                        return { type };
                    }
                }
            }
        }
        return std::visit(
            overloads {
                [&rhs_value_type, &n, &parser](IntType const &lhs_int_type, IntType const &rhs_int_type) -> BindResult {
                    if (lhs_int_type.width_bits > rhs_int_type.width_bits) {
                        return parser.bind_error(
                            n->location,
                            L"Invalid argument type. Cannot narrow integers");
                    }
                    return { rhs_value_type };
                },
                [&rhs_value_type, &n, &parser](SliceType const &lhs_slice_type, ZeroTerminatedArray const &rhs_zero_terminated_type) -> BindResult {
                    if (lhs_slice_type.slice_of != TypeRegistry::u32 || rhs_zero_terminated_type.array_of != TypeRegistry::u8) {
                        return parser.bind_error(
                            n->location,
                            L"Invalid argument type. Cannot cast slices to zero-terminated arrays except for strings");
                    }
                    return { rhs_value_type };
                },
                [&parser, &n](auto const &, auto const &) -> BindResult {
                    return parser.bind_error(
                        n->location,
                        L"Invalid argument type. Can only cast integers");
                } },
            lhs_value_type->description, rhs_value_type->description);
    }

    auto check_operators = [](Operator op, pType op_lhs_type, pType op_rhs_type) -> pType {
        for (auto const &o : binary_ops) {
            if (op == o.op && o.matches(op_lhs_type, op_rhs_type)) {
                return std::visit(
                    overloads {
                        [](pType const &result_type) -> pType {
                            return result_type;
                        },
                        [&op_lhs_type, &op_rhs_type](PseudoType const &pseudo_type) -> pType {
                            switch (pseudo_type) {
                            case PseudoType::Lhs:
                                return op_lhs_type;
                            case PseudoType::Rhs:
                                return op_rhs_type;
                            default:
                                UNREACHABLE();
                            }
                        } },
                    o.result);
            }
        }
        return nullptr;
    };

    if (auto result = check_operators(impl.op, lhs_value_type, rhs_value_type); result != nullptr) {
        return result;
    }
    if (auto const rhs_coerced_to_lhs = coerce(impl.rhs, lhs_value_type); rhs_coerced_to_lhs != nullptr) {
        if (auto result = check_operators(impl.op, lhs_value_type, rhs_coerced_to_lhs->bound_type); result != nullptr) {
            impl.rhs = rhs_coerced_to_lhs;
            return result;
        }
    }
    if (auto const lhs_coerced_to_rhs = coerce(impl.lhs, rhs_value_type); lhs_coerced_to_rhs != nullptr) {
        if (auto result = check_operators(impl.op, lhs_coerced_to_rhs->bound_type, rhs_value_type); result != nullptr) {
            impl.lhs = lhs_coerced_to_rhs;
            return result;
        }
    }

    return parser.bind_error(
        n->location,
        L"Operator `{}` cannot be applied to left hand type `{}` and right hand type `{}`",
        as_wstring(Operator_name(impl.op)),
        lhs_value_type->name,
        rhs_value_type->name);
}

template<>
BindResult bind(ASTNode n, Block &impl)
{
    auto types = try_bind_nodes(impl.statements);
    if (!types.empty()) {
        return types.back();
    }
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, Break &impl)
{
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, Call &impl)
{
    assert(n != nullptr);
    auto &parser = *(n.repo);
    if (impl.function != nullptr) {
        if (impl.function->status == ASTStatus::Initialized) {
            impl.function = normalize(impl.function);
        }
        try_bind(impl.function);
        auto const &f = get<FunctionDefinition>(impl.function);
        auto const &[_, result] = get<FunctionType>(f.declaration->bound_type);
        return result;
    }

    auto         arg_types = try_bind(impl.arguments);
    auto const  &type_descr = get<TypeList>(arg_types);
    auto        &args { get<ExpressionList>(impl.arguments).expressions };
    std::wstring name;
    ASTNodes     type_args {};
    if (auto const *id = get_if<Identifier>(impl.callable); id != nullptr) {
        name = id->identifier;
    } else if (auto const &stamped_id = get_if<StampedIdentifier>(impl.callable); stamped_id != nullptr) {
        name = stamped_id->identifier;
        type_args = stamped_id->arguments;
    }
    // function = parser.find_function_by_arg_list(id->identifier, arg_types);
    // if (function != nullptr) {
    //     return std::get<FunctionType>(function->bound_type->description).result;
    // }
    //

    auto match_non_generic_function = [&](ASTNode func_def) -> std::expected<ASTNode, ASTStatus> {
        auto const &def = get<FunctionDefinition>(func_def);
        auto const &decl = get<FunctionDeclaration>(def.declaration);
        if (decl.generics.empty()) {
            auto const &func_type_descr = get<FunctionType>(def.declaration->bound_type);
            if (type_descr.types.size() != func_type_descr.parameters.size()) {
                return nullptr;
            }
            ASTNodes reference_nodes;
            auto     made_reference_node { false };
            for (auto [ix, arg_param] : std::views::zip(args, type_descr.types, func_type_descr.parameters) | std::ranges::views::enumerate) {
                auto [arg, arg_type, param_type] { arg_param };
                auto arg_value = arg_type->value_type();
                auto param_value = param_type->value_type();
                if (arg_value != param_value) {
                    return nullptr;
                }
                if (is<ReferenceType>(param_type) && is<Constant>(arg)) {
                    return nullptr;
                }
                if (is<ReferenceType>(param_type) && !is<ReferenceType>(arg_type)) {
                    auto reference = normalize(make_node<UnaryExpression>(arg, Operator::AddressOf, arg));
                    reference_nodes.emplace_back(reference);
                    made_reference_node = true;
                } else {
                    reference_nodes.emplace_back(arg);
                }
            }
            if (made_reference_node) {
                impl.arguments = normalize(make_node<ExpressionList>(impl.arguments, reference_nodes));
                try_bind(impl.arguments);
            }
            return func_def;
        }
        return nullptr;
    };

    auto match_generic_function = [&](ASTNode func_def) -> std::expected<ASTNode, ASTStatus> {
        auto const &def = get<FunctionDefinition>(func_def);
        auto const &decl = get<FunctionDeclaration>(def.declaration);
        if (!decl.generics.empty()) {
            if (def.declaration->bound_type == nullptr) {
                if (auto const &decl_type = bind_declaration(def.declaration, func_def); !decl_type.has_value()) {
                    return BindError { decl_type.error() };
                }
            }
            std::map<std::wstring, pType> generic_args;
            for (auto const &[param_name, arg_type] : std::ranges::views::zip(decl.generics, type_args)) {
                generic_args[std::wstring(identifier(param_name))] = resolve(arg_type);
            }
            if (decl.parameters.size() != args.size()) {
                return nullptr;
            }
            for (auto const &[param, arg] : std::ranges::views::zip(decl.parameters, args)) {
                auto const &param_type = param->bound_type;
                auto const &arg_type = arg->bound_type;
                for (auto const inferred = arg_type->infer_generic_arguments(param_type); auto const &[name, arg_type] : inferred) {
                    if (generic_args.contains(name) && generic_args[name] != arg_type) {
                        return n.bind_error(
                            L"Ambiguous values inferred for generic parameter  `{}`: `{}` and `{}`",
                            name, arg_types->to_string(), generic_args[name]->to_string());
                    }
                    generic_args[name] = arg_type;
                }
            }
            auto all_inferred { true };
            for (auto const &generic_param : decl.generics) {
                if (!generic_args.contains(std::wstring(identifier(generic_param)))) {
                    all_inferred = false;
                    break;
                }
            }
            if (!all_inferred) {
                return nullptr;
            }
            return instantiate(func_def, generic_args);
        }
        return nullptr;
    };

    ASTNodes bound_overloads;
    {
        auto const overloads = parser.find_overloads(name, type_args);
        for (auto func_def : overloads) {
            if (func_def->status == ASTStatus::Initialized) {
                func_def = normalize(func_def);
            }
            auto const _ = bind(func_def);
            bound_overloads.push_back(func_def);
        }
    }
    if (type_args.empty()) {
        for (auto const &func_def : bound_overloads) {
            auto const &def = get<FunctionDefinition>(func_def);
            auto const &decl = get<FunctionDeclaration>(def.declaration);
            if (auto const ret = match_non_generic_function(func_def); !ret.has_value()) {
                return BindError { ret.error() };
            } else if (ret.value() != nullptr) {
                impl.function = ret.value();
                break;
            }
        }
        if (impl.function == nullptr) {
            for (auto const &func_def : bound_overloads) {
                auto const &def = get<FunctionDefinition>(func_def);
                auto const &decl = get<FunctionDeclaration>(def.declaration);
                if (auto const ret = match_generic_function(func_def); !ret.has_value()) {
                    return BindError { ret.error() };
                } else if (ret.value() != nullptr) {
                    impl.function = ret.value();
                    break;
                }
            }
        }
    } else {
        for (auto const &func_def : bound_overloads) {
            auto const &def = get<FunctionDefinition>(func_def);
            auto const &decl = get<FunctionDeclaration>(def.declaration);
            if (auto const ret = match_generic_function(func_def); !ret.has_value()) {
                return BindError { ret.error() };
            } else if (ret.value() != nullptr) {
                impl.function = ret.value();
                break;
            }
        }
    }
    if (impl.function != nullptr) {
        auto const &func { get<FunctionDefinition>(impl.function) };
        auto const &func_type_descr { get<FunctionType>(func.declaration->bound_type) };
        auto const &param_types { func_type_descr.parameters };
        for (auto const &[arg, param_type] : std::ranges::views::zip(args, param_types)) {
        }
        return func_type_descr.result;
    }
    if (parser.pass == 0) {
        return BindError { ASTStatus::Undetermined };
    }
    return n.bind_error(L"Unresolved function `{}{}`", name, arg_types->to_string());
}

template<>
BindResult bind(ASTNode n, Comptime &impl)
{
    auto &parser = *(n.repo);
    if (n->bound_type == nullptr) {
        switch (parser.bind(impl.statements)) {
        case ASTStatus::InternalError:
            log_error("Internal error(s) encountered during compilation of @comptime block");
            return nullptr;
        case ASTStatus::BindErrors:
        case ASTStatus::Ambiguous: {
            log_error("Error(s) found during compilation of @comptime block:");
            for (auto const &err : parser.errors) {
                log_error(L"{}:{} {}", err.location.line + 1, err.location.column + 1, err.message);
            }
            return parser.bind_error(n->location, L"Bind error in @comptime block");
        }
        case ASTStatus::Undetermined:
            return BindError { ASTStatus::Undetermined };
        case ASTStatus::Initialized:
        case ASTStatus::Normalized:
            UNREACHABLE();
        case ASTStatus::Bound:
            trace(L"Comptime script bind successful");
            break;
        }
        trace("Bound compile time script");
    }

    if (impl.output.empty()) {
        if (auto res = QBE::generate_qbe(impl.statements); !res.has_value()) {
            return parser.bind_error(n->location, res.error());
        } else {
            auto  program = res.value();
            auto &file = program.files[0];
            auto &function = file.functions[0];
            if (trace_on()) {
                trace("Compile time block IR:");
                std::wcerr << file;
                trace("---------------------------------------------------");
            }
            QBE::VM vm { program };
            if (auto exec_res = execute_qbe(vm, file, function, {}); !res.has_value()) {
                return parser.bind_error(n->location, res.error());
            } else {
                auto const output_val = exec_res.value();
                auto const output_slice = as<Slice>(as_value<Slice>(vm, output_val));
                impl.output = std::wstring { static_cast<wchar_t *>(output_slice.ptr), static_cast<size_t>(output_slice.size) };
                trace("@comptime block executed");
                trace(L"@comptime output: {}", impl.output);
            }
        }
    }

    if (auto parsed_output = parse<Block>(*(n.repo), impl.output); parsed_output) {
        trace("@comptime after parsing");
        if (trace_on()) {
            dump(parsed_output, std::wcerr);
        }
        impl.statements = normalize(parsed_output);
        trace("@comptime after normalizing");
        if (trace_on()) {
            dump(impl.statements, std::wcerr);
        }
        return bind(impl.statements);
    } else {
        log_error("@comptime parse failed");
        for (auto const &err : parser.errors) {
            log_error(L"{}:{} {}", err.location.line + 1, err.location.column + 1, err.message);
        }
        return n.bind_error(L"Error(s) parsing result of @comptime block");
    }
    return nullptr;
}

template<>
BindResult bind(ASTNode n, Constant &impl)
{
    assert(impl.bound_value.has_value());
    return impl.bound_value->type;
}

template<>
BindResult bind(ASTNode n, Continue &impl)
{
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, DeferStatement &impl)
{
    try_bind(impl.statement);
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, Dummy &impl)
{
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, Enum &impl)
{
    assert(n != nullptr);
    auto    &parser = *(n.repo);
    EnumType enoom;
    size_t   ix = 0;
    for (auto const &v : impl.values) {
        pType payload { nullptr };
        auto  enum_value = get<EnumValue>(v);
        if (enum_value.payload != nullptr) {
            payload = resolve(enum_value.payload);
            if (payload == nullptr) {
                return n.bind_error(L"Could not resolve type `{}`", to_string(enum_value.payload));
            }
        }
        size_t value = (enum_value.value != nullptr) ? as<int64_t>(get<Constant>(enum_value.value).bound_value.value()) : ix;
        enoom.values.emplace_back(enum_value.label, value, payload);
        ++ix;
    }
    enoom.underlying_type = nullptr;
    if (impl.underlying_type != nullptr) {
        enoom.underlying_type = resolve(impl.underlying_type);
        if (enoom.underlying_type == nullptr) {
            return n.bind_error(L"Could not resolve type `{}`", impl.underlying_type);
        }
    }
    auto ret = make_type(impl.name, enoom);
    parser.register_type(impl.name, ret);
    return make_type(impl.name, TypeType { .type = ret });
}

template<>
BindResult bind(ASTNode n, EnumValue &impl)
{
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, Error &impl)
{
    return bind(impl.expression);
}

template<>
BindResult bind(ASTNode n, ExpressionList &impl)
{
    return TypeRegistry::the().typelist_of(try_bind_nodes(impl.expressions));
}

template<>
BindResult bind(ASTNode n, ExternLink &impl)
{
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, ForStatement &impl)
{
    try_bind(impl.range_expr);
    if (!is<RangeType>((impl.range_expr)->bound_type)) {
        return n.bind_error(L"`for` loop range expression is a `{}`, not a range", (impl.range_expr)->bound_type);
    }
    return bind(impl.statement);
}

template<>
BindResult bind(ASTNode n, FunctionDeclaration &impl)
{
    return TypeRegistry::the().function_of(
        try_bind_nodes(impl.parameters),
        try_bind(impl.return_type));
}

template<>
BindResult bind(ASTNode n, FunctionDefinition &impl)
{
    assert(n != nullptr);
    auto &parser { *(n.repo) };
    bool  register_me = impl.declaration->bound_type == nullptr;
    if (auto maybe { bind_declaration(impl.declaration, n) }; !maybe.has_value()) {
        return BindError { maybe.error() };
    }
    auto t = (impl.declaration)->bound_type;
    assert(is<FunctionType>(t));
    if (register_me) {
        if (auto const &found = n->ns->parent_of()->ns->find_function(impl.name, t); found != nullptr && found != n) {
            return n.bind_error(L"Duplicate overload for function `{}` with type `{}`", impl.name, t);
        }
        n->ns->parent_of()->ns->register_function(impl.name, n);
    }
    auto &decl = get<FunctionDeclaration>(impl.declaration);
    if (!decl.generics.empty()) {
        return TypeRegistry::void_;
    }
    for (auto const &param : decl.parameters) {
        auto parameter = get<Parameter>(param);
        if (!parser.has_variable(parameter.name)) {
            parser.register_variable(parameter.name, param);
        }
    }
    if (auto impl_type = bind(impl.implementation); impl.implementation->status != ASTStatus::Bound) {
        return impl_type;
    }
    return TypeRegistry::void_;
}

template<class N>
    requires std::is_same_v<N, Identifier> || std::is_same_v<N, StampedIdentifier>
BindResult bind(ASTNode n, N &impl)
{
    auto const &type = n.repo->type_of(impl.identifier);
    if (type == nullptr) {
        if (n.repo->pass == 0) {
            return BindError { ASTStatus::Undetermined };
        } else {
            return n.bind_error(L"Unresolved identifier `{}`", impl.identifier);
        }
    }
    return type;
}

template<>
BindResult bind(ASTNode n, IfStatement &impl)
{
    try_bind(impl.condition);
    if (!impl.condition->bound_type->assignable_to(TypeRegistry::boolean)) {
        return n.bind_error(
            L"`if` loop condition is a `{}`, not a boolean",
            (impl.condition)->bound_type->name);
    }
    try_bind(impl.if_branch);
    if (impl.else_branch != nullptr) {
        try_bind(impl.else_branch);
    }
    auto if_type = (impl.if_branch)->bound_type;
    if (impl.else_branch == nullptr || (impl.else_branch)->bound_type == if_type) {
        return if_type;
    }
    return BindError { ASTStatus::Ambiguous };
}

template<>
BindResult bind(ASTNode n, LoopStatement &impl)
{
    return bind(impl.statement);
}

template<>
BindResult bind(ASTNode n, Module &impl)
{
    try_bind_nodes(impl.statements);
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, Parameter &impl)
{
    return bind(impl.type_name);
}

template<>
BindResult bind(ASTNode n, Program &impl)
{
    assert(n != nullptr);
    auto &parser { *(n.repo) };
    pType ret { nullptr };
    if (parser.pass == 0) {
        for (auto &[name, mod] : impl.modules) {
            n->ns->register_variable(name, mod);
        }
    }
    try_bind_nodes(impl.statements);
    try_bind_nodes(impl.modules | std::ranges::views::values);
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, PublicDeclaration &impl)
{
    return bind(impl.declaration);
}

template<>
BindResult bind(ASTNode n, Return &impl)
{
    try_bind(impl.expression);
    auto &parser { *(n.repo) };
    auto  func = parser.current_function();
    assert(func != nullptr && is<FunctionDefinition>(func));
    auto func_signature = get<FunctionDefinition>(func).declaration->bound_type;
    assert(is<FunctionType>(func_signature));
    auto return_type = get<FunctionType>(func_signature).result;
    if (impl.expression != nullptr && !impl.expression->bound_type->assignable_to(return_type)) {
        return n.bind_error(
            L"`return` expression is a `{}` and is not assignable to a `{}`",
            (impl.expression)->bound_type->name, return_type->name);
    }
    if (impl.expression == nullptr && return_type != TypeRegistry::void_) {
        return n.bind_error(
            L"`return` requires an expression of type `{}`",
            return_type->name);
    }
    if (impl.expression != nullptr && return_type == TypeRegistry::void_) {
        return n.bind_error(
            L"`return` returns from a function returning `void` and therefore cannot return a value");
    }
    return return_type;
}

template<>
BindResult bind(ASTNode n, Struct &impl)
{
    assert(n != nullptr);
    auto              &parser { *(n.repo) };
    StructType::Fields fields;
    for (auto const &m : impl.members) {
        try_bind(m);
        fields.emplace_back(get<StructMember>(m).label, m->bound_type);
    }
    pType type = TypeRegistry::the().struct_of(fields);
    parser.register_type(impl.name, type);
    return make_type(impl.name, TypeType { .type = type });
}

template<>
BindResult bind(ASTNode n, StructMember &impl)
{
    return bind(impl.member_type);
}

template<>
BindResult bind(ASTNode n, TypeSpecification &impl)
{
    auto ret = resolve(n);
    if (ret == nullptr) {
        return BindError { ASTStatus::Undetermined };
    }
    return ret;
}

template<>
BindResult bind(ASTNode n, UnaryExpression &impl)
{
    assert(n != nullptr);
    auto &parser { *(n.repo) };
    auto  operand_type = try_bind(impl.operand);
    if (impl.op == Operator::Sizeof) {
        return TypeRegistry::i64;
    }
    if (impl.op == Operator::AddressOf) {
        if (is<ReferenceType>(operand_type)) {
            return parser.bind_error(n->location, L"Cannot take address of reference");
        }
        if (is<Constant>(n)) {
            return parser.bind_error(n->location, L"Cannot take address of constant");
        }
        return TypeRegistry::the().referencing(operand_type);
    }
    for (auto const &[oper, operand, result] : unary_ops) {
        if (impl.op == oper && operand.matches(operand_type)) {
            return std::visit(
                overloads {
                    [](pType const &result_type) -> pType {
                        return result_type;
                    },
                    [&operand_type](PseudoType const &pseudo_type) -> pType {
                        switch (pseudo_type) {
                        case PseudoType::Self:
                            return operand_type;
                        case PseudoType::Boolean:
                            return TypeRegistry::boolean;
                        case PseudoType::Long:
                            return TypeRegistry::i64;
                        case PseudoType::Refer:
                            return std::visit(
                                overloads {
                                    [](OptionalType const &opt) -> pType {
                                        return TypeRegistry::the().referencing(opt.type);
                                    },
                                    [](auto const &) -> pType {
                                        UNREACHABLE();
                                        return TypeRegistry::void_;
                                    } },
                                operand_type->description);
                        default:
                            UNREACHABLE();
                        }
                    } },
                result);
        }
    }
    return parser.bind_error(
        n->location,
        L"Unary operator `{}` cannot be applied to type `{}`",
        as_wstring(Operator_name(impl.op)),
        operand_type->name);
}

template<>
BindResult bind(ASTNode n, VariableDeclaration &impl)
{
    auto  my_type = (impl.type_name != nullptr) ? try_bind(impl.type_name) : nullptr;
    auto  init_type = (impl.initializer != nullptr) ? try_bind(impl.initializer) : nullptr;
    auto &parser = *(n.repo);

    assert(my_type != nullptr || init_type != nullptr);

    if (my_type == nullptr) {
        my_type = init_type;
    }
    if (init_type != nullptr && !init_type->compatible(my_type) && !init_type->assignable_to(my_type)) {
        return n.bind_error(
            L"Type mismatch between declared type `{}` of `{}` and type of initializer value `{}`",
            my_type->name, impl.name, init_type->name);
    }
    if (n.repo->has_variable(impl.name)) {
        return n.bind_error(L"Duplicate variable name `{}`", impl.name);
    }
    parser.register_variable(impl.name, n);
    return my_type;
}

template<>
BindResult bind(ASTNode, Void &)
{
    return TypeRegistry::void_;
}

template<>
BindResult bind(ASTNode n, WhileStatement &impl)
{
    try_bind(impl.condition);
    pType t = (impl.condition)->bound_type;
    if (!is<BoolType>(t)) {
        return n.bind_error(L"`while` loop condition is a `{}`, not a boolean", t->name);
    }
    return bind(impl.statement);
}

template<>
BindResult bind(ASTNode n, Yield &impl)
{
    return bind(impl.statement);
}

/* ======================================================================== */

BindResult bind(ASTNode node)
{
    assert(node != nullptr);
    Parser &parser = *(node.repo);
    assert(node->status >= ASTStatus::Normalized);
    if (node->status == ASTStatus::Bound) {
        return node->bound_type;
    } else if (node->status > ASTStatus::Bound) {
        return BindError { node->status };
    } else { // node->status < ASTStatus::Bound) {
        if (node->ns.has_value()) {
            parser.push_namespace(node);
        }
        auto retval = std::visit(
            [&node](auto impl) {
                // We take the impl by value so we can update the copied
                // value in the bind but at the same time be able to create
                // new nodes and potentially move the nodes vector around.
                // When the bind returns we copy the (updated) copy back
                // into the node.
                auto ret = bind(node, impl);
                node->node = impl;
                return ret;
            },
            node->node);
        auto const &n = node.hunt();
        if (n->ns.has_value()) {
            parser.pop_namespace();
        }
        if (retval.has_value()) {
            auto const &type = retval.value();
            if (type == nullptr) {
                n->status = ASTStatus::Undetermined;
                parser.unbound_nodes.push_back(node);
                parser.unbound++;
                return BindError { ASTStatus::Undetermined };
            } else {
                n->bound_type = type;
                n->status = ASTStatus::Bound;
                return type;
            }
        } else {
            switch (retval.error()) {
            case ASTStatus::InternalError:
                dump(node, std::wcerr);
                assert("bind(): Internal error" == nullptr);
                break;
            case ASTStatus::Undetermined:
                parser.unbound_nodes.push_back(node);
                parser.unbound++;
                /* Fallthrough */
            default:
                n->status = retval.error();
                break;
            }
            return retval;
        }
    }
}
}
