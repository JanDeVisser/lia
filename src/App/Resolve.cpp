/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Utf8.h>

#include <App/Operator.h>
#include <App/Parser.h>
#include <App/SyntaxNode.h>
#include <App/Type.h>

namespace Lia {

template<typename T>
pType resolve(T const &, ASTNode const &)
{
    fatal("No resolve method for `{}`", typeid(T).name());
}

template<>
pType resolve(TypeNameNode const &d, ASTNode const &n)
{
    auto t = n.repo->find_type(d.name);
    if (t == nullptr) {
        return nullptr;
    }
    while (t != nullptr && is<TypeAlias>(t)) {
        t = std::get<TypeAlias>(t->description).alias_of;
    }
    return t;
}

template<>
pType resolve(ReferenceDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().referencing(resolve(d.referencing));
}

template<>
pType resolve(SliceDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().slice_of(resolve(d.slice_of));
}

template<>
pType resolve(ZeroTerminatedArrayDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().zero_terminated_array_of(resolve(d.array_of));
}

template<>
pType resolve(ArrayDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().array_of(resolve(d.array_of), d.size);
}

template<>
pType resolve(DynArrayDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().dyn_array_of(resolve(d.array_of));
}

template<>
pType resolve(OptionalDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().optional_of(resolve(d.optional_of));
}

template<>
pType resolve(ResultDescriptionNode const &d, ASTNode const &n)
{
    return TypeRegistry::the().result_of(resolve(d.success), resolve(d.error));
}

pType resolve(ASTNode const &n)
{
    assert(is<TypeSpecification>(n));
    auto const &impl = get<TypeSpecification>(n);
    return std::visit(
        [&n](auto const &description) -> pType {
            return resolve(description, n);
        },
        impl.description);
}

}
