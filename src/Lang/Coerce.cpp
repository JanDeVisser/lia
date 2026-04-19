/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Utf8.h>

#include <Lang/Operator.h>
#include <Lang/Parser.h>
#include <Lang/SyntaxNode.h>
#include <Lang/Type.h>

namespace Lang {

template<typename T>
ASTNode coerce(ASTNode n, T const &, pType const &target)
{
    assert(n->bound_type);
    if (target == n->bound_type) {
        return n;
    }
    return {};
}

ASTNode coerce(ASTNode n, pType const &target)
{
    return std::visit(
        [&n, &target](auto const &impl) -> ASTNode {
            return coerce(n, impl, target);
        },
        n->node);
}

}
