/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <Util/Logging.h>

#include <Lang/Operator.h>

namespace Lang {

char const *LiaKeyword_name(LiaKeyword kw)
{
    switch (kw) {
#undef S
#define S(KW, S)         \
    case LiaKeyword::KW: \
        return S;
        LiaKeywords(S)
#undef S
            default : UNREACHABLE();
    }
}

char const *Operator_name(Operator op)
{
    switch (op) {
#undef S
#define S(O)          \
    case Operator::O: \
        return #O;
        Operators(S)
#undef S
            default : UNREACHABLE();
    }
}

}
