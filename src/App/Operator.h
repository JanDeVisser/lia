/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <variant>

#include <App/Type.h>

namespace Lia {

enum class Position {
    Prefix,
    Infix,
    Postfix,
    Closing,
};

enum class Associativity {
    Left,
    Right,
};

#define BinOps(S)    \
    S(Add)           \
    S(AddressOf)     \
    S(BinaryAnd)     \
    S(BinaryInvert)  \
    S(BinaryOr)      \
    S(BinaryXor)     \
    S(Call)          \
    S(Cast)          \
    S(Divide)        \
    S(Equals)        \
    S(Greater)       \
    S(GreaterEqual)  \
    S(Idempotent)    \
    S(Length)        \
    S(Less)          \
    S(LessEqual)     \
    S(LogicalAnd)    \
    S(LogicalInvert) \
    S(LogicalOr)     \
    S(MemberAccess)  \
    S(Modulo)        \
    S(Multiply)      \
    S(Negate)        \
    S(NotEqual)      \
    S(Range)         \
    S(Sequence)      \
    S(ShiftLeft)     \
    S(ShiftRight)    \
    S(Sizeof)        \
    S(Subscript)     \
    S(Subtract)      \
    S(Unwrap)

#define AssignmentOps(S) \
    S(Assign)            \
    S(AssignAnd)         \
    S(AssignDecrement)   \
    S(AssignDivide)      \
    S(AssignIncrement)   \
    S(AssignModulo)      \
    S(AssignMultiply)    \
    S(AssignOr)          \
    S(AssignShiftLeft)   \
    S(AssignShiftRight)  \
    S(AssignXor)

#define Operators(S) AssignmentOps(S) BinOps(S)

enum class Operator : int {
#undef S
#define S(O) O,
    Operators(S)
#undef S
};

enum class PseudoType {
    Self,
    Lhs,
    Rhs,
    Refer,
    Boolean,
    Byte,
    Long,
    String,
};

using OperandType = std::variant<TypeKind, PseudoType>;

struct Operand {
    OperandType type;

    Operand(TypeKind k);
    Operand(PseudoType pseudo_type);
    bool matches(pType const &concrete, pType const &hint = nullptr) const;
};

struct BinaryOperator {
    Operand     lhs;
    Operator    op;
    Operand     rhs;
    OperandType result;

    bool  matches(pType const &concrete_lhs, pType const &concrete_rhs) const;
    pType return_type(pType const &lhs_type, pType const &rhs_type) const;
};

struct AssignOperator {
    Operator assign_op;
    Operator bin_op;
};

struct UnaryOperator {
    Operator    op;
    Operand     operand;
    OperandType result;
};

extern std::vector<BinaryOperator>  binary_ops;
extern std::vector<UnaryOperator>   unary_ops;
extern std::map<Operator, Operator> assign_ops;

extern char const *Operator_name(Operator op);

}
