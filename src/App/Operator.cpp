/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ranges>

#include <App/Operator.h>

namespace Lia {

std::vector<BinaryOperator> binary_ops {
    { TypeKind::IntType, Operator::Add, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::FloatType, Operator::Add, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::Subtract, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::FloatType, Operator::Subtract, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::Multiply, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::FloatType, Operator::Multiply, PseudoType::Lhs, PseudoType::Lhs },
    { TypeRegistry::string, Operator::Multiply, TypeKind::IntType, PseudoType::Lhs },
    { TypeKind::IntType, Operator::Divide, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::FloatType, Operator::Divide, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::Modulo, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::FloatType, Operator::Modulo, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::Equals, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::FloatType, Operator::Equals, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::OptionalType, Operator::Equals, TypeKind::VoidType, TypeRegistry::boolean },
    { TypeKind::IntType, Operator::NotEqual, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::FloatType, Operator::NotEqual, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::IntType, Operator::Less, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::FloatType, Operator::Less, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::IntType, Operator::LessEqual, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::FloatType, Operator::LessEqual, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::IntType, Operator::Greater, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::FloatType, Operator::Greater, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::IntType, Operator::GreaterEqual, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::FloatType, Operator::GreaterEqual, PseudoType::Lhs, TypeRegistry::boolean },
    { TypeKind::IntType, Operator::BinaryAnd, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::BinaryOr, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::BinaryXor, PseudoType::Lhs, PseudoType::Lhs },
    { TypeKind::IntType, Operator::ShiftLeft, TypeRegistry::u8, PseudoType::Lhs },
    { TypeKind::IntType, Operator::ShiftRight, TypeRegistry::u8, PseudoType::Lhs },
    { TypeRegistry::boolean, Operator::LogicalAnd, TypeRegistry::boolean, TypeRegistry::boolean },
    { TypeRegistry::boolean, Operator::LogicalOr, TypeRegistry::boolean, TypeRegistry::boolean },
};

std::vector<UnaryOperator> unary_ops = {
    { Operator::BinaryInvert, TypeKind::IntType, PseudoType::Self },
    { Operator::Idempotent, TypeKind::IntType, PseudoType::Self },
    { Operator::Idempotent, TypeKind::FloatType, PseudoType::Self },
    { Operator::Negate, TypeKind::IntType, PseudoType::Self },
    { Operator::Negate, TypeKind::FloatType, PseudoType::Self },
    { Operator::LogicalInvert, TypeKind::BoolType, PseudoType::Self },
    { Operator::LogicalInvert, TypeKind::OptionalType, PseudoType::Boolean },
    { Operator::Length, TypeKind::SliceType, PseudoType::Long },
    { Operator::Length, TypeKind::Array, PseudoType::Long },
    { Operator::Length, TypeKind::DynArray, PseudoType::Long },
    { Operator::Length, TypeKind::ZeroTerminatedArray, PseudoType::Long },
    { Operator::Unwrap, TypeKind::OptionalType, PseudoType::Refer },
};

std::map<Operator, Operator> assign_ops = {
    { Operator::AssignAnd, Operator::BinaryAnd },
    { Operator::AssignAnd, Operator::BinaryAnd },
    { Operator::AssignDecrement, Operator::Subtract },
    { Operator::AssignDivide, Operator::Divide },
    { Operator::AssignIncrement, Operator::Add },
    { Operator::AssignModulo, Operator::Modulo },
    { Operator::AssignMultiply, Operator::Multiply },
    { Operator::AssignOr, Operator::BinaryOr },
    { Operator::AssignShiftLeft, Operator::ShiftLeft },
    { Operator::AssignShiftRight, Operator::ShiftRight },
    { Operator::AssignXor, Operator::BinaryXor },
};

Operand::Operand(pType t)
    : type(std::move(t))
{
}

Operand::Operand(TypeKind k)
    : type(k)
{
}

Operand::Operand(PseudoType pseudo_type)
    : type(pseudo_type)
{
}

bool Operand::matches(pType const &concrete, pType const &hint) const
{
    static bool first = true;
    if (first) {
        first = false;
        for (auto const &[ix, result] : std::ranges::views::enumerate(
                 std::vector<OpResult> {
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     PseudoType::Lhs,
                     TypeRegistry::boolean,
                     TypeRegistry::boolean,
                 })) {
            binary_ops[ix].result = result;
        }
    }

    auto concrete_value_type = concrete->value_type();
    return std::visit(
        overloads {
            [&concrete_value_type](pType const &t) -> bool {
                return concrete_value_type == t;
            },
            [&concrete_value_type](TypeKind k) -> bool {
                return concrete_value_type->is(k);
            },
            [&hint, &concrete_value_type](PseudoType pseudo_type) -> bool {
                assert(hint != nullptr);
                return hint == concrete_value_type;
            },
        },
        type);
}

[[nodiscard]] bool BinaryOperator::matches(pType const &concrete_lhs, pType const &concrete_rhs) const
{
    return lhs.matches(concrete_lhs, concrete_rhs) && rhs.matches(concrete_rhs, concrete_lhs);
}
}
