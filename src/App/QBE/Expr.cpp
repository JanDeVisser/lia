/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

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
        auto ret = ILValue::pointer(++ctx.next_var);
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

GenResult qbe_operator(QBEBinExpr const &expr, QBEContext &ctx)
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

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, OptionalType const &optional, QBEContext &ctx)
{
    auto operand = dereference(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.type);
    switch (expr.op) {
    case Operator::Deref:
        return operand;
    case Operator::LogicalInvert: {
        auto flag_ptr = ILValue::pointer(++ctx.next_var);
        ctx.add_operation(
            ExprDef {
                operand,
                ILValue::integer(optional.type->size_of(), ILBaseType::L),
                ILOperation::Add,
                flag_ptr,
            });
        auto flag_value = ILValue::local(++ctx.next_var, ILBaseType::W);
        ctx.add_operation(
            LoadDef {
                flag_ptr,
                flag_value,
            });
        auto intermediate = ILValue::local(++ctx.next_var, ILBaseType::W);
        ctx.add_operation(
            ExprDef {
                flag_value,
                ILValue::integer(1, ILBaseType::W),
                ILOperation::Xor,
                intermediate,
            });
        auto ret_value = ILValue::local(++ctx.next_var, ILBaseType::W);
        ctx.add_operation(
            ExprDef {
                intermediate,
                ILValue::integer(1, ILBaseType::W),
                ILOperation::And,
                ret_value,
            });
        return ret_value;
    } break;
    default:
        NYI("QBE mapping for optional operator `{}`", Operator_name(expr.op));
        break;
    }
}

GenResult qbe_operator(QBEUnaryExpr const &expr, QBEContext &ctx)
{
    auto const &value_type = expr.operand.node->bound_type->value_type();
    if (expr.op == Operator::Idempotent) {
        return expr.operand.value;
    }
    if (expr.op == Operator::AddressOf) {
        return std::visit(
            overloads {
                [&expr](ILValue::Local const &local) -> ILValue {
                    UNREACHABLE();
                    return ILValue::null();
                },
                [&expr](ILValue::Global const &) -> ILValue {
                    return expr.operand.value;
                },
                [&expr](ILValue::Variable const &variable) -> ILValue {
                    return expr.operand.value;
                },
                [&expr](ILValue::Parameter const &parameter) -> ILValue {
                    return expr.operand.value;
                },
                [&expr](ILValue::Pointer const &parameter) -> ILValue {
                    return expr.operand.value;
                },
                [](auto const &) -> ILValue {
                    UNREACHABLE();
                    return ILValue::null();
                }

            },
            expr.operand.value.inner);
        return expr.operand.value;
    }
    return std::visit(
        [&expr, &ctx](auto const &descr) {
            return qbe_operator(expr, descr, ctx);
        },
        value_type->description);
}

}
