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

static GenResult binexpr(QBEBinExpr const &expr, ILOperation op, ILType type, QBEContext &ctx)
{
    auto    lhs_value = dereference(expr.lhs, ctx).value();
    auto    rhs_value = dereference(expr.rhs, ctx).value();
    int     var = ++ctx.next_var;
    ILValue ret = ILValue::local(var, type);
    ctx.add_operation(
        ExprDef {
            lhs_value.get_value(),
            rhs_value.get_value(),
            op,
            ret,
        });
    return QBEOperand(expr.node, ret);
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
static constexpr wchar_t const *empty_optional { L"Empty optional value" };

static void check_division_by_zero(QBEOperand const &rhs, QBEContext &ctx)
{
    auto zero { ILValue::local(++ctx.next_var, ILBaseType::W) };
    int  carry_on = ++ctx.next_label;
    int  abort_mission = ++ctx.next_label;
    ctx.add_operation(
        ExprDef {
            ILValue::integer(0, rhs.get_value().type),
            rhs.get_value(),
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

static void check_optional(QBEOperand const &val, OptionalType const &optional, QBEContext &ctx)
{
    auto zero { ILValue::local(++ctx.next_var, ILBaseType::W) };
    auto flag_ptr { ILValue::pointer(++ctx.next_var) };
    auto flag_val { ILValue::local(++ctx.next_var, ILBaseType::W) };
    auto flag_val_2 { ILValue::local(++ctx.next_var, ILBaseType::W) };
    int  carry_on = ++ctx.next_label;
    int  abort_mission = ++ctx.next_label;
    ctx.add_operation(
        ExprDef {
            val.get_value(),
            ILValue::integer(optional.type->size_of(), ILBaseType::L),
            ILOperation::Add,
            flag_ptr,
        });
    ctx.add_operation(
        LoadDef {
            flag_ptr,
            flag_val,
        });
    ctx.add_operation(
        ExprDef {
            ILValue::integer(0x00000001, ILBaseType::W),
            flag_val,
            ILOperation::And,
            flag_val_2,
        });
    ctx.add_operation(
        ExprDef {
            ILValue::integer(0, ILBaseType::W),
            flag_val_2,
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
    ILValue empty_optional_id = ctx.add_string(empty_optional);
    CallDef call_def = {
        L"libliart:lia$abort",
        ILValue::null(),
    };
    call_def.args.push_back(empty_optional_id);
    call_def.args.push_back(ILValue::integer(wcslen(empty_optional), ILBaseType::L));
    ctx.add_operation(call_def);
    ctx.add_operation(LabelDef { carry_on });
}

template<>
GenResult qbe_operator(QBEBinExpr const &expr, IntType const &lhs, IntType const &rhs, QBEContext &ctx)
{
    auto lhs_value = TRY_DEREFERENCE(expr.lhs, ctx);
    auto rhs_value = TRY_DEREFERENCE(expr.rhs, ctx);
    if (expr.op == Operator::Divide || expr.op == Operator::Modulo) {
        check_division_by_zero(rhs_value, ctx);
    }
    ILOperation op;
    ILType      type { lhs_value.get_value().type };
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
    auto lhs_value = TRY_DEREFERENCE(expr.lhs, ctx);
    auto rhs_value = TRY_DEREFERENCE(expr.rhs, ctx);
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
    return binexpr(expr, op, lhs_value.get_value().type, ctx);
}

GenResult qbe_operator(QBEBinExpr const &expr, StructType const &lhs, auto const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::MemberAccess) {
        auto id = get<Identifier>(expr.rhs.node);
        auto deref = TRY_DEREFERENCE(expr.lhs, ctx).get_value();
        auto ret = ILValue::pointer(++ctx.next_var);
        ctx.add_operation(
            ExprDef {
                deref,
                expr.rhs.get_value(),
                ILOperation::Add,
                ret,
            });
        return QBEOperand { expr.node, ret };
    }
    NYI("QBE mapping for struct operator `{}`", Operator_name(expr.op));
}

GenResult qbe_operator(QBEBinExpr const &expr, TypeType const &lhs, auto const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::MemberAccess) {
        return QBEOperand { expr.node, expr.rhs.get_value() };
    }
    NYI("QBE mapping for type operator `{}`", Operator_name(expr.op));
}

GenResult qbe_operator(QBEBinExpr const &expr, IntType const &lhs, TypeType const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::Cast) {
        if (expr.lhs.node->bound_type->value_type() == rhs.type) {
            return QBEOperand { expr.node, expr.lhs.get_value() };
        }
        ctx.add_operation(
            ExtDef {
                expr.lhs.get_value(),
                expr.rhs.get_value(),
            });
        return QBEOperand { expr.node, expr.rhs.get_value() };
    }
    NYI("QBE mapping for type operator `{}`", Operator_name(expr.op));
}

GenResult qbe_operator(QBEBinExpr const &expr, EnumType const &lhs, TypeType const &rhs, QBEContext &ctx)
{
    if (expr.op == Operator::Cast) {
        auto deref = TRY_DEREFERENCE(expr.lhs, ctx);
        if (lhs.underlying_type == rhs.type) {
            return deref;
        }

        ctx.add_operation(
            ExtDef {
                deref.get_value(),
                expr.rhs.get_value(),
            });
        return QBEOperand { expr.node, expr.rhs.get_value() };
    }
    NYI("QBE mapping for type operator `{}`", Operator_name(expr.op));
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
    auto operand = TRY_DEREFERENCE(expr.operand, ctx).get_value();
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
        return QBEOperand { expr.node, ret };
    }
    default:
        NYI("QBE mapping for bool operator `{}`", Operator_name(expr.op));
        break;
    }
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, IntType const &int_type, QBEContext &ctx)
{
    auto operand = TRY_DEREFERENCE(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.get_value().type);
    switch (expr.op) {
    case Operator::Negate:
        ctx.add_operation(
            ExprDef {
                operand.get_value(),
                ILValue::null(),
                ILOperation::Neg,
                var,
            });
        return QBEOperand { expr.node, var };
    case Operator::BinaryInvert: {
        ctx.add_operation(
            ExprDef {
                operand.get_value(),
                ILValue::literal(L"~0", operand.get_value().type),
                ILOperation::Xor,
                var,
            });
        auto ret = ILValue::local(++ctx.next_var, operand.get_value().type);
        ctx.add_operation(
            ExprDef {
                var,
                ILValue::literal(L"~0", operand.get_value().type),
                ILOperation::And,
                ret,
            });
        return QBEOperand { expr.node, ret };
    }
    default:
        NYI("QBE mapping for int operator `{}`", Operator_name(expr.op));
        break;
    }
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, FloatType const &float_type, QBEContext &ctx)
{
    auto operand = TRY_DEREFERENCE(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.get_value().type);
    switch (expr.op) {
    case Operator::Negate:
        ctx.add_operation(
            ExprDef {
                operand.get_value(),
                ILValue::null(),
                ILOperation::Neg,
                var,
            });
        return QBEOperand { expr.node, var };
    default:
        NYI("QBE mapping for float operator `{}`", Operator_name(expr.op));
        break;
    }
}

template<>
GenResult qbe_operator(QBEUnaryExpr const &expr, OptionalType const &optional, QBEContext &ctx)
{
    auto operand = TRY_DEREFERENCE(expr.operand, ctx);
    auto var = ILValue::local(++ctx.next_var, operand.get_value().type);
    switch (expr.op) {
    case Operator::Unwrap: {
        check_optional(operand, optional, ctx);
        auto ret = ILValue::pointer(++ctx.next_var);
        ctx.add_operation(
            CopyDef {
                operand.get_value(),
                ret,
            });
        return QBEOperand { expr.node, ret };
    } break;
    case Operator::LogicalInvert: {
        auto flag_ptr = ILValue::pointer(++ctx.next_var);
        ctx.add_operation(
            ExprDef {
                operand.get_value(),
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
        return QBEOperand { expr.node, ret_value };
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
        return QBEOperand { expr.node, expr.operand.get_value() };
    }
    if (expr.op == Operator::AddressOf) {
        return std::visit(
            overloads {
                [&expr](ILValue::Local const &local) -> GenResult {
                    assert(expr.operand.get_value().type == ILBaseType::L);
                    return QBEOperand { expr.node, expr.operand.get_value() };
                },
                [&expr](ILValue::Global const &) -> GenResult {
                    return QBEOperand { expr.node, expr.operand.get_value() };
                },
                [&expr](ILValue::Variable const &variable) -> GenResult {
                    return QBEOperand { expr.node, expr.operand.get_value() };
                },
                [&expr](ILValue::Parameter const &parameter) -> GenResult {
                    return QBEOperand { expr.node, expr.operand.get_value() };
                },
                [](auto const &) -> GenResult {
                    UNREACHABLE();
                    return QBEOperand { nullptr, ILValue::null() };
                } },
            expr.operand.get_value().inner);
        return QBEOperand { expr.node, expr.operand.get_value() };
    }
    return std::visit(
        [&expr, &ctx](auto const &descr) {
            return qbe_operator(expr, descr, ctx);
        },
        value_type->description);
}

}
