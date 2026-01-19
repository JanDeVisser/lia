/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <App/Parser.h>
#include <App/QBE/QBE.h>

namespace Lia::QBE {

ILBaseType basetype(ILType const &type)
{
    return std::visit(
        overloads {
            [](ILBaseType const &base_type) -> ILBaseType {
                return static_cast<ILBaseType>(static_cast<uint8_t>(base_type) & 0xFC);
            },
            [](auto const &) -> ILBaseType {
                UNREACHABLE();
            } },
        type);
}

ILBaseType must_extend(ILType const &type)
{
    return std::visit(
        overloads {
            [](ILBaseType const &base_type) -> ILBaseType {
                switch (base_type) {
                case ILBaseType::B:
                case ILBaseType::SB:
                    return ILBaseType::SB;
                case ILBaseType::UB:
                    return ILBaseType::UB;
                case ILBaseType::H:
                case ILBaseType::SH:
                    return ILBaseType::SH;
                case ILBaseType::UH:
                    return ILBaseType::UH;
                case ILBaseType::W:
                case ILBaseType::SW:
                case ILBaseType::UW:
                    return ILBaseType::W;
                case ILBaseType::L:
                    return ILBaseType::L;
                default:
                    UNREACHABLE();
                }
            },
            [](auto const &) -> ILBaseType {
                UNREACHABLE();
            } },
        type);
}

ILBaseType targettype(ILType const &type)
{
    return std::visit(
        overloads {
            [](ILBaseType const &base_type) -> ILBaseType {
                switch (base_type) {
                case ILBaseType::L:
                case ILBaseType::S:
                case ILBaseType::D:
                case ILBaseType::W:
                    return base_type;
                default:
                    return ILBaseType::W;
                }
            },
            [](auto const &) -> ILBaseType {
                UNREACHABLE();
            } },
        type);
}

int align_of(ILBaseType const &type)
{
    switch (type) {
#undef S
#define S(T, Code, Str, Align, Size) \
    case ILBaseType::T:              \
        return Align;                \
        break;
        ILBASETYPES(S)
#undef S
    default:
        UNREACHABLE();
    }
}

int align_of(std::wstring const &type)
{
    if (type == L":slice_t") {
        return TypeRegistry::string->align_of();
    }
    NYI(L"Alignment of non-base type `{}`", type);
}

int size_of(ILBaseType const &type)
{
    switch (type) {
#undef S
#define S(T, Code, Str, Align, Size) \
    case ILBaseType::T:              \
        return Size;                 \
        break;
        ILBASETYPES(S)
#undef S
    default:
        UNREACHABLE();
    }
}

int size_of(std::wstring const &type)
{
    if (type == L":slice_t") {
        return TypeRegistry::string->align_of();
    }
    NYI(L"Size of non-base type `{}`", type);
}

int align_of(ILType const &type)
{
    return std::visit([](auto const &t) -> int {
        return align_of(t);
    },
        type);
}

int size_of(ILType const &type)
{
    return std::visit([](auto const &t) -> int {
        return size_of(t);
    },
        type);
}

std::wostream &operator<<(std::wostream &os, ILBaseType const &type)
{
    switch (type) {
#undef S
#define S(T, Code, Str, Align, Size) \
    case ILBaseType::T:              \
        os << #Str;                  \
        break;
        ILBASETYPES(S)
#undef S
    default:
        UNREACHABLE();
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, ILType const &type)
{
    std::visit(
        overloads {
            [&os](ILBaseType const &inner) {
                os << inner;
            },
            [&os](std::wstring const &inner) {
                os << 'L';
            } },
        type);
    return os;
}

std::wostream &operator<<(std::wostream &os, ILOperation const &op)
{
    switch (op) {
#undef S
#define S(Op, Str, LiaOp) \
    case ILOperation::Op: \
        os << #Str;       \
        break;
        ILOPERATIONS(S)
#undef S
    default:
        UNREACHABLE();
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, ILValue const &value)
{
    std::visit(
        overloads {
            [&os](Local const &local) {
                os << "%v" << local.var;
            },
            [&os](ILValue::Global const &global) {
                os << '$' << global.name;
            },
            [&os](ILValue::Literal const &literal) {
                os << literal.literal;
            },
            [&os](ILValue::Variable const &param) {
                os << std::format(L"%{}$", param.name);
            },
            [&os](ILValue::Parameter const &param) {
                os << std::format(L"%{}$$", param.name);
            },
            [&os, &value](double const &dbl) {
                std::visit(
                    overloads {
                        [&os](ILBaseType const &type) {
                            switch (type) {
                            case ILBaseType::D:
                                os << "d_";
                                break;
                            case ILBaseType::S:
                                os << "s_";
                                break;
                            default:
                                break;
                            }
                        },
                        [](auto const &t) {
                            UNREACHABLE();
                        } },
                    value.type);
                os << dbl;
            },
            [&os](int64_t const &i) {
                os << i;
            },
            [&os](std::vector<ILValue> const &seq) {
                auto first { true };
                for (auto const &v : seq) {
                    if (!first) {
                        os << ", ";
                    }
                    first = false;
                    os << v;
                }
            },
        },
        value.inner);
    return os;
}

std::wostream &operator<<(std::wostream &os, AllocDef const &impl)
{
    os << "    " << impl.target << " = " << impl.target.type << " alloc" << impl.alignment << " " << impl.bytes;
    return os;
}

std::wostream &operator<<(std::wostream &os, BlitDef const &impl)
{
    os << "    blit " << impl.src << ", " << impl.dest << ", " << impl.bytes;
    return os;
}

std::wostream &operator<<(std::wostream &os, CallDef const &impl)
{
    os << "    ";
    if (!std::holds_alternative<ILBaseType>(impl.target.type)
        || std::get<ILBaseType>(impl.target.type) != ILBaseType::V) {
        os << impl.target << " = " << targettype(impl.target.type) << ' ';
    }
    std::wstring_view n { impl.name };
    if (auto colon = n.rfind(L':'); colon != std::wstring_view::npos) {
        n = std::wstring_view { n }.substr(colon + 1);
    }
    os << "call $" << n << '(';
    auto first { true };
    for (auto const &arg : impl.args) {
        if (!first) {
            os << ", ";
        }
        first = false;
        std::visit(
            overloads {
                [&os](ILBaseType const &bt) {
                    os << must_extend(bt);
                },
                [&os](std::wstring const &t) {
                    os << t;
                } },
            arg.type);
        os << " " << arg;
    }
    os << ")";
    if (n != impl.name) {
        os << " # " << impl.name;
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, CastDef const &impl)
{
    os << "    " << impl.target << " = " << impl.target.type << " cast " << impl.expr;
    return os;
}

std::wostream &operator<<(std::wostream &os, CopyDef const &impl)
{
    os << "    " << impl.target << " = " << targettype(impl.target.type) << " copy " << impl.expr;
    return os;
}

std::wostream &operator<<(std::wostream &os, ExprDef const &impl)
{
    std::wstringstream op;
    assert(std::holds_alternative<ILBaseType>(impl.target.type));
    auto t = std::get<ILBaseType>(impl.target.type);
    if (impl.op >= ILOperation::Equals) {
        op << 'c';
        if (impl.op >= ILOperation::GreaterEqual) {
            switch (t) {
            case ILBaseType::UB:
            case ILBaseType::UH:
            case ILBaseType::UW:
                op << 'u';
                break;
            case ILBaseType::S:
            case ILBaseType::D:
                break;
            default:
                op << 's';
                break;
            }
        }
    }
    op << impl.op;
    if (impl.op >= ILOperation::Equals) {
        op << basetype(impl.lhs.type);
    }
    os << "    " << impl.target << " = " << targettype(impl.target.type) << " " << op.str() << " " << impl.lhs << ", " << impl.rhs;
    return os;
}

std::wostream &operator<<(std::wostream &os, HltDef const &impl)
{
    os << "    hlt";
    return os;
}

std::wostream &operator<<(std::wostream &os, JmpDef const &impl)
{
    os << "    jmp @lbl_" << impl.label;
    return os;
}

std::wostream &operator<<(std::wostream &os, JnzDef const &impl)
{
    os << "    jnz " << impl.expr << ", @lbl_" << impl.on_true << ", @lbl_" << impl.on_false;
    return os;
}

std::wostream &operator<<(std::wostream &os, LabelDef const &impl)
{
    os << "@lbl_" << impl.label;
    return os;
}

std::wostream &operator<<(std::wostream &os, LoadDef const &impl)
{
    os << "    " << impl.target << " = " << targettype(impl.target.type) << " load" << impl.target.type << " " << impl.pointer;
    return os;
}

std::wostream &operator<<(std::wostream &os, PhiDef::PhiArgDef const &impl)
{
    os << "@lbl_" << impl.come_from << " " << impl.expr;
    return os;
}

std::wostream &operator<<(std::wostream &os, PhiDef const &impl)
{
    os << "    " << impl.target << " = " << impl.type << " phi ";
    bool first { true };
    for (auto const &arg : impl.args) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << arg;
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, RetDef const &impl)
{
    os << "    ret";
    if (impl.expr) {
        os << ' ' << impl.expr.value();
    }
    return os;
}

std::wostream &operator<<(std::wostream &os, StoreDef const &impl)
{
    os << "    store" << basetype(impl.expr.type) << ' ' << impl.expr << ", " << impl.target;
    return os;
}

std::wostream &operator<<(std::wostream &os, VaArgDef const &impl)
{
    os << "    " << impl.target << " = " << impl.type << " vaarg " << impl.arglist;
    return os;
}

std::wostream &operator<<(std::wostream &os, VaStartDef const &impl)
{
    os << "    vastart " << impl.arglist;
    return os;
}
}
