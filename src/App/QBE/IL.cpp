/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ranges>
#include <sstream>

#include <App/Parser.h>
#include <App/QBE/QBE.h>

namespace Lia::QBE {

bool qbe_first_class_type(pType const &type)
{
    auto const &t = type->value_type();
    return std::visit(
        overloads {
            [](IntType const &) -> bool {
                return true;
            },
            [](FloatType const &) -> bool {
                return true;
            },
            [](BoolType const &) -> bool {
                return true;
            },
            [](ZeroTerminatedArray const &) -> bool {
                return true;
            },
            [](auto const &) -> bool {
                return false;
            },
        },
        t->description);
}

ILBaseType qbe_type_code(IntType const &type)
{
    switch (type.width_bits) {
    case 8:
        return ILBaseType::B;
    case 16:
        return ILBaseType::H;
    case 32:
        return ILBaseType::W;
    case 64:
        return ILBaseType::L;
    default:
        UNREACHABLE();
    }
}

ILBaseType qbe_type_code(FloatType const &type)
{
    switch (type.width_bits) {
    case 32:
        return ILBaseType::S;
    case 64:
        return ILBaseType::D;
    default:
        UNREACHABLE();
    }
}

ILBaseType qbe_type_code(ZeroTerminatedArray const &)
{
    return ILBaseType::L;
}

ILBaseType qbe_type_code(BoolType const &)
{
    return ILBaseType::W;
}

ILBaseType qbe_type_code(StructType const &)
{
    return ILBaseType::L;
}

ILBaseType qbe_type_code(auto const &type)
{
    int                   status;
    std::type_info const &ti = typeid(type);
    auto                 *realname = abi::__cxa_demangle(ti.name(), NULL, NULL, &status);
    warning("Assuming qbe_type_code(`{}') is `l`", realname);
    return ILBaseType::L;
}

ILBaseType qbe_type_code(pType const &type)
{
    return std::visit(
        [](auto const &descr) -> ILBaseType {
            return qbe_type_code(descr);
        },
        type->description);
}

ILBaseType qbe_load_code(pType const &type)
{
    return std::visit(
        overloads {
            [](IntType const &int_type) -> ILBaseType {
                switch (int_type.width_bits) {
                case 8:
                    return (int_type.is_signed) ? ILBaseType::SB : ILBaseType::UB;
                case 16:
                    return (int_type.is_signed) ? ILBaseType::SH : ILBaseType::UH;
                case 32:
                    return (int_type.is_signed) ? ILBaseType::SW : ILBaseType::UW;
                case 64:
                    return ILBaseType::L;
                default:
                    UNREACHABLE();
                }
            },
            [](BoolType const &) -> ILBaseType {
                return ILBaseType::W;
            },
            [](FloatType const &float_type) -> ILBaseType {
                return qbe_type_code(float_type);
            },
            [](EnumType const &enum_type) -> ILBaseType {
                return qbe_load_code(enum_type.underlying_type);
            },
            [](auto const &) -> ILBaseType {
                return ILBaseType::L;
            } },
        type->description);
}

ILBaseType basetype(ILType const &type)
{
    return std::visit(
        overloads {
            [](std::monostate const &) -> ILBaseType {
                UNREACHABLE();
            },
            [](ILBaseType const &base_type) -> ILBaseType {
                return static_cast<ILBaseType>(static_cast<uint8_t>(base_type) & 0xFC);
            },
            [](ILStructType const &) -> ILBaseType {
                return ILBaseType::L;
            } },
        type);
}

ILType qbe_type(pType const &type)
{
    return std::visit(
        overloads {
            [](VoidType const &) -> ILType {
                return std::monostate();
            },
            [](BoolType const &) -> ILType {
                return ILBaseType::W;
            },
            [](FloatType const &flt_type) -> ILType {
                switch (flt_type.width_bits) {
                case 32:
                    return ILBaseType::S;
                case 64:
                    return ILBaseType::D;
                default:
                    UNREACHABLE();
                }
            },
            [](IntType const &int_type) -> ILType {
                switch (int_type.width_bits) {
                case 8:
                    return (int_type.is_signed) ? ILBaseType::SB : ILBaseType::UB;
                case 16:
                    return (int_type.is_signed) ? ILBaseType::SH : ILBaseType::UH;
                case 32:
                    return ILBaseType::W;
                case 64:
                    return ILBaseType::L;
                default:
                    UNREACHABLE();
                }
            },
            [](ReferenceType const &) -> ILType {
                return ILBaseType::L;
            },
            [](ZeroTerminatedArray const &) -> ILType {
                return ILBaseType::L;
            },
            [](SliceType const &slice) -> ILType {
                return ILStructType { L":slice_t", slice.align_of(), slice.size_of() };
            },
            [&type](OptionalType const &opt) -> ILType {
                return ILStructType { std::format(L":opt{}", *(type.id)), opt.align_of(), opt.size_of() };
            },
            [&type](StructType const &strukt) -> ILType {
                return ILStructType { std::format(L":struct{}", *(type.id)), strukt.align_of(), strukt.size_of() };
            },
            [](auto const &descr) -> ILType {
                NYI("return_type() for type description type `{}`", typeid(decltype(descr)).name());
            } },
        type->description);
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
            [](std::monostate const &) -> ILBaseType {
                UNREACHABLE();
            },
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
            [](ILStructType const &) -> ILBaseType {
                return ILBaseType::L;
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

int align_of(ILStructType const &type)
{
    return type.align;
}

int align_of(std::monostate const &)
{
    return 0;
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

int size_of(ILStructType const &type)
{
    return type.size;
}

int size_of(std::monostate const &)
{
    return 0;
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
            [](std::monostate const) {
            },
            [&os](ILBaseType const &inner) {
                os << inner;
            },
            [&os](ILStructType const &inner) {
                os << inner.name;
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
                os << "%local_" << local.var;
            },
            [&os](ILValue::Global const &global) {
                os << '$' << global.name;
            },
            [&os](ILValue::Literal const &literal) {
                os << literal.literal;
            },
            [&os](ILValue::Variable const &var) {
                os << "%var_" << var.depth << '.' << var.index;
            },
            [&os](ILValue::Parameter const &param) {
                os << "%param_" << param.index;
            },
            [&os](ILValue::ReturnValue const &) {
                os << "%ret$";
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
            [&os](ILValue::Env const &env) {
                os << "%local_" << env.local.var;
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
    os << "    " << impl.target << " = l alloc" << impl.alignment << " " << impl.bytes;
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
    if (!std::holds_alternative<std::monostate>(impl.target.type)) {
        os << impl.target << " = " << impl.target.type << ' ';
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
                [&os, &arg](ILValue::Env const &env) {
                    os << L"env";
                },
                [&os, &arg](auto const &) {
                    std::visit(
                        overloads {
                            [](std::monostate const &) {
                                UNREACHABLE();
                            },
                            [&os](ILBaseType const &bt) {
                                os << must_extend(bt);
                            },
                            [&os](ILStructType const &t) {
                                os << t.name;
                            } },
                        arg.type);
                } },
            arg.inner);
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

std::wostream &operator<<(std::wostream &os, ILFunction const &function)
{
    if (function.exported) {
        os << "export ";
    }
    os << "function " << qbe_type(function.return_type) << " $" << function.name << '(';
    auto first = true;
    for (auto const &param : function.parameters) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << qbe_type(param.type) << " %" << param.name << "$$";
    }
    os << R"() {
@start
)";
    for (auto const &instruction : function.instructions) {
        os << instruction;
    }
    os << R"(}

)";
    return os;
}

bool flatten_type(pType const &type, bool first, std::wostream &os);

bool flatten_type(BoolType const &, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << 'w';
    return false;
}

bool flatten_type(FloatType const &flt_type, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << qbe_type_code(flt_type);
    return false;
}

bool flatten_type(IntType const &int_type, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << qbe_type_code(int_type);
    return false;
}

bool flatten_type(PointerType const &strukt, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << 'l';
    return false;
}

bool flatten_type(SliceType const &, bool first, std::wostream &os)
{
    if (!first) {
        os << ", ";
    }
    os << "l, l";
    return false;
}

bool flatten_type(StructType const &strukt, bool first, std::wostream &os)
{
    for (auto const &field : strukt.fields) {
        first = flatten_type(field.type, first, os);
    }
    return first;
}

bool flatten_type(OptionalType const &opt, bool first, std::wostream &os)
{
    first = flatten_type(opt.type, first, os);
    first = flatten_type(TypeRegistry::boolean, first, os);
    return first;
}

bool flatten_type(auto const &descr, bool first, std::wostream &os)
{
    NYI("flatten_type for {}", typeid(descr).name());
}

bool flatten_type(pType const &type, bool first, std::wostream &os)
{
    return std::visit(
        [&os, &first](auto const &descr) {
            return flatten_type(descr, first, os);
        },
        type->description);
}

std::wostream &operator<<(std::wostream &os, ILFile const &file)
{
    for (auto const type : file.types) {
        std::visit(
            overloads {
                [&os](SliceType const &) {
                    os << "type :slice_t = { l, l }\n";
                },
                [&os, &type](StructType const &strukt) {
                    os << "type :struct" << *(type.id) << " = { ";
                    flatten_type(strukt, true, os);
                    os << " }\n";
                },
                [&os, &type](OptionalType const &opt) {
                    os << "type :opt" << *(type.id) << " = { ";
                    flatten_type(opt, true, os);
                    os << " }\n";
                },
                [](auto const &) {
                    UNREACHABLE();
                } },
            type->description);
    }
    if (!file.types.empty()) {
        os << '\n';
    }
    for (auto const &function : file.functions) {
        os << function;
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.strings)) {
        os << "data $str_" << ix + 1 << " = { ";
        for (auto ch : s) {
            os << std::format(L"w {:d}, ", ch);
        }
        os << "w 0 }\n";
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.cstrings)) {
        os << "data $cstr_" << ix + 1 << " = { ";
        for (auto ch : s) {
            os << std::format(L"b {:d}, ", ch);
        }
        os << L"b 0 }\n";
    }
    return os;
}

std::optional<ILBinding> ILFunction::find(std::wstring_view name)
{
    for (auto const &scope : variables) {
        for (auto const &binding : scope) {
            if (binding.name == name) {
                return binding;
            }
        }
    }
    for (auto const &binding : parameters) {
        if (binding.name == name) {
            return binding;
        }
    }
    return {};
}

ILBinding const &ILFunction::add(std::wstring_view name, pType const &type)
{
    assert(!variables.empty());
    return variables.back().emplace_back(std::wstring { name }, type, variables.size(), variables.back().size());
}

ILBinding const &ILFunction::add_parameter(std::wstring_view name, pType const &type)
{
    return parameters.emplace_back(std::wstring { name }, type, 0, parameters.size());
}

void ILFunction::push()
{
    variables.emplace_back();
}

void ILFunction::pop()
{
    assert(!variables.empty());
    variables.pop_back();
}
}
