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

bool operator==(ILType const &type, ILBaseType other)
{
    return std::holds_alternative<ILBaseType>(type) && std::get<ILBaseType>(type) == other;
}

bool operator==(ILBaseType const &type, ILType const &other)
{
    return std::holds_alternative<ILBaseType>(other) && std::get<ILBaseType>(other) == type;
}

bool operator!=(ILType const &type, ILBaseType other)
{
    return !std::holds_alternative<ILBaseType>(type) || std::get<ILBaseType>(type) != other;
}

bool operator!=(ILBaseType const &type, ILType const &other)
{
    return !std::holds_alternative<ILBaseType>(other) || std::get<ILBaseType>(other) != type;
}

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
            [](EnumType const &e) -> bool {
                return qbe_first_class_type(e.underlying_type);
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

void flatten_type(pType const &type, std::vector<ILBaseType> &components);

void flatten_type(BoolType const &, std::vector<ILBaseType> &components)
{
    components.emplace_back(ILBaseType::W);
}

void flatten_type(EnumType const &enum_type, std::vector<ILBaseType> &components)
{
    flatten_type(enum_type.underlying_type, components);
}

void flatten_type(FloatType const &flt_type, std::vector<ILBaseType> &components)
{
    components.emplace_back(qbe_type_code(flt_type));
}

void flatten_type(IntType const &int_type, std::vector<ILBaseType> &components)
{
    components.emplace_back(qbe_type_code(int_type));
}

void flatten_type(PointerType const &, std::vector<ILBaseType> &components)
{
    components.emplace_back(ILBaseType::L);
}

void flatten_type(SliceType const &, std::vector<ILBaseType> &components)
{
    components.emplace_back(ILBaseType::L);
    components.emplace_back(ILBaseType::L);
}

void flatten_type(StructType const &strukt, std::vector<ILBaseType> &components)
{
    for (auto const &field : strukt.fields) {
        flatten_type(field.type, components);
    }
}

void flatten_type(TypeType const &type_type, std::vector<ILBaseType> &components)
{
    flatten_type(type_type.type, components);
}

void flatten_type(OptionalType const &opt, std::vector<ILBaseType> &components)
{
    flatten_type(opt.type, components);
    components.emplace_back(ILBaseType::W);
}

void flatten_type(auto const &descr, std::vector<ILBaseType> &)
{
    NYI("flatten_type for {}", typeid(descr).name());
}

void flatten_type(pType const &type, std::vector<ILBaseType> &components)
{
    std::vector<ILBaseType> ret {};
    std::visit(
        [&components](auto const &descr) {
            return flatten_type(descr, components);
        },
        type->description);
}

std::vector<ILBaseType> flatten_type(pType const &type)
{
    std::vector<ILBaseType> ret;
    flatten_type(type, ret);
    return ret;
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
            [](ReferenceType const &reference) -> ILType {
                return ILBaseType::L;
            },
            [](ZeroTerminatedArray const &) -> ILType {
                return ILBaseType::L;
            },
            [&type](SliceType const &slice) -> ILType {
                return ILStructType { L":slice_t", flatten_type(type) };
            },
            [&type](EnumType const &enum_type) -> ILType {
                return qbe_type(enum_type.underlying_type);
            },
            [&type](OptionalType const &opt) -> ILType {
                return ILStructType { std::format(L":opt{}", *(type.id)), flatten_type(type) };
            },
            [&type](StructType const &strukt) -> ILType {
                return ILStructType { std::format(L":struct{}", *(type.id)), flatten_type(type) };
            },
            [&type](TypeType const &type_type) -> ILType {
                return ILStructType { std::format(L":type{}", *(type.id)), flatten_type(type) };
            },
            [](auto const &descr) -> ILType {
                NYI("qbe_type() for type description type `{}`", typeid(decltype(descr)).name());
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
    int ret { 0 };
    for (auto component : type.layout) {
        ret = std::max(ret, align_of(component));
    }
    return ret;
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
    int size { 0 };
    for (auto component : type.layout) {
        size = alignat(size, align_of(component)) + size_of(component);
    }
    return size;
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

std::wostream &operator<<(std::wostream &os, ILStructType const &type)
{
    os << type.name << " {";
    auto first { true };
    for (auto fld : type.layout) {
        if (first) {
            os << ',';
        }
        os << fld;
    }
    os << '}';
    return os;
}

std::wostream &operator<<(std::wostream &os, ILType const &type)
{
    std::visit(
        overloads {
            [](std::monostate const &) {
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
            [&os](ILValue::Local const &local) {
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

std::wostream &operator<<(std::wostream &os, ExtDef const &impl)
{
    os << "    "
       << impl.target
       << " = "
       << targettype(impl.target.type)
       << " ext"
       << get<ILBaseType>(impl.source.type)
       << " "
       << impl.source;
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

std::wostream &operator<<(std::wostream &os, NegateDef const &impl)
{
    os << "    "
       << impl.target
       << " = "
       << targettype(impl.target.type)
       << " neg "
       << impl.operand;
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
    for (auto const &[ix, param] : std::ranges::views::enumerate(function.parameters)) {
        if (!first) {
            os << ", ";
        }
        first = false;
        os << param.il_type << " %param_" << ix;
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

void emit_aggregate(pType const &type, std::wostream &os)
{
    ILType il_type = qbe_type(type);
    if (std::holds_alternative<ILStructType>(il_type)) {
        auto const &strukt { std::get<ILStructType>(il_type) };
        assert(!strukt.layout.empty());
        os << "type " << strukt.name << " = ";
        auto first { true };
        for (auto component : strukt.layout) {
            auto ch = (first) ? '{' : ',';
            os << ch << ' ';
            first = false;
            os << component;
        }
        os << " }\n";
    }
}

void emit_type(pType const &type, std::wostream &os)
{
    emit_aggregate(type, os);
    std::visit(
        overloads {
            [&os, &type](ReferenceType const &ref) {
                if (!qbe_first_class_type(ref.referencing)) {
                    emit_type(ref.referencing, os);
                }
            },
            [](auto const &) {
            } },
        type->description);
}

std::wostream &operator<<(std::wostream &os, ILFile const &file)
{
    for (auto const type : file.types) {
        emit_type(type, os);
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
    return variables.back().emplace_back(std::wstring { name }, type, variables.size(), variables.back().size(), qbe_type(type));
}

ILBinding const &ILFunction::add_parameter(std::wstring_view name, pType const &type)
{
    return parameters.emplace_back(std::wstring { name }, type, 0, parameters.size(), qbe_type(type));
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
