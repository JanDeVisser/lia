/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdint>
#include <expected>
#include <filesystem>
#include <ostream>
#include <string>
#include <variant>
#include <vector>

#include <Util/Logging.h>

#include <App/Operator.h>
#include <App/SyntaxNode.h>

namespace Lia::QBE {

using namespace Util;
namespace fs = std::filesystem;

#define QBE_ASSERT(expr, ctx)                                            \
    do {                                                                 \
        if (!(expr)) {                                                   \
            fatal("{}:{} Assertion failed: " #expr, __FILE__, __LINE__); \
        }                                                                \
    } while (0)

enum class LocalType {
    Unknown,
    Value,
    Reference,
};

struct Local {
    LocalType type = LocalType::Unknown;
    int       var;

    static Local value(int var);
    static Local ref(int var);
};

#define ILBASETYPES(S)     \
    S(V, 0x00, void, 0, 0) \
    S(B, 0x04, b, 1, 1)    \
    S(SB, 0x05, sb, 1, 1)  \
    S(UB, 0x06, ub, 1, 1)  \
    S(H, 0x08, h, 2, 2)    \
    S(SH, 0x09, sh, 2, 2)  \
    S(UH, 0x0A, uh, 2, 2)  \
    S(W, 0x10, w, 4, 4)    \
    S(SW, 0x11, sw, 4, 4)  \
    S(UW, 0x12, uw, 4, 4)  \
    S(L, 0x20, l, 8, 8)    \
    S(S, 0x40, s, 4, 4)    \
    S(D, 0x80, d, 8, 8)

enum class ILBaseType {
#undef S
#define S(T, Code, Str, Align, Size) T = Code,
    ILBASETYPES(S)
#undef S
};

struct ILStructType {
    std::wstring name;
    intptr_t     align;
    intptr_t     size;
};

using ILType = std::variant<std::monostate, ILBaseType, ILStructType>;

std::wostream &operator<<(std::wostream &os, ILBaseType const &type);
std::wostream &operator<<(std::wostream &os, ILType const &type);

ILBaseType basetype(ILType const &type);
ILBaseType must_extend(ILType const &type);
ILBaseType targettype(ILType const &type);
int        align_of(ILType const &type);
int        size_of(ILType const &type);
bool       qbe_first_class_type(pType const &type);
ILBaseType qbe_type_code(pType const &type);
ILBaseType qbe_load_code(pType const &type);
ILType     qbe_type(pType const &type);

enum class ILInstructionType {
    Alloc,
    Blit,
    Call,
    Cast,
    Copy,
    Expr,
    Hlt,
    Jmp,
    Jnz,
    Label,
    Load,
    Phi,
    Ret,
    Store,
    VaArg,
    VaStart,
};

#define ILOPERATIONS(S)                         \
    S(Add, add, Operator::Add)                  \
    S(And, and, Operator::BinaryAnd)            \
    S(Div, div, Operator::Divide)               \
    S(Mul, mul, Operator::Multiply)             \
    S(Neg, neg, Operator::Negate)               \
    S(Or, or, Operator::BinaryOr)               \
    S(Mod, rem, Operator::Modulo)               \
    S(Sar, sar, Operator::ShiftRight)           \
    S(Shl, shl, Operator::ShiftLeft)            \
    S(Shr, shr, Operator::ShiftRight)           \
    S(Sub, sub, Operator::Subtract)             \
    S(UDiv, udiv, Operator::Divide)             \
    S(UMod, urem, Operator::Modulo)             \
    S(Xor, xor, Operator::BinaryXor)            \
    S(Equals, eq, Operator::Equals)             \
    S(NotEqual, ne, Operator::NotEqual)         \
    S(GreaterEqual, ge, Operator::GreaterEqual) \
    S(Greater, gt, Operator::Greater)           \
    S(LessEqual, le, Operator::LessEqual)       \
    S(Less, lt, Operator::Less)

enum class ILOperation {
#undef S
#define S(Op, Str, LiaOp) Op,
    ILOPERATIONS(S)
#undef S
};

struct ILValue {
    struct Variable {
        int depth;
        int index;

        std::wstring name() const
        {
            return std::format(L"var_{}.{}", depth, index);
        }
    };

    struct Parameter {
        int index;

        std::wstring name() const
        {
            return std::format(L"param_{}", index);
        }
    };

    struct ReturnValue {
    };

    struct Global {
        std::wstring name;
    };

    struct Literal {
        std::wstring literal;
    };

    struct Env {
        Local local;
    };

    using ILValueInner = std::variant<
        Local,
        Global,
        Literal,
        Variable,
        Parameter,
        ReturnValue,
        int64_t,
        double,
        Env,
        std::vector<ILValue>>;

    template<typename TypeDesc>
    static ILValue local(int var, TypeDesc td)
    {
        ILType t { std::move(td) };
        return { t, Local::value(var) };
    }

    static ILValue local_ref(int var)
    {
        ILType t { ILBaseType::L };
        return { t, Local::ref(var) };
    }

    template<typename TypeDesc>
    static ILValue global(std::wstring name, TypeDesc td)
    {
        return { ILType { std::move(td) }, Global { std::move(name) } };
    }

    template<typename TypeDesc>
    static ILValue literal(std::wstring literal, TypeDesc td)
    {
        return { ILType { std::move(td) }, Literal { std::move(literal) } };
    }

    template<typename TypeDesc>
    static ILValue variable(int depth, int index, TypeDesc td)
    {
        return { ILType { std::move(td) }, Variable { depth, index } };
    }

    template<typename TypeDesc>
    static ILValue parameter(int index, TypeDesc td)
    {
        return { ILType { std::move(td) }, Parameter { index } };
    }

    static ILValue string(int str_id)
    {
        return global(std::format(L"str_{}", str_id), ILBaseType::L);
    }

    static ILValue cstring(int str_id)
    {
        return global(std::format(L"cstr_{}", str_id), ILBaseType::L);
    }

    template<typename TypeDesc>
    static ILValue float_val(double d, TypeDesc td)
    {
        ILType t { std::move(td) };
        return { t, d };
    }

    template<typename TypeDesc>
    static ILValue integer(int64_t i, TypeDesc td)
    {
        ILType t { std::move(td) };
        return { t, i };
    }

    static ILValue sequence(std::vector<ILValue> values, int align, int size)
    {
        return { ILType { ILBaseType::V }, std::move(values) };
    }

    static ILValue env(ILValue const &val)
    {
        assert(std::holds_alternative<Local>(val.inner));
        return { val.type, Env { std::get<Local>(val.inner) } };
    }

    template<typename TypeDesc>
    static ILValue return_value(TypeDesc td)
    {
        return { ILType { std::move(td) }, ReturnValue {} };
    }

    static ILValue null()
    {
        return { std::monostate {}, 0 };
    }

    ILType       type;
    ILValueInner inner;
};

using ILValues = std::vector<ILValue>;

std::wostream &operator<<(std::wostream &os, ILValue const &value);
std::wostream &operator<<(std::wostream &os, ILOperation const &op);

struct QBEOperand {
    ILValue value;
    ASTNode node;
};

struct QBEBinExpr {
    QBEOperand lhs;
    Operator   op;
    QBEOperand rhs;
};

struct QBEUnaryExpr {
    Operator   op;
    QBEOperand operand;
};

using GenResult = std::expected<ILValue, std::wstring>;

struct AllocDef {
    size_t  alignment;
    size_t  bytes;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, AllocDef const &impl);
};

struct BlitDef {
    ILValue  src;
    ILValue  dest;
    intptr_t bytes;

    friend std::wostream &operator<<(std::wostream &os, BlitDef const &impl);
};

struct CallDef {
    std::wstring         name;
    ILValue              target;
    std::vector<ILValue> args;

    friend std::wostream &operator<<(std::wostream &os, CallDef const &impl);
};

struct CastDef {
    ILValue expr;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, CastDef const &impl);
};

struct CopyDef {
    ILValue expr;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, CopyDef const &impl);
};

struct ExprDef {
    ILValue     lhs;
    ILValue     rhs;
    ILOperation op;
    ILValue     target;

    friend std::wostream &operator<<(std::wostream &os, ExprDef const &impl);
};

struct HltDef {
    friend std::wostream &operator<<(std::wostream &os, HltDef const &impl);
};

struct JmpDef {
    int label;

    friend std::wostream &operator<<(std::wostream &os, JmpDef const &impl);
};

struct JnzDef {
    ILValue expr;
    int     on_true;
    int     on_false;

    friend std::wostream &operator<<(std::wostream &os, JnzDef const &impl);
};

struct LabelDef {
    int label;

    friend std::wostream &operator<<(std::wostream &os, LabelDef const &impl);
};

struct LoadDef {
    ILValue pointer;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, LoadDef const &impl);
};

struct PhiDef {
    struct PhiArgDef {
        int     come_from;
        ILValue expr;

        friend std::wostream &operator<<(std::wostream &os, PhiArgDef const &impl);
    };

    std::vector<PhiArgDef> args;
    ILBaseType             type;
    ILValue                target;

    friend std::wostream &operator<<(std::wostream &os, PhiDef const &impl);
};

struct RetDef {
    std::optional<ILValue> expr;

    friend std::wostream &operator<<(std::wostream &os, RetDef const &impl);
};

struct StoreDef {
    ILValue expr;
    ILValue target;

    friend std::wostream &operator<<(std::wostream &os, StoreDef const &impl);
};

struct VaArgDef {
    std::wstring arglist;
    ILBaseType   type;
    ILValue      target;

    friend std::wostream &operator<<(std::wostream &os, VaArgDef const &impl);
};

struct VaStartDef {
    std::wstring arglist;

    friend std::wostream &operator<<(std::wostream &os, VaStartDef const &impl);
};

using ILInstructionImpl = std::variant<
    AllocDef,
    BlitDef,
    CallDef,
    CastDef,
    CopyDef,
    ExprDef,
    HltDef,
    JmpDef,
    JnzDef,
    LabelDef,
    LoadDef,
    PhiDef,
    RetDef,
    StoreDef,
    VaArgDef,
    VaStartDef>;

struct ILInstruction {
    ILInstructionImpl impl;

    friend std::wostream &operator<<(std::wostream &os, ILInstruction const &instr)
    {
        std::visit([&os](auto const &impl) { os << impl << '\n'; }, instr.impl);
        return os;
    }
};

struct ILFunction;
struct ILFile;
struct ILProgram;

template<class N>
concept ir_node = std::is_same_v<N, ILFile>
    || std::is_same_v<N, ILFunction>;

struct ILBinding {
    std::wstring name;
    pType        type;
    int          depth;
    int          index;
};

using ILBindings = std::vector<ILBinding>;
using ILBindingStack = std::vector<ILBindings>;

struct ILFunction {
    size_t                     file_id;
    std::wstring               name;
    pType                      return_type;
    bool                       exported { false };
    intptr_t                   ret_allocation { 0 };
    size_t                     id;
    ILBindings                 parameters {};
    ILBindingStack             variables {};
    std::vector<ILInstruction> instructions;
    std::vector<size_t>        labels;

    std::optional<ILBinding> find(std::wstring_view name);
    ILBinding const         &add(std::wstring_view name, pType const &type);
    ILBinding const         &add_parameter(std::wstring_view name, pType const &type);
    void                     push();
    void                     pop();
    friend std::wostream    &operator<<(std::wostream &os, ILFunction const &function);
};

struct ILFile {
    std::wstring              name;
    size_t                    id;
    std::vector<pType>        types;
    std::vector<std::wstring> strings;
    std::vector<std::string>  cstrings;
    std::vector<ILFunction>   functions;
    bool                      has_exports { false };
    bool                      has_main { false };
    friend std::wostream     &operator<<(std::wostream &os, ILFunction const &function);
};

struct ILProgram {
    std::wstring        name;
    std::vector<ILFile> files;
};

using ILVariables = std::map<std::wstring, Value>;

struct QBEContext {
    int       next_label;
    int       next_var;
    fs::path  file_name;
    bool      is_export { false };
    ILProgram program {};
    size_t    current_file;
    size_t    current_function;

    ILValue                  add_string(std::wstring_view s);
    ILValue                  add_cstring(std::string_view s);
    ILType                   qbe_type(pType const &type);
    void                     add_operation(ILInstructionImpl impl);
    std::optional<ILBinding> find(std::wstring_view name);
    ILBinding const         &add(std::wstring_view name, pType const &type);
    ILBinding const         &add_parameter(std::wstring_view name, pType const &type);
    void                     push();
    void                     pop();
    ILFunction              &function();
};

struct Frame {
    struct VM         &vm;
    ILFile const      &file;
    ILFunction const  &function;
    ILVariables        variables {};
    ILVariables        arguments {};
    std::vector<Value> locals {};
    size_t             ip { 0 };

    intptr_t allocate(size_t bytes, size_t alignment);
    intptr_t allocate(pType const &type);
    void     release(intptr_t ptr);
    uint8_t *ptr(intptr_t p);
    uint8_t *ptr(Value const &v);
};

using pFrame = Ptr<Frame, struct VM>;

struct VM {
    constexpr static size_t         STACK_SIZE = 64 * 1024;
    ILProgram const                &program;
    std::vector<Frame>              frames;
    std::vector<ILVariables>        globals;
    std::array<uint8_t, STACK_SIZE> stack;
    size_t                          stack_pointer { 0 };

    VM(ILProgram const &program);
    size_t       size() const;
    bool         empty() const;
    pFrame       new_frame(ILFile const &file, ILFunction const &function);
    void         release_frame();
    Frame const &operator[](size_t ix) const;
    Frame       &operator[](size_t ix);
    intptr_t     allocate(size_t bytes, size_t alignment);
    intptr_t     allocate(pType const &type);
    void         release(intptr_t ptr);
};

using ExecutionResult = std::expected<Value, std::wstring>;

ILValue                                dereference(QBEOperand const &operand, QBEContext &ctx);
GenResult                              qbe_operator(QBEBinExpr const &expr, QBEContext &ctx);
GenResult                              qbe_operator(QBEUnaryExpr const &expr, QBEContext &ctx);
std::expected<ILProgram, std::wstring> generate_qbe(ASTNode const &node);
std::expected<void, std::wstring>      compile_qbe(ILProgram const &program);
ExecutionResult                        execute_qbe(VM &vm, ILFile const &file, ILFunction const &function, std::vector<Value> const &args);
ExecutionResult                        execute_qbe(VM &vm);
std::wostream                         &operator<<(std::wostream &os, ILFunction const &function);
std::wostream                         &operator<<(std::wostream &os, ILFile const &file);

}
