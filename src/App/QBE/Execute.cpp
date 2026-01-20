/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <ranges>

#include <Util/Align.h>

#include <App/Parser.h>
#include <App/QBE/Native.h>
#include <App/QBE/QBE.h>

namespace Lia::QBE {

using namespace Util;
using namespace Lia;

pType type_from_qbe_type(ILBaseType const &t)
{
    switch (t) {
    case ILBaseType::V:
        return TypeRegistry::void_;
    case ILBaseType::B:
    case ILBaseType::SB:
        return TypeRegistry::i8;
    case ILBaseType::UB:
        return TypeRegistry::u8;
    case ILBaseType::H:
    case ILBaseType::SH:
        return TypeRegistry::i16;
    case ILBaseType::UH:
        return TypeRegistry::u16;
    case ILBaseType::W:
    case ILBaseType::SW:
        return TypeRegistry::i32;
    case ILBaseType::UW:
        return TypeRegistry::u32;
    case ILBaseType::L:
        return TypeRegistry::i64;
    case ILBaseType::S:
        return TypeRegistry::f32;
    case ILBaseType::D:
        return TypeRegistry::f64;
    default:
        UNREACHABLE();
    }
}

pType type_from_qbe_type(ILStructType const &t)
{
    trace(L"type_from_qbe_type `{}`", t.name);
    if (t.name == L":slice_t") {
        return TypeRegistry::string;
    }
    return TypeRegistry::pointer;
}

pType type_from_qbe_type(ILType t)
{
    return std::visit(
        [](auto const &inner) -> pType {
            return type_from_qbe_type(inner);
        },
        t);
}

bool is_pointer(ILValue const &value)
{
    return std::holds_alternative<ILBaseType>(value.type)
        && (std::get<ILBaseType>(value.type) == ILBaseType::L);
}

intptr_t Frame::allocate(size_t bytes, size_t alignment)
{
    return vm.allocate(bytes, alignment);
}

intptr_t Frame::allocate(pType const &type)
{
    return vm.allocate(type);
}

void Frame::release(intptr_t ptr)
{
    vm.release(ptr);
}

uint8_t *Frame::ptr(intptr_t p)
{
    return vm.stack.data() + p;
}

uint8_t *Frame::ptr(Value const &v)
{
    return ptr(as<ptrdiff_t>(v));
}

intptr_t VM::allocate(size_t bytes, size_t alignment)
{
    if (bytes == 0) {
        return stack_pointer;
    }
    intptr_t ptr { alignat(stack_pointer, alignment) };
    if (ptr + bytes > STACK_SIZE) {
        fatal("Stack overflow");
    }
    stack_pointer = ptr + bytes;
    trace("Allocated {} bytes aligned at {}. New stack pointer {}", bytes, alignment, stack_pointer);
    return ptr;
}

intptr_t VM::allocate(pType const &type)
{
    if (type->size_of() == 0) {
        return stack_pointer;
    }
    trace(L"Allocating `{}`, size {}, align {}", type->to_string(), type->size_of(), type->align_of());
    return allocate(type->size_of(), type->align_of());
}

void VM::release(intptr_t ptr)
{
    if (ptr < 0) {
        fatal("Stack underflow");
    }
    if (ptr > stack_pointer) {
        fatal("Releasing unallocated stack space");
    }
    auto bytes = stack_pointer - ptr;
    stack_pointer = ptr;
    trace("Released {} bytes. New stack pointer {}", bytes, stack_pointer);
}

void assign(pFrame const &frame, ILValue const &val_ref, Value const &v)
{
    if (v.type->size_of() == 0) {
        return;
    }
    std::visit(
        overloads {
            [frame, &v](Local const &local) {
                if (frame->locals.size() < local.var + 1) {
                    frame->locals.resize(local.var + 1);
                }
                trace(L"{} -> %v{}", v.to_string(), local.var);
                frame->locals[local.var] = v;
            },
            [frame, &v](ILValue::Variable const &variable) {
                trace(L"{} -> %{}$", v.to_string(), variable.name);
                frame->variables[variable.name] = v;
            },
            [frame, &v](ILValue::Parameter const &parameter) {
                trace(L"{} -> %{}$", v.to_string(), parameter.name);
                frame->variables[parameter.name] = v;
            },
            [](auto const &inner) {
                UNREACHABLE();
            } },
        val_ref.inner);
}

Value get(pFrame const &frame, ILValue const &val_ref)
{
    auto ret = std::visit(
        overloads {
            [&frame](Local const &local) -> Value {
                if (frame->locals.size() < local.var + 1) {
                    fatal("No local value with id `{}`in frame", local.var);
                }
                auto v = frame->locals[local.var];
                trace(L"{} <- %v{}", v.to_string(), local.var);
                return frame->locals[local.var];
            },
            [&frame](ILValue::Variable const &variable) -> Value {
                if (frame->variables.contains(variable.name)) {
                    auto v = frame->variables[variable.name];
                    trace(L"{} <- %{}$", v.to_string(), variable.name);
                    return v;
                }
                fatal(L"No variable with name `{}` in frame", variable.name);
                return {};
            },
            [&frame](ILValue::Parameter const &param) -> Value {
                if (frame->arguments.contains(param.name)) {
                    auto v = frame->arguments[param.name];
                    trace(L"{} <- %{}$$", v.to_string(), param.name);
                    return v;
                }
                fatal(L"No argument with name `{}` in frame", param.name);
                return {};
            },
            [&frame](ILValue::Global const &global) -> Value {
                auto const &vm = *(frame.repo);
                auto const &globals = vm.globals[frame->file.id];
                if (globals.contains(global.name)) {
                    return globals.at(global.name);
                }
                fatal(L"No global with name `{}`", global.name);
                return {};
            },
            [](int64_t const &int_val) -> Value {
                Value v { int_val };
                trace(L"{} <- int {}", v.to_string(), int_val);
                return v;
            },
            [](double const &dbl_val) -> Value {
                return { dbl_val };
            },
            [](auto const &inner) -> Value {
                UNREACHABLE();
            } },
        val_ref.inner);
    if (ret.type == TypeRegistry::boolean) {
        ret = Value { TypeRegistry::i32, (as<bool>(ret) ? 1 : 0) };
    }
    return ret;
}

Value get(pFrame const &frame, ILValue const &val_ref, ILType const &type)
{
    auto const &t = type_from_qbe_type(type);
    Value       v = get(frame, val_ref);
    if (v.type == TypeRegistry::pointer && t != v.type) {
        v = make_from_buffer(t, frame->ptr(v));
    }
    return v;
}

void store(pFrame const &frame, pType type, void *target, ILValue const &src_ref)
{
    Value src = get(frame, src_ref);
    if (type == TypeRegistry::string) {
        src = make_from_buffer(type, frame->ptr(src));
    }
    if (std::holds_alternative<Local>(src_ref.inner) && std::get<Local>(src_ref.inner).type == LocalType::Reference) {
        src = make_from_buffer(type, frame->ptr(src));
    }
    void *src_ptr = address_of(src);
    assert(src_ptr != nullptr);
    trace(L"memcpy({}, {} @{}, {})", target, src, src_ptr, type->to_string());
    memcpy(target, src_ptr, type->size_of());
}

VM::VM(ILProgram const &program)
    : program(program)
{
    globals.resize(program.files.size());
}

size_t VM::size() const
{
    return frames.size();
}

bool VM::empty() const
{
    return frames.empty();
}

pFrame VM::new_frame(ILFile const &file, ILFunction const &function)
{
    frames.emplace_back(*this, file, function);
    return { this };
}

void VM::release_frame()
{
    frames.pop_back();
}

Frame const &VM::operator[](size_t ix) const
{
    return frames[ix];
}

Frame &VM::operator[](size_t ix)
{
    return frames[ix];
}

struct ExecError {
    std::wstring message;
};

using ExecTermination = std::variant<Value, ExecError>;
using ExecResult = std::expected<ILValue, ExecTermination>;

template<typename InstrDef>
ExecResult execute(ILFunction const &il, pFrame const &frame, InstrDef const &instruction)
{
    NYI("Unimplemented execute() for {}", typeid(InstrDef).name());
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, AllocDef const &instruction)
{
    intptr_t ptr = frame->allocate(instruction.bytes, instruction.alignment);
    assign(frame, instruction.target, Value { TypeRegistry::pointer, ptr });
    ++frame->ip;
    return instruction.target;
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, BlitDef const &instruction)
{
    assert(is_pointer(instruction.src) && is_pointer(instruction.dest));
    auto src = frame->ptr(get(frame, instruction.src));
    auto dest = frame->ptr(get(frame, instruction.dest));
    for (auto ix = 0; ix < instruction.bytes; ++ix) {
        *(uint8_t *) (dest + ix) = *(uint8_t *) (src + ix);
    }
    ++frame->ip;
    return instruction.dest;
}

ExecResult native_call(ILFunction const &il, pFrame const &frame, CallDef const &instruction)
{
    intptr_t           depth { 0 };
    std::vector<pType> types;
    for (auto const &arg : instruction.args) {
        auto type = type_from_qbe_type(arg.type);
        depth += alignat(type->size_of(), 8);
        types.push_back(type);
    }
    auto     return_type = type_from_qbe_type(instruction.target.type);
    intptr_t args = frame->allocate(depth, 8);
    for (uint8_t *ptr = frame->ptr(args); auto const &arg : instruction.args) {
        auto type = type_from_qbe_type(arg.type);
        store(frame, type, ptr, arg);
        ptr += alignat(type->size_of(), 8);
    }
    intptr_t ret = frame->allocate(return_type);
    uint8_t *ret_ptr = frame->ptr(ret);
    trace(L"Native call: {} -> {}", instruction.name, return_type);
    if (native_call(as_utf8(instruction.name), frame->ptr(args), types, static_cast<void *>(ret_ptr), return_type)) {
        assign(frame, instruction.target, make_from_buffer(return_type, ret_ptr));
        frame->release(args);
        ++frame->ip;
        return instruction.target;
    }
    return std::unexpected(ExecError {});
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, CallDef const &instruction)
{
    if (auto colon = instruction.name.rfind(L':'); colon != std::wstring_view::npos) {
        return native_call(il, frame, instruction);
    }
    for (auto const &fnc : frame->file.functions) {
        std::vector<Value> args;
        for (auto const &arg : instruction.args) {
            args.push_back(get(frame, arg));
        }
        if (fnc.name == instruction.name) {
            if (auto res = execute_qbe(frame->vm, frame->file, fnc, args); !res.has_value()) {
                return std::unexpected(ExecError { res.error() });
            } else {
                assign(frame, instruction.target, res.value());
            }
            ++frame->ip;
            return instruction.target;
        }
    }
    fatal(L"Function `{}` not found", instruction.name);
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, CopyDef const &instruction)
{
    assign(frame, instruction.target, get(frame, instruction.expr));
    ++frame->ip;
    return instruction.target;
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, ExprDef const &instruction)
{
    auto lhs = get(frame, instruction.lhs);
    auto rhs = get(frame, instruction.rhs);

    Operator lia_op;
    switch (instruction.op) {
#undef S
#define S(Op, Str, LiaOp) \
    case ILOperation::Op: \
        lia_op = LiaOp;   \
        break;
        ILOPERATIONS(S)
#undef S
    default:
        UNREACHABLE();
    }
    auto v = evaluate(lhs, lia_op, rhs);
    assign(frame, instruction.target, v);
    ++frame->ip;
    return instruction.target;
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, JmpDef const &instruction)
{
    frame->ip = il.labels[instruction.label];
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, JnzDef const &instruction)
{
    auto expr = get(frame, instruction.expr);
    frame->ip = (as<bool>(expr))
        ? il.labels[instruction.on_true]
        : il.labels[instruction.on_false];
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, LabelDef const &instruction)
{
    ++frame->ip;
    return {};
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, LoadDef const &instruction)
{
    auto type { type_from_qbe_type(instruction.target.type) };
    auto src = frame->ptr(get(frame, instruction.pointer));
    assign(frame, instruction.target, make_from_buffer(type, src));
    ++frame->ip;
    return instruction.target;
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, RetDef const &instruction)
{
    if (instruction.expr) {
        return std::unexpected(get(frame, instruction.expr.value(), il.return_type));
    }
    return std::unexpected(Value {});
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, StoreDef const &instruction)
{
    auto type { type_from_qbe_type(instruction.target.type) };
    auto target = frame->ptr(get(frame, instruction.target));
    store(frame, type, target, instruction.expr);
    ++frame->ip;
    return instruction.target;
}

ExecResult execute(ILFunction const &il, pFrame const &frame, ILInstruction const &instruction)
{
    return std::visit([&il, &frame](auto const &impl) -> ExecResult {
        trace("impl {}", typeid(impl).name());
        return execute(il, frame, impl);
    },
        instruction.impl);
}

ExecutionResult execute_qbe(VM &vm, ILFile const &file, ILFunction const &function, std::vector<Value> const &args)
{
    auto frame { vm.new_frame(file, function) };
    for (auto const &[arg, param] : std::ranges::views::zip(args, function.parameters)) {
        frame->arguments[param.name] = arg;
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.strings)) {
        auto n = std::format(L"str_{}", ix + 1);
        if (!vm.globals[file.id].contains(n)) {
            vm.globals[file.id][n] = Value { (void *) s.data() };
        }
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.cstrings)) {
        auto n = std::format(L"cstr_{}", ix + 1);
        if (!vm.globals[file.id].contains(n)) {
            vm.globals[file.id][n] = Value { (void *) s.data() };
        }
    }
    auto base_pointer = vm.stack_pointer;
    while (true) {
        trace(L"function `{}`[{}] ip {}", function.name, function.instructions.size(), frame->ip);
        if (auto res = execute(function, frame, function.instructions[frame->ip]); !res.has_value()) {
            return std::visit(
                overloads {
                    [&vm, &base_pointer](Value const &ret_value) -> ExecutionResult {
                        vm.release(base_pointer);
                        return ret_value;
                    },
                    [&vm, &base_pointer](ExecError const &error) -> ExecutionResult {
                        vm.release(base_pointer);
                        return std::unexpected(error.message);
                    } },
                res.error());
        } else {
            if (frame->ip >= function.instructions.size()) {
                vm.release(base_pointer);
                return get(frame, res.value(), function.return_type);
            }
        }
    }
}

ExecutionResult execute_qbe(VM &vm)
{
    for (auto const &[file_ix, file] : std::ranges::views::enumerate(vm.program.files)) {
        if (file.has_main) {
            for (auto const &function : file.functions) {
                if (function.name == L"main") {
                    return execute_qbe(vm, file, function, {});
                }
            }
        }
    }
    return L"No main function";
}

}
