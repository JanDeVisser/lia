/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <cstdint>
#include <ranges>

#include <Util/Align.h>

#include <App/Parser.h>
#include <App/QBE/QBE.h>

namespace Lia::QBE {

using namespace Util;
using namespace Lia;

std::wostream &operator<<(std::wostream &os, QBEValue::Ptr const &ptr)
{
    os << '[';
    switch (ptr.type) {
    case QBEValue::Ptr::PtrType::Stack:
        os << 'S';
        break;
    case QBEValue::Ptr::PtrType::Data:
        os << 'D';
        break;
    case QBEValue::Ptr::PtrType::Heap:
        os << 'H';
        break;
    }
    os << "] " << ptr.ptr;
    return os;
}

std::wostream &operator<<(std::wostream &os, QBEValue const &value)
{
    std::visit(
        overloads {
            [&os](std::monostate const &) {
                os << L"(void)";
            },
            [&os](auto const &payload) {
                os << payload;
            } },
        value.payload);
    os << " " << value.type;
    return os;
}

QBEValue Frame::make_from_buffer(ILType type, QBEValue::Ptr ptr)
{
    void const *buffer = vm.ptr(ptr);
    return std::visit(
        overloads {
            [](std::monostate const &) -> QBEValue {
                return QBEValue {};
            },
            [this, &buffer](ILBaseType const &bt) -> QBEValue {
                switch (bt) {
                case ILBaseType::V:
                    return QBEValue {};
                case ILBaseType::B:
                case ILBaseType::SB:
                    return QBEValue { bt, *((int8_t *) buffer) };
                case ILBaseType::UB:
                    return QBEValue { bt, *((uint8_t *) buffer) };
                case ILBaseType::H:
                case ILBaseType::SH:
                    return QBEValue { bt, *((int16_t *) buffer) };
                case ILBaseType::UH:
                    return QBEValue { bt, *((uint16_t *) buffer) };
                case ILBaseType::W:
                case ILBaseType::SW:
                    return QBEValue { bt, *((int32_t *) buffer) };
                case ILBaseType::UW:
                    return QBEValue { bt, *((uint32_t *) buffer) };
                case ILBaseType::L:
                    return QBEValue { bt, *((int64_t *) buffer) };
                case ILBaseType::S:
                    return QBEValue { bt, *((float *) buffer) };
                case ILBaseType::D:
                    return QBEValue { bt, *((double *) buffer) };
                }
                return QBEValue {};
            },
            [this, &ptr, &type](ILStructType const &strukt) -> QBEValue {
                return QBEValue {
                    type,
                    ptr,
                };
            } },
        type);
}

QBEValue Frame::make_from_buffer(ILType type, QBEValue val)
{
    if (std::holds_alternative<QBEValue::Ptr>(val.payload)) {
        return make_from_buffer(type, std::get<QBEValue::Ptr>(val.payload));
    }
    fatal("Attempt to dereference non-pointer value");
}

Value infer_value(VM const &vm, QBEValue const &val)
{
    return std::visit(
        overloads {
            [&vm, &val](ILBaseType const &bt) -> Value {
                return std::visit(
                    overloads {
                        [](std::monostate const &) -> Value {
                            return Value {};
                        },
                        [](std::integral auto const &i) -> Value {
                            return Value { i };
                        },
                        [](std::floating_point auto const &flt) -> Value {
                            return Value { flt };
                        },
                        [&vm](QBEValue::Ptr const &ptr) -> Value {
                            return Value { vm.ptr(ptr) };
                        },
                    },
                    val.payload);
            },
            [&vm, &val](ILStructType const &strukt) -> Value {
                assert(std::holds_alternative<QBEValue::Ptr>(val.payload));
                void const *ptr = vm.ptr(std::get<QBEValue::Ptr>(val.payload));
                if (strukt.name == L"slice_t") {
                    return make_value(*((Slice *) ptr));
                } else {
                    return Value { ptr };
                }
            },
            [](std::monostate const &) -> Value {
                return Value {};
            } },
        val.type);
}

bool is_pointer(ILValue const &value)
{
    return std::visit(
        overloads {
            [](std::monostate const &) -> bool {
                return false;
            },
            [](ILBaseType const &bt) -> bool {
                return bt == ILBaseType::L;
            },
            [](ILStructType const &st) -> bool {
                return true;
            } },
        value.type);
}

QBEValue::Ptr Frame::allocate(size_t bytes, size_t alignment)
{
    return vm.allocate(bytes, alignment);
}

QBEValue::Ptr Frame::allocate(ILType const &type)
{
    return vm.allocate(type);
}

void Frame::release(QBEValue::Ptr ptr)
{
    vm.release(ptr);
}

uint8_t *Frame::ptr(QBEValue::Ptr ptr)
{
    return static_cast<uint8_t *>(vm.ptr(ptr));
}

uint8_t *Frame::ptr(QBEValue const &v)
{
    return std::visit(
        overloads {
            [this](QBEValue::Ptr const &p) -> uint8_t * {
                return ptr(p);
            },
            [](intptr_t const &p) -> uint8_t * {
                return reinterpret_cast<uint8_t *>(p);
            },
            [](auto const &) -> uint8_t * {
                fatal("Attempt to dereference non-pointer value");
            } },
        v.payload);
}

QBEValue::Ptr VM::allocate(size_t bytes, size_t alignment)
{
    if (bytes == 0) {
        return QBEValue::Ptr { QBEValue::Ptr::PtrType::Stack, stack_pointer };
    }
    intptr_t ptr { alignat(stack_pointer, alignment) };
    if (ptr + bytes > STACK_SIZE) {
        fatal("Stack overflow");
    }
    stack_pointer = ptr + bytes;
    trace("Allocated {} bytes aligned at {}. New stack pointer {}", bytes, alignment, stack_pointer);
    return QBEValue::Ptr { QBEValue::Ptr::PtrType::Stack, static_cast<size_t>(ptr) };
}

QBEValue::Ptr VM::allocate(ILType const &type)
{
    if (size_of(type) == 0) {
        return QBEValue::Ptr { QBEValue::Ptr::PtrType::Stack, stack_pointer };
    }
    trace(L"Allocating size {}, align {}", size_of(type), align_of(type));
    return allocate(size_of(type), align_of(type));
}

void VM::release(QBEValue::Ptr ptr)
{
    if (ptr.type != QBEValue::Ptr::PtrType::Stack) {
        fatal("Can only release stack memory");
    }
    if (ptr.ptr > stack_pointer) {
        fatal("Releasing unallocated stack space");
    }
    auto bytes = stack_pointer - ptr.ptr;
    stack_pointer = ptr.ptr;
    trace("Released {} bytes. New stack pointer {}", bytes, stack_pointer);
}

void *VM::ptr(QBEValue::Ptr ptr)
{
    switch (ptr.type) {
    case QBEValue::Ptr::PtrType::Stack:
        return stack.data() + ptr.ptr;
    case QBEValue::Ptr::PtrType::Data:
        return data.data() + ptr.ptr;
    case QBEValue::Ptr::PtrType::Heap:
        NYI("Heap access in QBE execution is not yet implemented");
    }
    UNREACHABLE();
}

void const *VM::ptr(QBEValue::Ptr ptr) const
{
    switch (ptr.type) {
    case QBEValue::Ptr::PtrType::Stack:
        return stack.data() + ptr.ptr;
    case QBEValue::Ptr::PtrType::Data:
        return data.data() + ptr.ptr;
    case QBEValue::Ptr::PtrType::Heap:
        NYI("Heap access in QBE execution is not yet implemented");
    }
    UNREACHABLE();
}

void assign(pFrame const &frame, ILValue const &val_ref, QBEValue const &v)
{
    if (size_of(v.type) == 0) {
        return;
    }
    std::visit(
        overloads {
            [frame, &v](ILValue::Local const &local) {
                if (frame->locals.size() < local.var + 1) {
                    frame->locals.resize(local.var + 1);
                }
                frame->locals[local.var] = v;
            },
            [frame, &v](ILValue::Pointer const &ptr) {
                if (frame->pointers.size() < ptr.ptr + 1) {
                    frame->pointers.resize(ptr.ptr + 1);
                }
                frame->pointers[ptr.ptr] = v;
            },
            [frame, &v](ILValue::Variable const &variable) {
                frame->variables[variable.name()] = v;
            },
            [frame, &v](ILValue::ReturnValue const &ret_val) {
                frame->return_value = v;
            },
            [frame, &v](ILValue::Parameter const &parameter) {
                frame->variables[parameter.name()] = v;
            },
            [](auto const &inner) {
                fatal("Execute::assign({})", typeid(decltype(inner)).name());
            } },
        val_ref.inner);
}

QBEValue get(pFrame const &frame, ILValue const &val_ref)
{
    auto ret = std::visit(
        overloads {
            [&frame](ILValue::Local const &local) -> QBEValue {
                if (frame->locals.size() < local.var + 1) {
                    fatal("No local value with id `{}`in frame", local.var);
                }
                return frame->locals[local.var];
            },
            [&frame](ILValue::Pointer const &ptr) -> QBEValue {
                if (frame->pointers.size() < ptr.ptr + 1) {
                    fatal("No pointer value with id `{}`in frame", ptr.ptr);
                }
                return frame->pointers[ptr.ptr];
            },
            [&frame](ILValue::Variable const &variable) -> QBEValue {
                if (frame->variables.contains(variable.name())) {
                    auto v = frame->variables[variable.name()];
                    return v;
                }
                fatal(L"No variable with name `{}` in frame", variable.name());
                return {};
            },
            [&frame](ILValue::Parameter const &param) -> QBEValue {
                if (frame->arguments.contains(param.name())) {
                    auto v = frame->arguments[param.name()];
                    return v;
                }
                fatal(L"No parameter with name `{}` in frame", param.name());
                return {};
            },
            [&frame](ILValue::Global const &global) -> QBEValue {
                auto const &globals = frame->vm.globals[frame->file.id];
                if (globals.contains(global.name)) {
                    return globals.at(global.name);
                }
                fatal(L"No global with name `{}`", global.name);
                return {};
            },
            [&frame](ILValue::ReturnValue const &) -> QBEValue {
                trace(L"get(ReturnValue) {}", frame->return_value);
                return frame->return_value;
            },
            [](int64_t const &int_val) -> QBEValue {
                QBEValue v { ILBaseType::L, int_val };
                return v;
            },
            [](double const &dbl_val) -> QBEValue {
                return { ILBaseType::D, dbl_val };
            },
            [](auto const &inner) -> QBEValue {
                fatal("Value get({})", typeid(decltype(inner)).name());
            } },
        val_ref.inner);
    return ret;
}

void store(pFrame const &frame, ILType type, QBEValue::Ptr target, ILValue const &src_ref)
{
    if (size_of(type) == 0) {
        return;
    }
    QBEValue src = get(frame, src_ref);
    using voidptr = void const *;
    intptr_t p { 0 };
    std::visit(
        overloads {
            [](std::monostate const &) {
            },
            [&frame, &p](QBEValue::Ptr const &ptr) {
                p = reinterpret_cast<intptr_t>(frame->ptr(ptr));
                trace("Storing pointer {}", p);
            },
            [&p](bool const &i) {
                p = i;
            },
            [&p](int8_t const &i) {
                p = i;
            },
            [&p](uint8_t const &i) {
                p = i;
            },
            [&p](int16_t const &i) {
                p = i;
            },
            [&p](uint16_t const &i) {
                p = i;
            },
            [&p](int32_t const &i) {
                p = i;
            },
            [&p](uint32_t const &i) {
                p = i;
            },
            [&p](int64_t const &i) {
                p = i;
            },
            [&p](uint64_t const &i) {
                p = i;
            },
            [&p](float const &f) {
                p = *reinterpret_cast<intptr_t const *>(&f);
            },
            [&p](double const &f) {
                p = *reinterpret_cast<intptr_t const *>(&f);
            } },
        src.payload);
    trace(L"Storing value `{}` of type `{}` (size {}) at target `{}` [{} -> {}]", src, type, size_of(type), target, p, frame->ptr(target) - frame->vm.stack.data());
    memset(frame->ptr(target), 0, size_of(type));
    memcpy(frame->ptr(target), &p, size_of(type));
    frame->vm.dump_stack();
}

void store(pFrame const &frame, ILType type, QBEValue target, ILValue const &src_ref)
{
    if (std::holds_alternative<QBEValue::Ptr>(target.payload)) {
        store(frame, type, std::get<QBEValue::Ptr>(target.payload), src_ref);
    }
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

void VM::dump_stack() const
{
    trace("  Stack:");
    for (auto ix = 0; ix < stack_pointer; ix += sizeof(uint64_t)) {
        trace("    0x{:02x}: 0x{:016x}", ix, *reinterpret_cast<uint64_t const *>(&stack[ix]));
    }
}

struct ExecError {
    std::wstring message;
};

using ExecTermination = std::variant<QBEValue, ExecError>;
using ExecResult = std::expected<ILValue, ExecTermination>;

template<typename InstrDef>
ExecResult execute(ILFunction const &il, pFrame const &frame, InstrDef const &instruction)
{
    NYI("Unimplemented execute() for {}", typeid(InstrDef).name());
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, AllocDef const &instruction)
{
    auto ptr { frame->allocate(instruction.bytes, instruction.alignment) };
    assign(frame, instruction.target, QBEValue { ILBaseType::L, ptr });
    ++frame->ip;
    return instruction.target;
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, BlitDef const &instruction)
{
    assert(is_pointer(instruction.src));
    assert(is_pointer(instruction.dest));
    auto src = frame->ptr(get(frame, instruction.src));
    auto dest = frame->ptr(get(frame, instruction.dest));
    info(L"Blit({} ({}), {} ({}), {})", src - frame->vm.stack.data(), instruction.src, dest - frame->vm.stack.data(), instruction.dest, instruction.bytes);
    for (auto ix = 0; ix < instruction.bytes; ++ix) {
        *(uint8_t *) (dest + ix) = *(uint8_t *) (src + ix);
    }
    frame->vm.dump_stack();
    ++frame->ip;
    return instruction.dest;
}

ExecResult native_call(ILFunction const &il, pFrame const &frame, CallDef const &instruction)
{
    intptr_t            depth { 0 };
    std::vector<ILType> types;
    for (auto const &arg : instruction.args) {
        auto type { arg.type };
        depth += alignat(size_of(type), 8);
        types.push_back(type);
    }
    auto return_type { instruction.target.type };
    auto args = frame->allocate(depth, 8);
    for (auto ptr = args; auto const &arg : instruction.args) {
        store(frame, arg.type, ptr, arg);
        ptr = { QBEValue::Ptr::PtrType::Stack, ptr.ptr + alignat(size_of(arg.type), 8) };
    }
    auto     ret = frame->allocate(return_type);
    uint8_t *ret_ptr = frame->ptr(ret);
    trace(L"Native call: {} -> {}", instruction.name, return_type);
    if (native_call(as_utf8(instruction.name), frame->ptr(args), types, static_cast<void *>(ret_ptr), return_type)) {
        assign(frame, instruction.target, frame->make_from_buffer(return_type, ret));
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
        std::vector<QBEValue> args;
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
    auto src = get(frame, instruction.pointer);
    assign(frame, instruction.target, frame->make_from_buffer(instruction.target.type, src));
    ++frame->ip;
    return instruction.target;
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, RetDef const &instruction)
{
    if (instruction.expr) {
        auto retval { instruction.expr.value() };
        return std::unexpected(get(frame, retval));
    }
    return std::unexpected(QBEValue {});
}

template<>
ExecResult execute(ILFunction const &il, pFrame const &frame, StoreDef const &instruction)
{
    auto type { instruction.target.type };
    auto target = get(frame, instruction.target);
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

ExecutionResult execute_qbe(VM &vm, ILFile const &file, ILFunction const &function, std::vector<QBEValue> const &args)
{
    auto frame { vm.new_frame(file, function) };
    for (auto const &[ix, arg] : std::ranges::views::enumerate(args)) {
        std::wstring name;
        name = function.parameters[ix].name;
        frame->arguments[std::format(L"param_{}", ix)] = arg;
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.strings)) {
        auto n = std::format(L"str_{}", ix + 1);
        if (!vm.globals[file.id].contains(n)) {
            assert((vm.data_pointer + (s.length() + 1) * sizeof(wchar_t)) <= VM::STACK_SIZE);
            QBEValue val { ILBaseType::L, QBEValue::Ptr { QBEValue::Ptr::PtrType::Data, vm.data_pointer } };
            trace(L"Set global {}.{} = {} ({})", file.id, n, val, reinterpret_cast<intptr_t>(vm.data.data() + vm.data_pointer));
            for (auto ch : s) {
                *((wchar_t *) (vm.data.data() + vm.data_pointer)) = ch;
                vm.data_pointer += sizeof(wchar_t);
            }
            *((wchar_t *) (vm.data.data() + vm.data_pointer)) = 0;
            vm.data_pointer += sizeof(wchar_t);
            vm.globals[file.id][n] = val;
        }
    }
    for (auto const &[ix, s] : std::ranges::views::enumerate(file.cstrings)) {
        auto n = std::format(L"cstr_{}", ix + 1);
        if (!vm.globals[file.id].contains(n)) {
            assert((vm.data_pointer + s.length() + 1) <= VM::STACK_SIZE);
            QBEValue val { ILBaseType::L, QBEValue::Ptr { QBEValue::Ptr::PtrType::Data, vm.data_pointer } };
            for (auto ch : s) {
                *((char *) (vm.data.data() + vm.data_pointer)) = ch;
                ++vm.data_pointer;
            }
            *((wchar_t *) (vm.data.data() + vm.data_pointer)) = 0;
            vm.data_pointer += sizeof(wchar_t);
            vm.globals[file.id][n] = val;
            trace(L"Set global {}.{} = {}", file.id, n, val);
        }
    }
    QBEValue::Ptr base_pointer { QBEValue::Ptr::PtrType::Stack, vm.stack_pointer };
    while (true) {
        trace(L"function `{}`[{}] ip {}", function.name, function.instructions.size(), frame->ip);
        if (auto res = execute(function, frame, function.instructions[frame->ip]); !res.has_value()) {
            return std::visit(
                overloads {
                    [&vm, &base_pointer, &function](QBEValue const &ret_value) -> ExecutionResult {
                        trace(L"function `{}` returns `{}`", function.name, ret_value);
                        vm.release(base_pointer);
                        vm.dump_stack();
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
                return get(frame, res.value());
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
    return std::unexpected(L"No main function");
}
}
