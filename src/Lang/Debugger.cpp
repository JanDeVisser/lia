/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <iostream>

namespace Lang {

std::optional<std::wstring> get_command_string()
{
    std::cout << "*> " << std::flush;
    std::wstring ret;
    std::getline(std::wcin, ret);
    if (std::wcin.eof()) {
        return {};
    }
    return ret;
}

void dump_scopes(Interp &interpreter)
{
    std::cout << "Scopes: " << interpreter.scopes.size() << " top: 0x" << std::hex << interpreter.stack.top << std::dec << std::endl;
    if (interpreter.stack.top == 0) {
        std::cout << std::endl;
        return;
    }
    auto scopes { 0 };
    for (auto ix = 0; ix < interpreter.scopes.size(); ++ix) {
        auto &scope { interpreter.scopes[ix] };
        auto  next_bp = (ix < interpreter.scopes.size() - 1) ? interpreter.scopes[ix + 1].bp : interpreter.stack.top;
        if (scopes == 0) {
            std::cout << std::right << std::setw(2) << std::setfill(' ') << scope.bp;
        }
        std::cout << '.';
        ++scopes;
        if (next_bp > scope.bp) {
            std::cout << std::right << std::setw(4 - scopes) << std::setfill(' ') << " ";
            for (auto stix = scope.bp; stix < next_bp; stix += 8) {
                if (stix == scope.bp) {
                    std::cout << "   ";
                } else {
                    std::cout << "          ";
                }
                for (auto bix { 0 }; bix < 8 && stix + bix < next_bp; ++bix) {
                    std::cout << " 0x" << std::hex << std::setw(2) << std::setfill('0') << (interpreter.stack.stack[stix + bix] & 0xFF);
                }
                std::cout << " ";
                for (auto bix { 0 }; bix < 8 && stix + bix < next_bp; ++bix) {
                    if (isprint(interpreter.stack.stack[stix + bix])) {
                        std::cout << " " << interpreter.stack.stack[stix + bix];
                    } else {
                        std::cout << " .";
                    }
                }
                std::cout << "  ";
                for (auto &var : scope.variables) {
                    if (var.second.address == stix) {
                        std::wcout << var.first;
                    }
                }
                std::cout << std::endl;
            }
            scopes = 0;
        } else if (ix == interpreter.scopes.size() - 1) {
            std::cout << std::endl;
        }
    }
    std::cout << std::endl;
}

int debugger_main()
{
    log_error("The debugger doesn't work yet");
    return 0;
#if 0
    struct Context {
        std::wstring file_name;
        std::wstring contents;
        Parser       parser;
        ASTNode      syntax;
        ASTNode      normalized;
        IR::IRNodes  ir {};
        Value        exit_code;

        void reset()
        {
            file_name = L"";
            contents = L"";
            syntax = nullptr;
            normalized = nullptr;
            ir = {};
        }
    };
    Context ctx;

    auto trace_program = [&ctx]() {
        enum class DebuggerState {
            Run,
            Step,
        };

        DebuggerState state { DebuggerState::Step };
        auto          cb = [&state](Interp::CallbackType cb_type, Interp &interpreter, Interp::CallbackPayload const &payload) -> bool {
            switch (cb_type) {
            case Interp::CallbackType::BeforeOperation:
                std::wcout << std::get<Operation>(payload) << std::endl;
                if (state == DebuggerState::Step) {
                    while (true) {
                        std::cout << "==> " << std::flush;
                        std::wstring ret;
                        std::getline(std::wcin, ret);
                        if (ret == L"s") {
                            break;
                        } else if (ret == L"c") {
                            state = DebuggerState::Run;
                            break;
                        }
                    }
                }
                // dump_scopes(interpreter);
                break;
            case Interp::CallbackType::AfterOperation:
                dump_scopes(interpreter);
                break;
            case Interp::CallbackType::StartModule:
                std::wcout << "Initializing module " << std::get<IR::pIR>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::EndModule:
                std::wcout << "Module " << std::get<IR::pIR>(payload)->name << " initialized" << std::endl;
                break;
            case Interp::CallbackType::StartFunction:
                std::wcout << "Entering function " << std::get<pIR>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::EndFunction:
                std::wcout << "Leaving function " << std::get<pIR>(payload)->name << std::endl;
                break;
            case Interp::CallbackType::OnScopeStart:
                break;
            case Interp::CallbackType::AfterScopeStart:
                std::wcout << "Scope created" << std::endl;
                dump_scopes(interpreter);
                break;
            case Interp::CallbackType::OnScopeDrop:
                break;
            case Interp::CallbackType::AfterScopeDrop: {
                dump_scopes(interpreter);
            } break;
            }
            return true;
        };
        Interp interpreter;
        interpreter.callback = cb;
        auto ret = interpreter.execute(ctx.ir.program);
        std::wcout << ret << "\n";
        ctx.exit_code = ret;
    };

    auto quit { false };
    do {
        auto cmdline_maybe = get_command_string();
        if (!cmdline_maybe.has_value()) {
            break;
        }
        auto cmdline = *cmdline_maybe;
        if (auto stripped = strip(std::wstring_view { cmdline }); !stripped.empty()) {
            auto parts = split(stripped, ' ');
            if (parts[0] == L"quit") {
                quit = true;
            } else if (parts[0] == L"load") {
                if (parts.size() != 2) {
                    std::cerr << "Error: filename missing\n";
                } else {
                    load_file(parts[1]);
                }
            } else if (parts[0] == L"parse") {
                if (ctx.contents.empty()) {
                    std::cerr << "Error: no file loaded\n";
                } else {
                    parse_file();
                }
            } else if (parts[0] == L"build") {
                if (parts.size() != 2) {
                    std::cerr << "Error: filename missing\n";
                } else {
                    ctx.file_name = parts[1];
                    if (auto contents_maybe = load_file(ctx.file_name); contents_maybe) {
                        ctx.contents = contents_maybe.value();
                        ctx.contents = parse_file(load_file(ctx.file_name));
                        if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                            std::cerr << "Error: parse failed\n";
                        } else {
                            ctx.ir = IR::generate_ir(ctx.normalized);
                        }
                    }
                }
            } else if (parts[0] == L"cat") {
                std::wcout << ctx.contents << std::endl;
            } else if (parts[0] == L"print") {
                if (parts.size() != 2) {
                    std::cerr << "Error: name of tree to print missing\n";
                } else {
                    if (parts[1] == L"syntax") {
                        if (ctx.syntax == nullptr) {
                            std::cerr << "Error: no syntax tree available\n";
                        } else {
                            ctx.syntax->dump();
                        }
                    } else if (parts[1] == L"normalized") {
                        if (ctx.normalized == nullptr) {
                            std::cerr << "Error: no normalized tree available\n";
                        } else {
                            ctx.normalized->dump();
                        }
                    }
                }
            } else if (parts[0] == L"generate") {
                if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                    std::cerr << "Error: no bound tree available\n";
                } else {
                    ctx.ir = IR::generate_ir(ctx.normalized);
                }
            } else if (parts[0] == L"list") {
                IR::list(ctx.ir, std::wcout);
            } else if (parts[0] == L"run") {
                if (ctx.syntax == nullptr) {
                    if (parts.size() < 2) {
                        std::cerr << "Error: filename missing\n";
                    } else {
                        load_file(parts[1]);
                        parse_file(false);
                        if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                            std::cerr << "Error: parse failed\n";
                        } else {
                            ctx.ir = IR::generate_ir(ctx.normalized);
                        }
                    }
                }
                auto ret = Lang::Interpreter::execute_ir(ctx.ir);
                std::wcout << ret << "\n";
                ctx.exit_code = ret;
            } else if (parts[0] == L"trace") {
                trace_program();
            } else if (parts[0] == L"compile") {
                if (parts.size() != 2) {
                    std::cerr << "Error: filename missing\n";
                } else {
                    load_file(parts[1]);
                    parse_file();
                    if (ctx.normalized == nullptr || ctx.normalized->status != ASTNodeImpl::Status::Bound) {
                        std::cerr << "Error: parse failed\n";
                    } else {
                        ctx.ir = IR::generate_ir(ctx.normalized);
#ifdef IS_ARM64
                        MUST(Arm64::generate_arm64(ctx.ir));
#endif
#ifdef IS_X86_64
                        MUST(X86_64::generate_x86_64(ctx.ir));
#endif
                    }
                }
            } else {
                std::cout << "?\n";
            }
        }
    } while (!quit);
    return 0;
#endif
}

}
