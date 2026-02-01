/*
 * Copyright (c) 2021, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#pragma once

#include <cstdio>
#include <cstring>
#include <format>
#include <iomanip>
#include <iostream>
#include <mutex>
#include <sys/signal.h>

#include <config.h>

#include <Util/Utf8.h>
#include <utility>

namespace Util {

template<class... Ts>
struct overloads : Ts... {
    using Ts::operator()...;
};

template<class T>
concept numeric = std::is_integral_v<T> || std::is_floating_point_v<T>;

template<class T>
concept numeric_or_bool = std::is_integral_v<T> || std::is_floating_point_v<T> || std::is_same_v<T, bool>;

#define LOG_LEVELS(S)        \
    S(Fatal, 0, "Fatal")     \
    S(Error, 1, "Error")     \
    S(Warning, 2, "Warning") \
    S(Info, 3, "Info")       \
    S(Trace, 4, "Trace")

enum class LogLevel {
#undef S
#define S(L, C, S) L = C,
    LOG_LEVELS(S)
#undef S
};

struct LogLevelDescription {
    LogLevel       level;
    char const    *name;
    wchar_t const *wname;
};

inline std::ostream &operator<<(std::ostream &os, LogLevel level)
{
    switch (level) {
#undef S
#define S(L, C, S)    \
    case LogLevel::L: \
        os << S;      \
        break;
        LOG_LEVELS(S)
#undef S
    default:
        std::unreachable();
    }
    return os;
}

std::optional<LogLevel> LogLevel_by_name(std::string_view const &);
struct LoggingConfig {
    LogLevel level { LogLevel::Fatal };
};

extern LoggingConfig log_config;
extern std::mutex    g_logging_mutex;

extern void set_logging_config(LoggingConfig const &c);

struct LogMessageMeta {
    std::string_view file;
    size_t           line;
    std::string_view function;
    LogLevel         level;

    std::string_view file_name() const
    {
        std::string_view f { file };
        if (f.front() == '/') {
            auto ix = f.find_last_of('/');
            if (ix != std::string_view::npos) {
                auto len = f.length() - ix - 1;
                if (len > 19) {
                    len = 19;
                }
                f = f.substr(ix + 1, len);
            }
        }
        return f;
    }
};

inline bool info_on() { return log_config.level >= LogLevel::Info; }
inline bool verbose_on() { return log_config.level >= LogLevel::Info; }
inline bool trace_on() { return log_config.level >= LogLevel::Trace; }

inline std::ostream &operator<<(std::ostream &os, LogMessageMeta const &meta)
{
    if (meta.level <= log_config.level) {
        if (meta.level > LogLevel::Info) {
            auto f = meta.file_name();
            os << f << ":"
               << std::left
               << std::setw(24 - f.length() - 1) << meta.line
               << ':' << std::setw(20) << meta.function
               << ':' << std::setw(5) << meta.level
               << ':';
        } else {
            std::string_view color;
            switch (meta.level) {
            case Util::LogLevel::Fatal:
                color = "\x1b[31m";
                break;
            case Util::LogLevel::Error:
                color = "\x1b[91m";
                break;
            case Util::LogLevel::Warning:
                color = "\x1b[93m";
                break;
            case Util::LogLevel::Info:
                color = "\x1b[92m";
                break;
            default:
                std::unreachable();
            }
            os << '[' << color << std::left << std::setw(7) << meta.level << "\x1b[m] ";
        }
    }
    return os;
}

template<typename T, typename... Args>
void logmsg(LogMessageMeta const &meta, T const *msg, Args &&...args)
{
    static_assert(false, "print_message(std::basic_string<T>) must be for char or wchar_t");
}

template<typename... Args>
void logmsg(LogMessageMeta const &meta, char const *msg, Args &&...args)
{
    std::lock_guard<std::mutex> const lock(g_logging_mutex);
    if (meta.level <= log_config.level) {
        std::cerr << meta;
        std::cerr << std::vformat(msg, std::make_format_args(args...)) << '\n';
    }
}

template<typename... Args>
void logmsg(LogMessageMeta const &meta, wchar_t const *msg, Args &&...args)
{
    std::lock_guard<std::mutex> const lock(g_logging_mutex);
    if (meta.level <= log_config.level) {
        std::cerr << meta;
        std::wcerr << std::vformat(msg, std::make_wformat_args(args...)) << '\n';
    }
}

template<typename T, typename... Args>
void trace_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg({ file, line, function, LogLevel::Trace }, message, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
void info_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg({ file, line, function, LogLevel::Info }, message, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
void warning_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg({ file, line, function, LogLevel::Warning }, message, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
void error_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args &&...args)
{
    logmsg({ file, line, function, LogLevel::Error }, message, std::forward<Args>(args)...);
}

template<typename T, typename... Args>
__attribute__((noreturn)) void fatal_msg(std::string_view const &file, size_t line, std::string_view const &function, T const *message, Args const &...args)
{
    logmsg({ file, line, function, LogLevel::Fatal }, message, std::forward<Args const &>(args)...);
    abort();
}

template<typename T, typename... Args>
void assert_msg(std::string_view const &file, size_t line, std::string_view const &function, bool condition, T const *message, Args const &...args)
{
    if (condition) {
        return;
    }
    fatal_msg(file, line, function, message, std::forward<Args const &>(args)...);
}

#ifdef assert
#undef assert
#endif

#define trace(fmt, ...) Util::trace_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define info(fmt, ...) Util::info_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define warning(fmt, ...) Util::warning_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define log_error(fmt, ...) Util::error_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define fatal(fmt, ...) Util::fatal_msg(__FILE__, __LINE__, __func__, fmt __VA_OPT__(, ) __VA_ARGS__)
#define assert(condition)                                                                    \
    do {                                                                                     \
        bool __c = (condition);                                                              \
        Util::assert_msg(__FILE__, __LINE__, __func__, __c, "Assertion error: " #condition); \
    } while (0)
#define assert_with_msg(condition, fmt, ...) Util::assert_msg(__FILE__, __LINE__, __func__, condition, "Assertion error: " #condition ": " fmt __VA_OPT__(, ) __VA_ARGS__)

#define UNREACHABLE()                                                                    \
    do {                                                                                 \
        std::cerr << __FILE__ << ':' << __LINE__ << ' ' << __func__ << " Unreachable\n"; \
        std::unreachable();                                                              \
    } while (0)
#define NYI(fmt, ...) Util::fatal_msg(__FILE__, __LINE__, __func__, "Not Yet Implemented: " fmt __VA_OPT__(, ) __VA_ARGS__)

}

inline std::wostream &operator<<(std::wostream &os, std::monostate const &)
{
    os << L"void";
    return os;
}

inline std::ostream &operator<<(std::ostream &os, std::monostate const &)
{
    os << "void";
    return os;
}

template<>
struct std::formatter<std::monostate, wchar_t> {

    template<class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext &ctx)
    {
        auto it = ctx.begin();
        if (it == ctx.end() || *it == '}') {
            return it;
        }
        ++it;
        return it;
    }

    template<class FmtContext>
    FmtContext::iterator format(std::monostate const &, FmtContext &ctx) const
    {
        return std::ranges::copy(L"(void)", ctx.out()).out;
    }
};

template<>
struct std::formatter<std::monostate, char> {

    template<class ParseContext>
    constexpr ParseContext::iterator parse(ParseContext &ctx)
    {
        auto it = ctx.begin();
        if (it == ctx.end() || *it == '}') {
            return it;
        }
        ++it;
        return it;
    }

    template<class FmtContext>
    FmtContext::iterator format(std::monostate const &, FmtContext &ctx) const
    {
        return std::ranges::copy("(void)", ctx.out()).out;
    }
};
