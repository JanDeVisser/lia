/*
 * Copyright (c) 2025, Jan de Visser <jan@finiandarcy.com>
 *
 * SPDX-License-Identifier: MIT
 */

#include <filesystem>

#include <config.h>

#include <Util/Logging.h>

#include <Lang/Config.h>

namespace Lang {

using namespace Util;
namespace fs = std::filesystem;

fs::path lia_dir()
{
    fs::path liadir { getenv("LIA_DIR") ? getenv("LIA_DIR") : LIA_APPDIR };
    if (liadir.empty()) {
        liadir = "/usr/share/lia";
    }
    auto std_lia { liadir / "share" / "std.lia" };
    if (!fs::exists(std_lia)) {
        fatal("{} not found", std_lia.string());
    }
    return liadir;
}

}
