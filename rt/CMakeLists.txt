
include_directories(.)

enable_language(ASM-ATT)

add_library(
        trampoline
        STATIC
        arch/${CMAKE_SYSTEM_NAME}/${CMAKE_SYSTEM_PROCESSOR}/trampoline.s
)

add_library(
        liart
        STATIC SHARED
        dynarr.c
        meta.c
        puts.c
        utf8.c
)

set_target_properties(liart PROPERTIES LINKER_LANGUAGE C)

install(TARGETS liart trampoline
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        LIBRARY DESTINATION lib)
