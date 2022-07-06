cmake_minimum_required(VERSION 2.8)

set(TARGETS CACHE INTERNAL "Targets" FORCE)

option(UTILS_CLANG_COMPLETE "Generate .clang_complete files on source.")

macro(UTILS_ADD_LIBRARY target)
    add_library(${target} ${ARGN})
    set(TARGETS "${TARGETS};${target}" CACHE INTERNAL "Targets")
endmacro()

macro(UTILS_ADD_EXECUTABLE target)
    add_executable(${target} ${ARGN})
    set(TARGETS "${TARGETS};${target}" CACHE INTERNAL "Targets")
endmacro()

macro(UTILS_GENERATE_CLANG_COMPLETE)
    if(UTILS_CLANG_COMPLETE)
        foreach(target ${TARGETS})
            get_target_property(dirs ${target} INCLUDE_DIRECTORIES)
            file(WRITE "${${target}_SOURCE_DIR}/.clang_complete" "")
            foreach(dir ${dirs})
                file(APPEND "${${target}_SOURCE_DIR}/.clang_complete" "-I ${dir}\n")
            endforeach()
        endforeach()
    endif()
endmacro()
