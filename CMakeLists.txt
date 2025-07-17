cmake_minimum_required(VERSION 3.28)
project(thayer C)

set(CMAKE_C_STANDARD 11)

add_executable(thayer main.c
        common.h
        chunk.h
        chunk.c
        memory.h
        memory.c
        debug.c
        debug.h
        value.h
        value.c
        vm.c
        vm.h
        compiler.h
        compiler.c
        scanner.h
        scanner.c)
