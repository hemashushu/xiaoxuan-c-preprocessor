/**
 * Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

// Build from this directory with a GCC version that supports C23 #embed:
//
// gcc -Wall -g -std=c23 -o main.elf \
//  -I ../include \
//  -I ../src/header \
//  -I ../src/common \
//  --embed-dir=resources \
//  main.c lib.c foo.c bar.c

#include <stdio.h>
#include <stdlib.h>

#include "hello.h"

int main()
{
    puts("Hello, World!");

    int value = hello();
    printf("Value: %d\n", value);

    return EXIT_SUCCESS;
}