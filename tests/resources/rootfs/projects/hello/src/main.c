/**
 * Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

// You can compile this code with a C compiler that supports the C23 standard.
//
// gcc -Wall -g -std=c23 -o main.elf \
//  -I ../include \
//  -I ../src/header \
//  -I ../src/common \
//  -I ../src/resources \
//  main.c lib.c foo.c bar.c

#include <stdio.h>
#include <stdlib.h>

#include "hello.h"

int main()
{
    puts("Hello, world!");

    int value = hello();
    printf("Output value is: %d\n", value);

    return EXIT_SUCCESS;
}