/**
 * Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

// You can compile this code with a C compiler that supports the C23 standard.
// `gcc -Wall -g -std=c23 -o main.elf -I ../header -I ../include -I ../resources -I common main.c lib.c data.c`

#include "app.h"
#include "data.h"
#include "test.h"

int main() {
    puts("Hello, world!");

    int result = add(1, 2);
    printf("1 + 2 = %d\n", result);

    int total = sum();
    printf("Sum of data: %d\n", total);

    return EXIT_SUCCESS;
}