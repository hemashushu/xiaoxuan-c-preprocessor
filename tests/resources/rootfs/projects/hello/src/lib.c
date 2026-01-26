/**
 * Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

#include "hello.h"

#include "foo.h"
#include "bar.h"

int hello()
{
    // 42 + 55 = 97
    return foo() + bar();
}