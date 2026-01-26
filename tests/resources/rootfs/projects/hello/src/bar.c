/**
 * Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

#include "bar.h"

// In GCC, the file path is resolved relative to the source file location,
// effectively ignoring user include paths specified with the `-I ...` option.
char data[] = {
#embed "resources/hippo.png"
};

int bar()
{
    int total = 0;
    for (int i = 0; i < sizeof(data); i++)
    {
        total += data[i];
    }
    return total;
}