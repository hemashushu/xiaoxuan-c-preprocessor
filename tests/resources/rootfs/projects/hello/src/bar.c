/**
 * Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

#include "bar.h"

// GCC searches resource paths configured with --embed-dir, not header paths
// configured with -I. For this fixture, add --embed-dir=resources when building
// from this directory.
char data[] = {
#embed "hippo.dat" limit(5)
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