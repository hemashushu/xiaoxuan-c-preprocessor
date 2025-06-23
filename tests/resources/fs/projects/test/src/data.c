/**
 * Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
 *
 * This Source Code Form is subject to the terms of
 * the Mozilla Public License version 2.0 and additional exceptions.
 * For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.
 */

#include "data.h"

char data[] = {
#embed "../resources/data.bin" if_empty(0x01, 0x02, 0x03)
};

int sum() {
    int total = 0;
    for(int i = 0; i < sizeof(data); i++) {
        total += data[i];
    }
    return total;
}