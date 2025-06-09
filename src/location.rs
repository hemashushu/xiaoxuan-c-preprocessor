// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::range::Range;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Location {
    pub file_number: usize,
    pub range: Range,
}

impl Location {
    pub fn new(file_number: usize, range: &Range) -> Self {
        Self {
            file_number,
            range: *range,
        }
    }
}

impl Default for Location {
    fn default() -> Self {
        Self {
            file_number: 0,
            range: Range::default(),
        }
    }
}
