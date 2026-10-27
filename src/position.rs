// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

#[derive(Debug, PartialEq, Clone, Copy, Default)]
pub struct Position {
    pub index: usize,  // Zero-based Unicode scalar index, not a byte offset.
    pub line: usize,   // The line number in the source (starting from 0).
    pub column: usize, // Zero-based character column, not a visual display width.
}

impl Position {
    /// Creates a position from a zero-based character index, line, and column.
    pub fn new(index: usize, line: usize, column: usize) -> Self {
        Self {
            index,
            line,
            column,
        }
    }
}
