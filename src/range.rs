// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::position::Position;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Range {
    pub start: Position,
    pub end_included: Position,
}

impl Range {
    pub fn new(start: &Position, end_included: &Position) -> Self {
        Self {
            start: *start,
            end_included: *end_included,
        }
    }

    pub fn from_position(pos: &Position) -> Self {
        Self {
            start: *pos,
            end_included: *pos,
        }
    }

    pub fn from_position_and_length(pos: &Position, length: usize) -> Self {
        Self {
            start: *pos,
            end_included: Position::new(pos.index + length - 1, pos.line, pos.column + length - 1),
        }
    }

    pub fn from_detail_and_length(index: usize, line: usize, column: usize, length: usize) -> Self {
        let start = Position::new(index, line, column);
        let end_included = Position::new(index + length - 1, line, column + length - 1);
        Self {
            start,
            end_included,
        }
    }
}
