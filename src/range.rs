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

    pub fn from_single_position(pos: &Position) -> Self {
        Self {
            start: *pos,
            end_included: *pos,
        }
    }
}
