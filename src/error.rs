// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{position::Position, range::Range};

#[derive(Debug, PartialEq)]
pub enum PreprocessError {
    Message(String),
    UnexpectedEndOfDocument(String),
    MessageWithPosition(String, Position),
    MessageWithRange(String, Range),
}

impl Display for PreprocessError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PreprocessError::Message(msg) => f.write_str(msg),
            PreprocessError::UnexpectedEndOfDocument(detail) => {
                writeln!(f, "Unexpected to reach the end of document.")?;
                write!(f, "{}", detail)
            }
            PreprocessError::MessageWithPosition(detail, position) => {
                writeln!(
                    f,
                    "Error at line: {} column: {}",
                    position.line + 1,
                    position.column + 1
                )?;
                write!(f, "{}", detail)
            }
            PreprocessError::MessageWithRange(detail, range) => {
                writeln!(
                    f,
                    "Error from line: {} column: {}, to line: {} column: {}",
                    range.start.line + 1,
                    range.start.column + 1,
                    range.end_included.line + 1,
                    range.end_included.column + 1
                )?;
                write!(f, "{}", detail)
            }
        }
    }
}

impl std::error::Error for PreprocessError {}

#[derive(Debug, PartialEq)]
pub struct PreprocessFileError {
    pub file_number: usize,
    pub error: PreprocessError,
}

impl PreprocessFileError {
    pub fn new(file_number: usize, error: PreprocessError) -> Self {
        Self { file_number, error }
    }
}

impl Display for PreprocessFileError {
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unimplemented!()
    }
}

impl std::error::Error for PreprocessFileError {}
