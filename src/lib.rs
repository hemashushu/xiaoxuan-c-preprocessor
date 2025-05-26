// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use position::Position;
use range::Range;

mod charwithposition;
mod peekableiter;
mod position;
mod range;
mod token;
mod tokenize;

#[derive(Debug, PartialEq, Clone)]
pub enum PreprocessError {
    Message(String),
    UnexpectedEndOfDocument(String),

    // Note: The `index` field in `Position` may exceed the last valid index of the input string.
    // For example, if the text is `'a`, a "char incomplete" error would have an index of 2,
    // which is one past the end of the string.
    MessageWithPosition(String, Position),
    MessageWithRange(String, Range),
}

impl Display for PreprocessError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            PreprocessError::Message(msg) => f.write_str(msg),
            PreprocessError::UnexpectedEndOfDocument(message) => {
                writeln!(
                    f,
                    "Unexpectedly reached the end of the document: {}.",
                    message
                )
            }
            PreprocessError::MessageWithPosition(message, position) => {
                writeln!(
                    f,
                    "Error at line {}, column {}: {}.",
                    position.line + 1,
                    position.column + 1,
                    message
                )
            }
            PreprocessError::MessageWithRange(message, range) => {
                writeln!(
                    f,
                    "Error from line {}, column {} to line {}, column {}: {}.",
                    range.start.line + 1,
                    range.start.column + 1,
                    range.end_included.line + 1,
                    range.end_included.column + 1,
                    message
                )
            }
        }
    }
}

impl std::error::Error for PreprocessError {}
