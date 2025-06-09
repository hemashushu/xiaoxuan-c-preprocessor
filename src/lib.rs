// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use position::Position;
use range::Range;

use crate::{location::Location, token::Token};

mod ast;
mod ast_printer;
mod charwithposition;
mod context;
mod definition;
mod lexer;
mod location;
mod parser;
mod peekableiter;
mod position;
mod range;
mod token;

pub mod errorprinter;
pub mod file_cache;
pub mod file_provider;
pub mod processor;
pub mod prompt;

#[derive(Debug, PartialEq)]
pub struct TokenWithRange {
    pub token: Token,
    pub range: Range,
}

impl TokenWithRange {
    pub fn new(token: Token, range: Range) -> Self {
        Self { token, range }
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
}

impl TokenWithLocation {
    pub fn new(token: Token, location: Location) -> Self {
        Self { token, location }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
