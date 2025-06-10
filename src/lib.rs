// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{fmt::Display, path::PathBuf};

use position::Position;
use range::Range;

use crate::{location::Location, prompt::Prompt, token::Token};

mod ast;
mod ast_printer;
mod char_with_position;
mod context;
mod definition;
mod lexer;
mod location;
mod memory_file_provider;
mod parser;
mod peekable_iter;
mod position;
mod range;
mod token;

pub mod error_printer;
pub mod file_cache;
pub mod file_provider;
pub mod native_file_provider;
pub mod preprocessor;
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

#[derive(Debug, PartialEq)]
pub enum PreprocessError {
    Message(String),
    UnexpectedEndOfDocument(String),
    MessageWithPosition(String, Position),
    MessageWithRange(String, Range),
}

impl Display for PreprocessError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unimplemented!()
    }
}

impl std::error::Error for PreprocessError {}
