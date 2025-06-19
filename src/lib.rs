// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{location::Location, position::Position, range::Range, token::Token};

mod ast;
mod ast_printer;
mod char_with_position;
mod context;
mod expression;
mod lexer;
mod location;
mod macro_map;
mod parser;
mod peekable_iter;
mod position;
mod processor;
mod range;
mod token;

pub const FILE_NUMBER_PREDEFINED: usize = 0;
pub const FILE_NUMBER_SOURCE_FILE_BEGIN: usize = 2_usize.pow(16);

pub mod error_printer;
pub mod file_provider;
pub mod header_file_cache;
pub mod memory_file_provider;
// pub mod native_file_provider;
pub mod prompt;

pub use processor::process_source_file;

#[derive(Debug, PartialEq, Clone)]
pub struct TokenWithRange {
    pub token: Token,
    pub range: Range,
}

impl TokenWithRange {
    pub fn new(token: Token, range: Range) -> Self {
        Self { token, range }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
    fn fmt(&self, _f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unimplemented!()
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
