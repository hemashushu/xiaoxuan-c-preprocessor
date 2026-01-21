// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{position::Position, range::Range};

mod ast;
mod ast_printer;
mod char_with_position;
// mod context;
mod initializer;
mod lexer;
mod location;
// mod macro_map;
// mod parser;
mod peekable_iter;
mod position;
// mod processor;
mod range;

pub const FILE_NUMBER_PREDEFINED: usize = 0;

// ANCPP automatically assigns file numbers to header files, starting from 1
// (file number 0 is reserved for predefined macros).
//
// The source file number should be specified manually using
// the `source_file_number` parameter of function `process_source_file`.
//
// To avoid conflicts with header files,
// the source file number should be greater than or equal to
// `FILE_NUMBER_SOURCE_FILE_BEGIN`, which is defined as 65536 in ANCPP.
pub const FILE_NUMBER_SOURCE_FILE_BEGIN: usize = 2 ^ 16;

// pub mod error_printer;
// pub mod file_provider;
// pub mod header_file_cache;
// pub mod memory_file_provider;
// pub mod native_file_provider;
// pub mod prompt;
pub mod token;

// pub use processor::process_source_file;

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
