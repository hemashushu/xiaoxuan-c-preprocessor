// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{position::Position, range::Range};

mod char_with_position;
mod initializer;
mod lexer;
mod location;
mod parser;
mod peekable_iter;
mod position;
// mod processor;
mod range;

pub mod ast;
pub mod ast_printer;
// pub mod context;
pub mod error_printer;
// pub mod memory_file_provider;
// pub mod native_file_provider;
// pub mod prompt;
pub mod error;
pub mod token;

// pub use processor::process_source_file;

// ANCPP reserves file number 0 for predefined macros.
pub const FILE_NUMBER_PREDEFINED: usize = 0;

// ANCPP automatically assigns file numbers to **header files**, starting from 1
// (file number 0 is reserved for predefined macros).
//
// However, the number of **source files** are specified manually using
// the `source_file_number` parameter of function `process_source_file`.
//
// To avoid conflicts with header files, this file number should be greater than
// or equal to `FILE_NUMBER_SOURCE_FILE_BEGIN`, which is defined as 65536.
pub const FILE_NUMBER_SOURCE_FILE_BEGIN: usize = 2 ^ 16;
