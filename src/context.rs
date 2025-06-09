// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    definition::Definition, file_cache::FileCache, file_provider::FileProvider, prompt::Prompt,
};

pub struct Context<'a, T>
where
    T: FileProvider,
{
    pub file_cache: &'a mut FileCache,
    pub file_provider: &'a T,
    pub definitions: Definition,
    pub included_files: Vec<String>, // To prevent redundant inclusions
    pub prompts: Vec<Prompt>,        // For user-facing messages
    pub current_file_number: usize,  // Current file number for tracking
}
