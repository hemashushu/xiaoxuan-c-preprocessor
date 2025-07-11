// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>. All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::path::{Path, PathBuf};

use crate::ast::Program;

/// A cache for files, typically header files (`*.h`) that have been included.
/// Stores the canonical path, source code, and the parsed program for each file.
/// This cache prevents redundant parsing of the same file multiple times.
///
/// The souce code file (`*.c`) should not be cached.
pub struct HeaderFileCache {
    items: Vec<CacheItem>,
}

struct CacheItem {
    canonical_full_path: PathBuf,

    /// The source code of the file.
    /// It is used to generate the error message when parsing fails.
    text_content: String,

    program: Program,
}

impl HeaderFileCache {
    /// Creates a new, empty `FileCache`.
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Adds a file to the cache.
    ///
    /// - `canonical_full_path` - The canonical path of the file.
    /// - `text_content` - The source code of the file.
    ///
    /// Returns the file number assigned to this file.
    /// File number starts from 1 since 0 is reserved for predefined macros.
    pub fn add(&mut self, canonical_full_path: &Path, text_content: &str) -> usize {
        // File number starts from 1 since 0 is reserved for predefined macros.
        let file_number = self.items.len() + 1;

        let item = CacheItem {
            canonical_full_path: canonical_full_path.to_path_buf(),
            text_content: text_content.to_owned(),
            program: Program::default(),
        };

        self.items.push(item);

        file_number
    }

    /// Sets the program for a file in the cache.
    ///
    /// The `Program` is set after parsing the file instead of during the `add` method.
    /// This is because the `Program` is generated after parsing the file,
    /// and the parse operation may fail and raise an error, which contains
    /// the `file_number`, this number only exists after the file is `add` to the cache.
    pub fn set_program(&mut self, canonical_full_path: &Path, program: Program) {
        let file_number = self
            .items
            .iter()
            .position(|item| item.canonical_full_path == canonical_full_path)
            .unwrap();

        self.items[file_number].program = program;
    }

    pub fn get_file_number(&self, canonical_full_path: &Path) -> Option<usize> {
        self.items
            .iter()
            .position(|item| item.canonical_full_path == canonical_full_path)
            .map(|index| index + 1) // File number starts from 1
    }

    pub fn get_text_content(&self, canonical_full_path: &Path) -> Option<&String> {
        self.items
            .iter()
            .find(|item| item.canonical_full_path == canonical_full_path)
            .map(|item| &item.text_content)
    }

    pub fn get_program(&self, canonical_full_path: &Path) -> Option<&Program> {
        self.items
            .iter()
            .find(|item| item.canonical_full_path == canonical_full_path)
            .map(|item| &item.program)
    }

    /// Checks if a file with the given canonical path exists in the cache.
    pub fn contains(&self, canonical_full_path: &Path) -> bool {
        self.items
            .iter()
            .any(|item| item.canonical_full_path == canonical_full_path)
    }

    /// Returns a list of tuples containing the file number and canonical path for each cached file.
    ///
    /// The file number for the first item is 1, since 0 is reserved for predefined macros.
    /// For example, the first item will be `(1, "/path/to/file.h")`.
    pub fn get_cache_file_list(&self) -> Vec<(usize, PathBuf)> {
        self.items
            .iter()
            .enumerate()
            .map(|(index, item)| (index + 1, item.canonical_full_path.clone()))
            .collect()
    }
}

impl Default for HeaderFileCache {
    fn default() -> Self {
        Self::new()
    }
}