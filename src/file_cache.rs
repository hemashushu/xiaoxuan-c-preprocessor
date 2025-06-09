// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>. All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{PreprocessError, ast::Program, parser::parse_from_str};

/// A cache for files, typically header files (`*.h`) that have been included.
/// Stores the canonical path, source code, and the parsed program for each file.
/// This cache prevents redundant parsing of the same file multiple times.
///
/// The souce code file (`*.c`) should not be cached.
pub struct FileCache {
    items: Vec<FileCacheItem>,
}

struct FileCacheItem {
    canonical_path: String,
    source: String,
    program: Program,
}

impl FileCache {
    /// Creates a new, empty `FileCache`.
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Adds a file to the cache.
    ///
    /// # Arguments
    /// * `canonical_path` - The canonical path of the file.
    /// * `source` - The source code of the file.
    ///
    /// # Returns
    /// Returns the file number assigned to this file.
    /// File number 0 is reserved for predefined macros,
    /// so the first file added will have file number 1.
    pub fn add(&mut self, canonical_path: &str, source: &str) -> Result<usize, PreprocessError> {
        let program = parse_from_str(source)?;
        let item = FileCacheItem {
            canonical_path: canonical_path.to_owned(),
            source: source.to_owned(),
            program,
        };
        self.items.push(item);

        // File number 0 is reserved for predefined macros,
        // so returns the length of items as the file number.
        Ok(self.items.len())
    }

    /// Retrieves a reference to a cached file by its canonical path.
    pub fn get(&self, canonical_path: &str) -> Option<&FileCacheItem> {
        self.items
            .iter()
            .find(|item| item.canonical_path == canonical_path)
    }

    /// Checks if a file with the given canonical path exists in the cache.
    pub fn contains(&self, canonical_path: &str) -> bool {
        self.items
            .iter()
            .any(|item| item.canonical_path == canonical_path)
    }

    /// Returns a list of tuples containing the file number and canonical path for each cached file.
    ///
    /// The file number for the first item is 1, since 0 is reserved for predefined macros.
    /// For example, the first item will be `(1, "/path/to/file.h")`.
    pub fn get_cache_list(&self) -> Vec<(usize, String)> {
        self.items
            .iter()
            .enumerate()
            .map(|(index, item)| (index + 1, item.canonical_path.clone()))
            .collect()
    }
}
