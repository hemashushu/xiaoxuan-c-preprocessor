// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    PreprocessError, TokenWithLocation, definition::Definition, file_cache::FileCache,
    file_provider::FileProvider, prompt::Prompt,
};

/// The `Context` struct holds all state required during preprocessing.
/// It manages file access, macro definitions, file inclusion tracking, and user-facing messages.
///
/// - `file_provider`: Provides access to files (e.g., headers) during preprocessing.
/// - `file_cache`: Caches parsed files to avoid redundant parsing.
/// - `definition`: Stores macro definitions and related state.
/// - `included_files`: Tracks included files to prevent multiple inclusions.
/// - `prompts`: Collects informational or warning messages for the user.
/// - `current_file_number`: Tracks the file number currently being processed.
pub struct Context<'a, T>
where
    T: FileProvider,
{
    /// The file number currently being processed.
    pub current_file_number: usize,

    /// The path of the file currently being processed.
    pub current_file_path: PathBuf,

    /// Reference to the file provider used for file access.
    pub file_provider: &'a T,

    /// Mutable reference to the file cache.
    pub file_cache: &'a mut FileCache,

    /// Macro definitions and related state.
    pub definition: Definition,

    /// List of included files to prevent redundant inclusions.
    pub included_files: Vec<usize>,

    /// User-facing messages, warnings, or notifications.
    pub prompts: Vec<Prompt>,

    /// Output tokens generated during preprocessing.
    pub tokens: Vec<TokenWithLocation>,
}

impl<'a, T> Context<'a, T>
where
    T: FileProvider,
{
    /// Creates a new `Context` with empty macro definitions and no included files.
    ///
    /// # Arguments
    /// * `file_provider` - Reference to the file provider.
    /// * `file_cache` - Mutable reference to the file cache.
    /// * `current_file_number` - The file number currently being processed.
    pub fn new(
        file_provider: &'a T,
        file_cache: &'a mut FileCache,
        current_file_number: usize,
        current_file_path: &Path,
    ) -> Self {
        Self {
            current_file_number,
            current_file_path: current_file_path.to_path_buf(),
            file_provider,
            file_cache,
            definition: Definition::new(),
            included_files: Vec::new(),
            prompts: Vec::new(),
            tokens: Vec::new(),
        }
    }

    /// Creates a new `Context` with predefined macro key-value pairs.
    ///
    /// # Arguments
    /// * `file_provider` - Reference to the file provider.
    /// * `file_cache` - Mutable reference to the file cache.
    /// * `current_file_number` - The file number currently being processed.
    /// * `predefinitions` - A map of macro names to their values.
    ///
    /// # Returns
    /// Returns a `Context` initialized with the provided macro definitions.
    pub fn from_keyvalues(
        current_file_number: usize,
        current_file_path: &Path,
        file_provider: &'a T,
        file_cache: &'a mut FileCache,
        predefinitions: &HashMap<String, String>,
    ) -> Result<Self, PreprocessError> {
        Ok(Self {
            current_file_number,
            current_file_path: current_file_path.to_path_buf(),
            file_provider,
            file_cache,
            definition: Definition::from_key_values(predefinitions)?,
            included_files: Vec::new(),
            prompts: Vec::new(),
            tokens: Vec::new(),
        })
    }
}
