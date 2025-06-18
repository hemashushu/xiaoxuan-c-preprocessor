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
    PreprocessError, TokenWithLocation, header_file_cache::HeaderFileCache, file_provider::FileProvider,
    macro_map::MacroMap, prompt::Prompt,
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
    pub project_root_directory: PathBuf,

    pub resolve_relative_file: bool,

    pub current_file: ContextFile,

    /// Reference to the file provider used for file access.
    pub file_provider: &'a T,

    /// Mutable reference to the file cache.
    pub file_cache: &'a mut HeaderFileCache,

    /// Macro definitions and related state.
    pub macro_map: MacroMap,

    /// List of included files to prevent redundant inclusions.
    pub included_files: Vec<IncludeFile>,

    /// User-facing messages, warnings, or notifications.
    pub prompts: Vec<Prompt>,

    /// Output tokens generated during preprocessing.
    pub output: Vec<TokenWithLocation>,
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
    /// * `project_root_directory` - The root directory of the project.
    /// * `current_file_number` - The file number currently being processed.
    /// * `current_file_canonical_full_path` - The canonical full path of the current file.
    /// * `current_file_relative_path` - The relative path of the current file. (relative to the project root directory)
    pub fn new(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        project_root_directory: &Path,
        resolve_relative_file: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Self {
        Self {
            file_provider,
            file_cache,
            project_root_directory: project_root_directory.to_path_buf(),
            resolve_relative_file,
            current_file: ContextFile::new(
                current_file_number,
                IncludeFile::new(
                    current_file_canonical_full_path,
                    FileSource::from_source_file(current_file_relative_path),
                ),
            ),
            macro_map: MacroMap::new(),
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        }
    }

    /// Creates a new `Context` with predefined macro key-value pairs.
    ///
    /// # Arguments
    /// * `file_provider` - Reference to the file provider.
    /// * `file_cache` - Mutable reference to the file cache.
    /// * `predefinitions` - A map of macro names to their values.
    /// * `project_root_directory` - The root directory of the project.
    /// * `current_file_number` - The file number currently being processed.
    /// * `current_file_canonical_full_path` - The canonical full path of the current file.
    /// * `current_file_relative_path` - The relative path of the current file. (relative to the project root directory)
    ///
    /// # Returns
    /// Returns a `Context` initialized with the provided macro definitions.
    pub fn from_keyvalues(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        predefinitions: &HashMap<String, String>,
        project_root_directory: &Path,
        resolve_relative_file: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessError> {
        Ok(Self {
            file_provider,
            file_cache,
            macro_map: MacroMap::from_key_values(predefinitions)?,
            project_root_directory: project_root_directory.to_path_buf(),
            resolve_relative_file,
            current_file: ContextFile::new(
                current_file_number,
                IncludeFile::new(
                    current_file_canonical_full_path,
                    FileSource::from_source_file(current_file_relative_path),
                ),
            ),
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        })
    }

    pub fn contains_include_file(
        &self,
        canonical_full_path: &Path,
    ) -> bool {
        self.included_files
            .iter()
            .any(|file| file.canonical_full_path == canonical_full_path)
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct ContextFile {
    pub number: usize,
    pub source_file: IncludeFile,
}

impl ContextFile {
    pub fn new(number: usize, source_file: IncludeFile) -> Self {
        Self {
            number,
            source_file,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IncludeFile {
    pub canonical_full_path: PathBuf,
    pub file_source: FileSource,
}

/// Represents the source of a file.
#[derive(Debug, PartialEq, Clone)]
pub enum FileSource {
    /// Represents a file that is part of the source code, such as `main.c` and `lib.c`.
    /// The `relative_file_path` is the path relative to the project root directory.
    SourceFile(/* relative_file_path */ PathBuf),

    /// Represents a user header file, which is typically a header file included by the user.
    /// For example, `#include "my_header.h"`.
    /// The `relative_file_path` is the path relative to the user header directory.
    UserHeader(/* relative_file_path */ PathBuf),

    /// Represents a system header file, which is typically a standard library header or a system-provided header.
    /// For example, `#include <stdio.h>`.
    /// The `external_file_path` is the path relative to the system header directory.
    SystemHeader(/* external_file_path */ PathBuf),
}

impl IncludeFile {
    pub fn new(canonical_full_path: &Path, file_source: FileSource) -> Self {
        Self {
            canonical_full_path: canonical_full_path.to_path_buf(),
            file_source,
        }
    }
}

impl FileSource {
    pub fn from_source_file(relative_file_path: &Path) -> Self {
        FileSource::SourceFile(relative_file_path.to_path_buf())
    }

    pub fn from_header_file(relative_file_path: &Path, is_system_header: bool) -> Self {
        if is_system_header {
            FileSource::SystemHeader(relative_file_path.to_path_buf())
        } else {
            FileSource::UserHeader(relative_file_path.to_path_buf())
        }
    }
}