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
    PreprocessError, TokenWithLocation, file_provider::FileProvider,
    header_file_cache::HeaderFileCache, macro_map::MacroMap, prompt::Prompt,
};

/// The `Context` struct holds all state required during preprocessing.
/// It manages file access, macro definitions, file inclusion tracking, and user-facing messages.
pub struct Context<'a, T>
where
    T: FileProvider,
{
    /// Reference to the file provider used for file access.
    pub file_provider: &'a T,

    /// Mutable reference to the file cache.
    pub header_file_cache: &'a mut HeaderFileCache,

    pub reserved_identifiers: &'a [&'a str],

    /// Macro definitions and related state.
    pub macro_map: MacroMap,

    /// Whether to resolve relative file paths base on the current file.
    /// Set to false to only search in the specified including directories.
    pub resolve_relative_path_within_current_file: bool,

    /// The file number currently being processed.
    pub current_file_item: FileItem,

    /// List of included files to prevent redundant inclusions.
    pub included_files: Vec<FileLocation>,

    /// User-facing messages, warnings, or notifications.
    pub prompts: Vec<Prompt>,

    /// Output tokens generated during preprocessing.
    pub output: Vec<TokenWithLocation>,
}

impl<'a, T> Context<'a, T>
where
    T: FileProvider,
{
    #[allow(dead_code)]
    pub fn new(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        resolve_relative_path_within_current_file: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Self {
        Self {
            file_provider,
            header_file_cache: file_cache,
            reserved_identifiers,
            resolve_relative_path_within_current_file,
            current_file_item: FileItem::new(
                current_file_number,
                FileLocation::new(
                    current_file_canonical_full_path,
                    FileOrigin::from_source_file(current_file_relative_path),
                ),
            ),
            macro_map: MacroMap::new(),
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from_keyvalues(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        predefinitions: &HashMap<String, String>,
        resolve_relative_path_within_current_file: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessError> {
        Ok(Self {
            file_provider,
            header_file_cache: file_cache,
            reserved_identifiers,
            macro_map: MacroMap::from_key_values(predefinitions)?,
            resolve_relative_path_within_current_file,
            current_file_item: FileItem::new(
                current_file_number,
                FileLocation::new(
                    current_file_canonical_full_path,
                    FileOrigin::from_source_file(current_file_relative_path),
                ),
            ),
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        })
    }

    pub fn contains_include_file(&self, canonical_full_path: &Path) -> bool {
        self.included_files
            .iter()
            .any(|file| file.canonical_full_path == canonical_full_path)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FileItem {
    pub number: usize,
    pub file_location: FileLocation,
}

impl FileItem {
    pub fn new(number: usize, file_location: FileLocation) -> Self {
        Self {
            number,
            file_location,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FileLocation {
    pub canonical_full_path: PathBuf,
    pub file_origin: FileOrigin,
}

/// Represents the source of a file.
#[derive(Debug, PartialEq, Clone)]
pub enum FileOrigin {
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

impl FileLocation {
    pub fn new(canonical_full_path: &Path, file_origin: FileOrigin) -> Self {
        Self {
            canonical_full_path: canonical_full_path.to_path_buf(),
            file_origin,
        }
    }
}

impl FileOrigin {
    pub fn from_source_file(relative_file_path: &Path) -> Self {
        FileOrigin::SourceFile(relative_file_path.to_path_buf())
    }

    pub fn from_user_header_file(relative_file_path: &Path) -> Self {
        FileOrigin::UserHeader(relative_file_path.to_path_buf())
    }

    pub fn from_system_header_file(relative_file_path: &Path) -> Self {
        FileOrigin::SystemHeader(relative_file_path.to_path_buf())
    }
}
