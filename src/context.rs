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
    pub file_cache: &'a mut HeaderFileCache,

    /// Macro definitions and related state.
    pub macro_map: MacroMap,

    /// Whether to resolve relative file paths.
    /// For example, `#include "../../folder/header.h"` will be resolved to the actual file path
    /// using the current source or header file as the base directory.
    pub should_resolve_relative_path: bool,

    /// The file number currently being processed.
    pub current_file: ContextFile,

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
    #[allow(dead_code)]
    pub fn new(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        should_resolve_relative_path: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Self {
        Self {
            file_provider,
            file_cache,
            should_resolve_relative_path,
            current_file: ContextFile::new(
                current_file_number,
                IncludeFile::new(
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

    pub fn from_keyvalues(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        predefinitions: &HashMap<String, String>,
        should_resolve_relative_path: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessError> {
        Ok(Self {
            file_provider,
            file_cache,
            macro_map: MacroMap::from_key_values(predefinitions)?,
            should_resolve_relative_path,
            current_file: ContextFile::new(
                current_file_number,
                IncludeFile::new(
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

impl IncludeFile {
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
