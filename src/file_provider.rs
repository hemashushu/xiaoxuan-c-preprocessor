// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::path::{Component, Path, PathBuf};

pub trait FileProvider {
    /// Resolves a user header file path relative to the user header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file(&self, relative_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a user header file path relative to a source file's directory.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file_relative_to_source_file(
        &self,
        relative_file_path: &Path,
        source_file_canonical_full_path: &Path,
    ) -> Option<PathBuf>;

    /// Resolves a system header file path relative to the system header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_system_file(&self, relative_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a user header file path using a fallback strategy.
    /// This mimics the behavior of the quoted include directive in C, e.g., `#include "header.h"`.
    ///
    /// The resolution process is as follows:
    /// 1. If `resolve_relative` is true, attempts to resolve the header file relative to the source file's directory.
    /// 2. If not found, attempts to resolve the file in the user header search directories.
    /// 3. If still not found, attempts to resolve the file in the system header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file_with_fallback(
        &self,
        relative_file_path: &Path,
        source_file_canonical_full_path: &Path,
        resolve_relative: bool,
    ) -> Option<ResolvedResult> {
        if resolve_relative {
            if let Some(resolved_path) = self.resolve_user_file_relative_to_source_file(
                relative_file_path,
                source_file_canonical_full_path,
            ) {
                return Some(ResolvedResult::new(resolved_path, false));
            }
        }

        if let Some(resolved_path) = self.resolve_user_file(relative_file_path) {
            return Some(ResolvedResult::new(resolved_path, false));
        }

        if let Some(resolved_path) = self.resolve_system_file(relative_file_path) {
            return Some(ResolvedResult::new(resolved_path, true));
        }

        None
    }

    /// Loads the contents of a file given its canonical path.
    /// Returns the file contents as a `String` if successful, or an `std::io::Error` if an error occurs.
    fn load_text_file(&self, canonical_full_path: &Path) -> Result<String, std::io::Error>;

    /// Loads the contents of a binary file given its canonical path.
    /// Returns the file contents as a `Vec<u8>` if successful, or an `std::io::Error` if an error occurs.
    fn load_binary_file(
        &self,
        canonical_full_path: &Path,
        offset: usize,

        // Optional maximum length to read from the file.
        // If `None`, reads the entire file.
        // If `Some(n)`, reads up to `n` bytes.
        // If the file is shorter than `n`, reads the entire file.
        max_length: Option<usize>,
    ) -> Result<Vec<u8>, std::io::Error>;

    /// Resolves a source file path relative to the project's root directory.
    /// Returns the canonical path if found, or an `std::io::Error` if an error occurs.
    fn resolve_source_file(&self, relative_file_path: &Path) -> Result<PathBuf, std::io::Error>;

    fn file_size(&self, canonical_full_path: &Path) -> Result<usize, std::io::Error>;
}

#[derive(Debug, PartialEq)]
pub struct ResolvedResult {
    /// The canonical path of the resolved file.
    pub canonical_full_path: PathBuf,

    /// Indicates whether the resolved file is a system header file.
    pub is_system_file: bool,
}

impl ResolvedResult {
    pub fn new(canonical_full_path: PathBuf, is_system_file: bool) -> Self {
        Self {
            canonical_full_path,
            is_system_file,
        }
    }
}

/// Canonicalizes a given path without checking the real file system.
///
/// This function normalizes the path by resolving components like `.` and `..`,
/// but does not resolve symbolic links or check if the path exists on the file system.
/// Returns a new `PathBuf` with the normalized path.
pub fn normalize_path(src: &Path) -> PathBuf {
    let mut output = PathBuf::new();

    let components = src.components();
    for component in components {
        match component {
            Component::Prefix(..) => unimplemented!("todo: handle prefix components"),
            Component::RootDir => {
                output.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                output.pop();
            }
            Component::Normal(c) => {
                output.push(c);
            }
        }
    }
    output
}
