// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::path::{Path, PathBuf};

pub trait FileProvider {
    /// Resolves a user header file path relative to the user header search directories.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file(&self, header_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a header file path relative to the directory of the source file.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_relative_file(
        &self,
        header_file_path: &Path,
        source_canonical_file_path: &Path,
    ) -> Option<PathBuf>;

    /// Resolves a system header file path relative to the system header search directories.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_system_file(&self, header_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a user header file path using a fallback strategy.
    /// This mimics the behavior of the quoted include directive in C, e.g., `#include "header.h"`.
    ///
    /// The resolution process is as follows:
    /// 1. If `resolve_relative` is true, attempts to resolve the header file relative to the source file's directory.
    /// 2. If not found, attempts to resolve the file in the user header search directories.
    /// 3. If still not found, attempts to resolve the file in the system header search directories.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file_with_fallback(
        &self,
        header_file_path: &Path,
        source_canonical_file_path: &Path,
        resolve_relative: bool,
    ) -> Option<PathBuf> {
        if resolve_relative {
            if let Some(resolved_path) =
                self.resolve_relative_file(header_file_path, source_canonical_file_path)
            {
                return Some(resolved_path);
            }
        }

        if let Some(resolved_path) = self.resolve_user_file(header_file_path) {
            return Some(resolved_path);
        }

        if let Some(resolved_path) = self.resolve_system_file(header_file_path) {
            return Some(resolved_path);
        }

        None
    }

    /// Loads the contents of a file given its canonical path.
    /// Returns the file contents as a `String` if successful, or an `std::io::Error` if an error occurs.
    fn load_file(&self, file_canonical_path: &Path) -> Result<String, std::io::Error>;
}
