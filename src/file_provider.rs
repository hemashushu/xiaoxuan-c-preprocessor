// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::path::{Path, PathBuf};

pub trait FileProvider {
    /// Resolves a user file path relative to the user header directories.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file(&self, header_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a relative file path based on the source file's canonical path.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_relative_file(
        &self,
        header_file_path: &Path,
        source_canonical_file_path: &Path,
    ) -> Option<PathBuf>;

    /// Resolves a system file path relative to the system header directories.
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_system_file(&self, header_file_path: &Path) -> Option<PathBuf>;

    /// Loads the content of a file given its canonical path.
    /// Returns the content as a `String` if successful, or an `std::io::Error` if an error occurs.
    fn load_file(&self, file_canonical_path: &Path) -> Result<String, std::io::Error>;
}
