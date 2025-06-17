// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::path::{Path, PathBuf};

pub struct NativeFileProvider {
    /// The root directory of the project.
    project_root_directory: PathBuf,

    /// Directories to search for system headers.
    system_directories: Vec<PathBuf>,

    /// Directories to search for user headers and binary files.
    user_directories: Vec<PathBuf>,
}

impl NativeFileProvider {
    /// Creates a new `NativeFileProvider` with the specified directories for system and user headers.
    ///
    /// - `system_headers_directories`: Directories to search for system and external module headers.
    ///   These are used when resolving `#include` directives with angle brackets,
    ///   e.g., `#include <stdio.h>`.
    /// - `user_headers_directories`: Directories to search for module-specific headers.
    ///   These are used when resolving `#include` directives with double quotes,
    ///   e.g., `#include "relative/path/to/header.h"`.
    pub fn new(
        project_root_directory: &Path,
        user_directories: &[&Path],
        system_directories: &[&Path],
    ) -> Self {
        let user_directories_owned = user_directories.iter().map(|&p| PathBuf::from(p)).collect();

        let system_directories_owned = system_directories
            .iter()
            .map(|&p| PathBuf::from(p))
            .collect();

        Self {
            project_root_directory: PathBuf::from(project_root_directory),
            system_directories: system_directories_owned,
            user_directories: user_directories_owned,
        }
    }
}
