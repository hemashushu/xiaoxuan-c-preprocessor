// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::path::{Path, PathBuf};

pub struct NativeFileProvider {
    /// Directories to search for system headers.
    system_headers_directories: Vec<PathBuf>,

    /// Directories to search for user headers.
    user_headers_directories: Vec<PathBuf>,
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
    pub fn new(user_headers_directories: &[&Path], system_headers_directories: &[&Path]) -> Self {
        let user_headers_directories_owned = user_headers_directories
            .iter()
            .map(|&p| PathBuf::from(p))
            .collect();

        let system_headers_directories_owned = system_headers_directories
            .iter()
            .map(|&p| PathBuf::from(p))
            .collect();

        Self {
            system_headers_directories: system_headers_directories_owned,
            user_headers_directories: user_headers_directories_owned,
        }
    }
}
