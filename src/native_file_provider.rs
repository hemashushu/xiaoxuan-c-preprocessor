// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    fs,
    io::{self, Read, Seek},
    path::{Path, PathBuf},
};

use crate::context::FileProvider;

pub struct NativeFileProvider {
    /// Directories to search for system headers.
    system_include_directories: Vec<PathBuf>,

    /// Directories to search for user headers and binary files.
    user_include_directories: Vec<PathBuf>,
}

impl NativeFileProvider {
    /// Creates a new `NativeFileProvider` with the specified directories for system and user headers.
    ///
    /// - `system_include_directories`: Directories to search for system and external module headers.
    ///   These are used when resolving `#include` and `embed` directives with angle brackets,
    ///   e.g., `#include <stdio.h>`.
    /// - `user_include_directories`: Directories to search for module-specific headers.
    ///   These are used when resolving `#include` and `embed` directives with double quotes,
    ///   e.g., `#include "header.h"`.
    pub fn new(
        user_include_directories: &[PathBuf],
        system_include_directories: &[PathBuf],
    ) -> Self {
        Self {
            system_include_directories: system_include_directories.to_vec(),
            user_include_directories: user_include_directories.to_vec(),
        }
    }
}

impl FileProvider for NativeFileProvider {
    fn resolve_user_file(&self, relative_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.user_include_directories {
            let full_path = dir.join(relative_file_path);
            if let Ok(canonical_full_path) = full_path.canonicalize() {
                // Check if the file exists and is a regular file
                if canonical_full_path.is_file() && canonical_full_path.exists() {
                    return Some(canonical_full_path);
                }
            }
        }
        None
    }

    fn resolve_user_file_relative_to_current_file(
        &self,
        relative_file_path: &Path,
        source_file_canonical_full_path: &Path,
    ) -> Option<PathBuf> {
        let source_file_directory = source_file_canonical_full_path.parent().unwrap();
        let full_path = source_file_directory.join(relative_file_path);
        if let Ok(canonical_full_path) = full_path.canonicalize() {
            // Check if the file exists and is a regular file
            if canonical_full_path.is_file() && canonical_full_path.exists() {
                return Some(canonical_full_path);
            }
        }

        None
    }

    fn resolve_system_file(&self, relative_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.system_include_directories {
            let full_path = dir.join(relative_file_path);
            if let Ok(canonical_full_path) = full_path.canonicalize() {
                // Check if the file exists and is a regular file
                if canonical_full_path.is_file() && canonical_full_path.exists() {
                    return Some(canonical_full_path);
                }
            }
        }
        None
    }

    fn load_text_file(&self, canonical_full_path: &Path) -> Result<String, io::Error> {
        // Read the file as text
        let content = fs::read_to_string(canonical_full_path)?;
        Ok(content)
    }

    fn load_binary_file(
        &self,
        canonical_full_path: &Path,
        offset: usize,

        // Optional maximum length to read from the file.
        // If `None`, reads the entire file.
        // If `Some(n)`, reads up to `n` bytes.
        // If the file is shorter than `n`, reads the entire file.
        max_length: Option<usize>,
    ) -> Result<Vec<u8>, io::Error> {
        // Open the file in read-only mode
        let mut file = fs::File::open(canonical_full_path)?;

        // Move the cursor to the specified offset
        file.seek(io::SeekFrom::Start(offset as u64))?;

        // Read the file into a buffer
        let mut buffer = Vec::new();
        if let Some(length) = max_length {
            buffer.resize(length, 0);

            let bytes_read = file.read(&mut buffer)?;
            buffer.truncate(bytes_read);
        } else {
            file.read_to_end(&mut buffer)?;
        }

        Ok(buffer)
    }

    fn file_size(&self, canonical_full_path: &Path) -> Result<usize, io::Error> {
        let metadata = fs::metadata(canonical_full_path)?;
        Ok(metadata.len() as usize)
    }
}

#[cfg(test)]
mod tests {
    use std::{
        env,
        path::{Path, PathBuf},
    };

    use crate::{
        context::{FileProvider, FilePathResolveResult},
        native_file_provider::NativeFileProvider,
    };

    /// Helpers to get the path to the test resources rootfs directory.
    fn get_rootfs_path() -> PathBuf {
        // `env::current_dir()` returns the current Rust project's root folder
        let mut dir = env::current_dir().unwrap();
        dir.push("tests");
        dir.push("resources");
        dir.push("rootfs");
        dir
    }

    #[test]
    fn test_native_file_provider() {
        let rootfs_path = get_rootfs_path();

        let project_root_path = rootfs_path.join("projects/hello");
        let user_include_dirs = vec![
            project_root_path.join("include"),
            project_root_path.join("src/header"),
            project_root_path.join("src/common"),
            project_root_path.join("src/resources"),
        ];

        let system_include_path = rootfs_path.join("usr/include");
        let system_include_dirs = vec![system_include_path];

        let provider = NativeFileProvider::new(&user_include_dirs, &system_include_dirs);

        let vrootfs =
            |file_path_str: &str| -> PathBuf { rootfs_path.join(file_path_str.trim_start_matches('/')) };

        // Test resolving user header files
        assert_eq!(
            provider.resolve_user_file(Path::new("hello.h")),
            Some(vrootfs("/projects/hello/include/hello.h"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("foo.h")),
            Some(vrootfs("/projects/hello/src/header/foo.h"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("bar.h")),
            Some(vrootfs("/projects/hello/src/header/bar.h"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("subfolder/buzz.h")),
            Some(vrootfs("/projects/hello/src/header/subfolder/buzz.h"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("hippo.dat")),
            Some(vrootfs("/projects/hello/src/resources/hippo.dat"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("config.h")),
            Some(vrootfs("/projects/hello/src/common/config.h"))
        );

        // Test resolving system header files
        assert_eq!(
            provider.resolve_system_file(Path::new("stdio.h")),
            Some(vrootfs("/usr/include/stdio.h"))
        );

        assert_eq!(
            provider.resolve_system_file(Path::new("stdlib.h")),
            Some(vrootfs("/usr/include/stdlib.h"))
        );

        // Test resolving user header files relative to a source file
        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("./bar.h"),
                &vrootfs("/projects/hello/src/header/foo.h")
            ),
            Some(vrootfs("/projects/hello/src/header/bar.h"))
        );

        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("subfolder/buzz.h"),
                &vrootfs("/projects/hello/src/header/foo.h")
            ),
            Some(vrootfs("/projects/hello/src/header/subfolder/buzz.h"))
        );

        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("../foo.h"),
                &vrootfs("/projects/hello/src/header/subfolder/buzz.h")
            ),
            Some(vrootfs("/projects/hello/src/header/foo.h"))
        );

        // Test resolving user header files with fallback (resolve to user header directory)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("foo.h"),
                &vrootfs("/projects/hello/src/main.c"),
                true
            ),
            Some(FilePathResolveResult::new(
                vrootfs("/projects/hello/src/header/foo.h"),
                false
            ))
        );

        // Test resolving user header files with fallback (resolve to system header directory)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("stdlib.h"),
                &vrootfs("/projects/hello/src/main.c"),
                true
            ),
            Some(FilePathResolveResult::new(vrootfs("/usr/include/stdlib.h"), true))
        );

        // Test resolving user header files with fallback (enable relative path)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("./lib.c"),
                &vrootfs("/projects/hello/src/main.c"),
                true
            ),
            Some(FilePathResolveResult::new(
                vrootfs("/projects/hello/src/lib.c"),
                false
            ))
        );

        // Test resolving user header files with fallback (disable relative path)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("./lib.c"),
                &vrootfs("/projects/hello/src/main.c"),
                false
            ),
            None
        );

        // Test resolving non-existent files
        assert_eq!(
            provider.resolve_user_file(Path::new("non_existent.h")),
            None
        );

        assert_eq!(
            provider.resolve_system_file(Path::new("non_existent.h")),
            None
        );

        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("non_existent.h"),
                &vrootfs("/projects/hello/src/main.c")
            ),
            None
        );

        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("non_existent.h"),
                &vrootfs("/projects/hello/src/main.c"),
                true
            ),
            None
        );
    }
}

