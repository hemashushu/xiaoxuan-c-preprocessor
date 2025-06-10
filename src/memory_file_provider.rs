// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Component, Path, PathBuf},
};

use crate::file_provider::FileProvider;

/// A simple in-memory file provider that implements the `FileProvider` trait.
/// This provider is intended for unit testing only and does not interact with the real file system.
///
/// The in-memory file provider simulates a file system with the following structure:
/// - `/projects/test` — project root directory
/// - `/projects/test/src` — source directory
/// - `/projects/test/header` — internal header directory
/// - `/projects/test/include` — public header directory
/// - `/usr/include` — system header directory
pub struct MemoryFileProvider {
    /// Maps canonical file paths to their contents.
    files: HashMap<PathBuf, String>,

    /// Directories to search for system headers.
    system_headers_directories: Vec<PathBuf>,

    /// Directories to search for user headers.
    user_headers_directories: Vec<PathBuf>,
}

impl MemoryFileProvider {
    /// Creates a new `MemoryFileProvider` with the specified directories for system and user headers.
    pub fn new() -> Self {
        let user_headers_directories: Vec<PathBuf> = vec![
            PathBuf::from("/projects/test/header"),
            PathBuf::from("/projects/test/include"),
        ];

        let system_headers_directories: Vec<PathBuf> = vec![PathBuf::from("/usr/include")];

        Self {
            files: HashMap::new(),
            system_headers_directories,
            user_headers_directories,
        }
    }

    /// Adds a file to the in-memory file provider.
    pub fn add_user_file(&mut self, path_relative_to_project_root: &Path, content: &str) {
        let path = Path::new("/projects/test").join(path_relative_to_project_root);
        let canonical_path = normalize_path(&path);
        self.files.insert(canonical_path, content.to_owned());
    }

    pub fn add_system_file(
        &mut self,
        path_relative_to_system_include_directory: &Path,
        content: &str,
    ) {
        let path = Path::new("/usr/include").join(path_relative_to_system_include_directory);
        let canonical_path = normalize_path(&path);
        self.files.insert(canonical_path, content.to_owned());
    }
}

impl FileProvider for MemoryFileProvider {
    fn resolve_user_file(&self, header_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.user_headers_directories {
            let full_path = dir.join(header_file_path);
            let canonical_full_path = normalize_path(&full_path);

            if self.files.contains_key(&canonical_full_path) {
                return Some(canonical_full_path);
            }
        }
        None
    }

    fn resolve_relative_file(
        &self,
        header_file_path: &Path,
        source_canonical_file_path: &Path,
    ) -> Option<PathBuf> {
        let source_file_directory = source_canonical_file_path.parent().unwrap();
        let full_path = source_file_directory.join(header_file_path);
        let canonical_full_path = normalize_path(&full_path);

        if self.files.contains_key(&canonical_full_path) {
            Some(canonical_full_path)
        } else {
            None
        }
    }

    fn resolve_system_file(&self, header_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.system_headers_directories {
            let full_path = dir.join(header_file_path);
            let canonical_full_path = normalize_path(&full_path);

            if self.files.contains_key(&canonical_full_path) {
                return Some(canonical_full_path);
            }
        }
        None
    }

    fn load_file(&self, file_canonical_path: &Path) -> Result<String, std::io::Error> {
        self.files
            .get(file_canonical_path)
            .cloned()
            .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "File not found"))
    }
}

/// Canonicalizes a given path without checking the real file system.
///
/// This function normalizes the path by resolving components like `.` and `..`,
/// but does not resolve symbolic links or check if the path exists on the file system.
/// Returns a new `PathBuf` with the normalized path.
pub fn normalize_path(path: &Path) -> PathBuf {
    let mut ret = PathBuf::new();

    let components = path.components();
    for component in components {
        match component {
            Component::Prefix(..) => unimplemented!(),
            Component::RootDir => {
                ret.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                ret.pop();
            }
            Component::Normal(c) => {
                ret.push(c);
            }
        }
    }
    ret
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use pretty_assertions::assert_eq;

    use crate::{file_provider::FileProvider, memory_file_provider::MemoryFileProvider};

    #[test]
    fn test_memory_file_provider() {
        let mut provider = MemoryFileProvider::new();
        provider.add_user_file(Path::new("src/main.c"), "SRC_MAIN_C");
        provider.add_user_file(Path::new("include/hello.h"), "INCLUDE_HELLO_H");
        provider.add_user_file(Path::new("header/foo.h"), "HEADER_FOO_H");
        provider.add_user_file(Path::new("header/bar.h"), "HEADER_BAR_H");
        provider.add_user_file(Path::new("header/folder/buz.h"), "HEADER_FOLDER_BUZ_H");
        provider.add_system_file(Path::new("stdio.h"), "STDIO_H");
        provider.add_system_file(Path::new("elf.h"), "ELF_H");

        // Test loading files
        assert_eq!(
            provider
                .load_file(&PathBuf::from("/projects/test/src/main.c"))
                .unwrap(),
            "SRC_MAIN_C"
        );

        assert_eq!(
            provider
                .load_file(&PathBuf::from("/projects/test/include/hello.h"))
                .unwrap(),
            "INCLUDE_HELLO_H"
        );
        assert_eq!(
            provider
                .load_file(&PathBuf::from("/projects/test/header/foo.h"))
                .unwrap(),
            "HEADER_FOO_H"
        );
        assert_eq!(
            provider
                .load_file(&PathBuf::from("/projects/test/header/folder/buz.h"))
                .unwrap(),
            "HEADER_FOLDER_BUZ_H"
        );
        assert_eq!(
            provider
                .load_file(&PathBuf::from("/usr/include/stdio.h"))
                .unwrap(),
            "STDIO_H"
        );

        // Test user file resolution
        assert_eq!(
            provider.resolve_user_file(Path::new("hello.h")),
            Some(PathBuf::from("/projects/test/include/hello.h"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("foo.h")),
            Some(PathBuf::from("/projects/test/header/foo.h"))
        );

        assert_eq!(
            provider.resolve_user_file(Path::new("folder/buz.h")),
            Some(PathBuf::from("/projects/test/header/folder/buz.h"))
        );

        // Test system file resolution
        assert_eq!(
            provider.resolve_system_file(Path::new("stdio.h")),
            Some(PathBuf::from("/usr/include/stdio.h"))
        );

        // Test relative file resolution
        assert_eq!(
            provider.resolve_relative_file(
                Path::new("./bar.h"),
                Path::new("/projects/test/header/foo.h")
            ),
            Some(PathBuf::from("/projects/test/header/bar.h"))
        );

        assert_eq!(
            provider.resolve_relative_file(
                Path::new("folder/buz.h"),
                Path::new("/projects/test/header/foo.h")
            ),
            Some(PathBuf::from("/projects/test/header/folder/buz.h"))
        );

        assert_eq!(
            provider.resolve_relative_file(
                Path::new("../foo.h"),
                Path::new("/projects/test/header/folder/buz.h")
            ),
            Some(PathBuf::from("/projects/test/header/foo.h"))
        );

        // Test user file resolution with fallback
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("elf.h"),
                Path::new("/projects/test/src/main.c"),
                true
            ),
            Some(PathBuf::from("/usr/include/elf.h"))
        );

        // Test non-existent file resolution
        assert_eq!(
            provider.resolve_user_file(Path::new("non_existent.h")),
            None
        );

        assert_eq!(
            provider.resolve_system_file(Path::new("non_existent.h")),
            None
        );

        assert_eq!(
            provider.resolve_relative_file(
                Path::new("non_existent.h"),
                Path::new("/projects/test/src/main.c")
            ),
            None
        );

        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("non_existent.h"),
                Path::new("/projects/test/src/main.c"),
                true
            ),
            None
        );
    }
}
