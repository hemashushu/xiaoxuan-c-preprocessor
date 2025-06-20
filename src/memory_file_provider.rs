// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::file_provider::{FileProvider, normalize_path};

/// A simple in-memory file provider that implements the `FileProvider` trait.
/// This provider is intended for unit testing only and does not interact with the real file system.
///
/// The in-memory file provider simulates a file system with the following structure:
/// - `/projects/test` — project root directory
/// - `/projects/test/src` — source directory
/// - `/projects/test/src/common` — internal shared header such as project's configuration
/// - `/projects/test/include` — public header directory
/// - `/projects/test/header` — internal header directory
/// - `/projects/test/share` — binary files directory
/// - `/usr/include` — system header directory
pub struct MemoryFileProvider {
    /// Maps canonical file paths to their contents.
    file_content_map: HashMap<PathBuf, FileContent>,

    /// Directories to search for system headers.
    system_directories: Vec<PathBuf>,

    /// Directories to search for user headers.
    user_directories: Vec<PathBuf>,
}

enum FileContent {
    Text(String),
    Binary(Vec<u8>),
}

impl MemoryFileProvider {
    pub fn new() -> Self {
        let user_directories: Vec<PathBuf> = vec![
            PathBuf::from("/projects/test/header"),     // internal headers
            PathBuf::from("/projects/test/include"),    // public headers
            PathBuf::from("/projects/test/share"),      // binary files
            PathBuf::from("/projects/test/src/common"), // internal shared headers
        ];

        let system_directories: Vec<PathBuf> = vec![PathBuf::from("/usr/include")];

        Self {
            file_content_map: HashMap::new(),
            system_directories,
            user_directories,
        }
    }

    /// Adds a file to the in-memory file provider.
    ///
    /// `source_file_relative_path` is the path relative to the project root directory,
    pub fn add_user_text_file(&mut self, source_file_relative_path: &Path, content: &str) {
        let path = Path::new("/projects/test").join(source_file_relative_path);
        let normalized_path = normalize_path(&path);
        self.file_content_map
            .insert(normalized_path, FileContent::Text(content.to_owned()));
    }

    /// Adds a binary file to the in-memory file provider.
    ///
    /// `source_file_relative_path` is the path relative to the project root directory.
    pub fn add_user_binary_file(&mut self, source_file_relative_path: &Path, content: Vec<u8>) {
        let path = Path::new("/projects/test").join(source_file_relative_path);
        let normalized_path = normalize_path(&path);
        self.file_content_map
            .insert(normalized_path, FileContent::Binary(content));
    }

    /// Adds a system file to the in-memory file provider.
    ///
    /// `source_file_relative_path` is the path relative to the system headers directory (e.g. `/usr/include`).
    pub fn add_system_file(&mut self, source_file_relative_path: &Path, content: &str) {
        let path = Path::new("/usr/include").join(source_file_relative_path);
        let normalized_path = normalize_path(&path);
        self.file_content_map
            .insert(normalized_path, FileContent::Text(content.to_owned()));
    }
}

impl Default for MemoryFileProvider {
    fn default() -> Self {
        Self::new()
    }
}

impl FileProvider for MemoryFileProvider {
    fn resolve_user_file(&self, header_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.user_directories {
            let full_path = dir.join(header_file_path);
            let canonical_full_path = normalize_path(&full_path);

            if self.file_content_map.contains_key(&canonical_full_path) {
                return Some(canonical_full_path);
            }
        }
        None
    }

    fn resolve_system_file(&self, header_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.system_directories {
            let full_path = dir.join(header_file_path);
            let canonical_full_path = normalize_path(&full_path);

            if self.file_content_map.contains_key(&canonical_full_path) {
                return Some(canonical_full_path);
            }
        }
        None
    }

    fn resolve_user_file_relative_to_current_file(
        &self,
        header_file_path: &Path,
        source_file_normalize_full_path: &Path,
    ) -> Option<PathBuf> {
        let source_file_directory = source_file_normalize_full_path.parent().unwrap();
        let full_path = source_file_directory.join(header_file_path);
        let canonical_full_path = normalize_path(&full_path);

        if self.file_content_map.contains_key(&canonical_full_path) {
            Some(canonical_full_path)
        } else {
            None
        }
    }

//     fn resolve_source_file(&self, relative_file_path: &Path) -> Result<PathBuf, std::io::Error> {
//         let full_path = self.project_root_directory.join(relative_file_path);
//         let canonical_full_path = normalize_path(&full_path);
//
//         if self.file_content_map.contains_key(&canonical_full_path) {
//             Ok(canonical_full_path)
//         } else {
//             Err(std::io::Error::new(
//                 std::io::ErrorKind::NotFound,
//                 "File not found",
//             ))
//         }
//     }

    fn load_text_file(&self, canonical_full_path: &Path) -> Result<String, std::io::Error> {
        let file_content = self.file_content_map.get(canonical_full_path);

        match file_content {
            Some(FileContent::Text(content)) => Ok(content.clone()),
            Some(FileContent::Binary(_)) => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "File is not a text file",
            )),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "File not found",
            )),
        }
    }

    fn load_binary_file(
        &self,
        canonical_full_path: &Path,
        offset: usize,
        max_length: Option<usize>,
    ) -> Result<Vec<u8>, std::io::Error> {
        let file_content = self.file_content_map.get(canonical_full_path);

        match file_content {
            Some(FileContent::Binary(content)) => {
                if offset > content.len() {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidInput,
                        "Offset exceeds file size",
                    ));
                }

                let end_position = if let Some(len) = max_length {
                    if offset + len > content.len() {
                        content.len()
                    } else {
                        offset + len
                    }
                } else {
                    content.len()
                };

                Ok(content[offset..end_position].to_vec())
            }
            Some(FileContent::Text(_)) => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "File is not a binary file",
            )),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "File not found",
            )),
        }
    }

    fn file_size(&self, canonical_full_path: &Path) -> Result<usize, std::io::Error> {
        let file_content = self.file_content_map.get(canonical_full_path);

        match file_content {
            Some(FileContent::Binary(content)) => Ok(content.len()),
            Some(FileContent::Text(content)) => Ok(content.len()),
            None => Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "File not found",
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use pretty_assertions::assert_eq;

    use crate::{
        file_provider::{FileProvider, ResolvedResult},
        memory_file_provider::MemoryFileProvider,
    };

    #[test]
    fn test_memory_file_provider() {
        let mut provider = MemoryFileProvider::new();
        provider.add_user_text_file(Path::new("src/main.c"), "SRC_MAIN_C");
        provider.add_user_text_file(Path::new("src/lib.c"), "SRC_LIB_C");
        provider.add_user_text_file(Path::new("include/hello.h"), "INCLUDE_HELLO_H");
        provider.add_user_text_file(Path::new("header/foo.h"), "HEADER_FOO_H");
        provider.add_user_text_file(Path::new("header/bar.h"), "HEADER_BAR_H");
        provider.add_user_text_file(Path::new("header/folder/buz.h"), "HEADER_FOLDER_BUZ_H");
        provider.add_user_binary_file(Path::new("share/hippo.png"), vec![1, 2, 3, 4, 5]);
        provider.add_system_file(Path::new("stdio.h"), "STDIO_H");
        provider.add_system_file(Path::new("elf.h"), "ELF_H");

        // Test loading text files
        assert_eq!(
            provider
                .load_text_file(&PathBuf::from("/projects/test/src/main.c"))
                .unwrap(),
            "SRC_MAIN_C"
        );

        assert_eq!(
            provider
                .load_text_file(&PathBuf::from("/projects/test/include/hello.h"))
                .unwrap(),
            "INCLUDE_HELLO_H"
        );

        assert_eq!(
            provider
                .load_text_file(&PathBuf::from("/projects/test/header/foo.h"))
                .unwrap(),
            "HEADER_FOO_H"
        );

        assert_eq!(
            provider
                .load_text_file(&PathBuf::from("/projects/test/header/folder/buz.h"))
                .unwrap(),
            "HEADER_FOLDER_BUZ_H"
        );

        assert_eq!(
            provider
                .load_text_file(&PathBuf::from("/usr/include/stdio.h"))
                .unwrap(),
            "STDIO_H"
        );

        assert!(
            provider
                .load_text_file(&PathBuf::from("/projects/test/src/non_existent.c"))
                .is_err()
        );

        assert!(
            provider
                .load_text_file(&PathBuf::from("/projects/test/share/hippo.png"))
                .is_err()
        );

        // Test loading binary files
        assert_eq!(
            provider
                .load_binary_file(&PathBuf::from("/projects/test/share/hippo.png"), 0, None)
                .unwrap(),
            vec![1, 2, 3, 4, 5]
        );

        assert_eq!(
            provider
                .load_binary_file(&PathBuf::from("/projects/test/share/hippo.png"), 2, None)
                .unwrap(),
            vec![3, 4, 5]
        );

        assert_eq!(
            provider
                .load_binary_file(&PathBuf::from("/projects/test/share/hippo.png"), 2, Some(2))
                .unwrap(),
            vec![3, 4]
        );

        assert_eq!(
            provider
                .load_binary_file(
                    &PathBuf::from("/projects/test/share/hippo.png"),
                    2,
                    Some(10)
                )
                .unwrap(),
            vec![3, 4, 5]
        );

        assert!(
            provider
                .load_binary_file(&PathBuf::from("/projects/test/share/hippo.png"), 6, None)
                .is_err()
        );

        assert!(
            provider
                .load_binary_file(
                    &PathBuf::from("/projects/test/share/non_existent.png"),
                    0,
                    None
                )
                .is_err()
        );

        assert!(
            provider
                .load_binary_file(&PathBuf::from("/projects/test/src/main.c"), 0, None)
                .is_err()
        );

        // Test resolving user header files
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

        assert_eq!(
            provider.resolve_user_file(Path::new("hippo.png")),
            Some(PathBuf::from("/projects/test/share/hippo.png"))
        );

        // Test resolving system header files
        assert_eq!(
            provider.resolve_system_file(Path::new("stdio.h")),
            Some(PathBuf::from("/usr/include/stdio.h"))
        );

        // Test resolving user header files relative to a source file
        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("./bar.h"),
                Path::new("/projects/test/header/foo.h")
            ),
            Some(PathBuf::from("/projects/test/header/bar.h"))
        );

        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("folder/buz.h"),
                Path::new("/projects/test/header/foo.h")
            ),
            Some(PathBuf::from("/projects/test/header/folder/buz.h"))
        );

        assert_eq!(
            provider.resolve_user_file_relative_to_current_file(
                Path::new("../foo.h"),
                Path::new("/projects/test/header/folder/buz.h")
            ),
            Some(PathBuf::from("/projects/test/header/foo.h"))
        );

        // Test resolving user header files with fallback (resolve to user header directory)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("foo.h"),
                Path::new("/projects/test/src/main.c"),
                true
            ),
            Some(ResolvedResult::new(
                PathBuf::from("/projects/test/header/foo.h"),
                false
            ))
        );

        // Test resolving user header files with fallback (resolve to system header directory)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("elf.h"),
                Path::new("/projects/test/src/main.c"),
                true
            ),
            Some(ResolvedResult::new(
                PathBuf::from("/usr/include/elf.h"),
                true
            ))
        );

        // Test resolving user header files with fallback (enable relative path)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("./lib.c"),
                Path::new("/projects/test/src/main.c"),
                true
            ),
            Some(ResolvedResult::new(
                PathBuf::from("/projects/test/src/lib.c"),
                false
            ))
        );

        // Test resolving user header files with fallback (disable relative path)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("./lib.c"),
                Path::new("/projects/test/src/main.c"),
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
