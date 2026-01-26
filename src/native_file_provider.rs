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
        context::{FileProvider, ResolvedResult},
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
            |filepath: &str| -> PathBuf { rootfs_path.join(filepath.trim_start_matches('/')) };

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
            provider.resolve_user_file(Path::new("hippo.png")),
            Some(vrootfs("/projects/hello/src/resources/hippo.png"))
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
            Some(ResolvedResult::new(
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
            Some(ResolvedResult::new(vrootfs("/usr/include/stdlib.h"), true))
        );

        // Test resolving user header files with fallback (enable relative path)
        assert_eq!(
            provider.resolve_user_file_with_fallback(
                Path::new("./lib.c"),
                &vrootfs("/projects/hello/src/main.c"),
                true
            ),
            Some(ResolvedResult::new(
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

// TODO
// MOVE TO PROCESSOR TESTS
//
// #[cfg(test)]
// mod tests {

//     use std::{
//         collections::HashMap,
//         env, io,
//         path::{Path, PathBuf},
//     };

//     use pretty_assertions::assert_eq;

//     use crate::{context::{FileProvider, HeaderFileCache, PreprocessResult}, native_file_provider::NativeFileProvider, token::TokenWithLocation};

//     fn build_test_provider() -> NativeFileProvider {
//         let test_resources_path = get_test_resources_path();

//         let project_root_path = test_resources_path.join("projects/test");
//         let user_include_dirs = vec![
//             project_root_path.join("include"),
//             project_root_path.join("header"),
//             project_root_path.join("resources"),
//             project_root_path.join("src/common"),
//         ];

//         let system_include_path = test_resources_path.join("usr/include");
//         let system_include_dirs = vec![system_include_path];

//         NativeFileProvider::new(&user_include_dirs, &system_include_dirs)
//     }

//     fn resolve_source_file_canonical_path(relative_file_path: &Path) -> Result<PathBuf, io::Error> {
//         let test_resources_path = get_test_resources_path();
//         let project_root_path = test_resources_path.join("projects/test");

//         let full_path = project_root_path.join(relative_file_path);
//         let canonical_full_path = full_path.canonicalize()?;
//         Ok(canonical_full_path)
//     }

//     fn print_tokens(token_with_location: &[TokenWithLocation]) -> String {
//         token_with_location
//             .iter()
//             .map(|TokenWithLocation { token, .. }| token.to_string())
//             .collect::<Vec<_>>()
//             .join(" ")
//     }

//     fn process<T>(
//         source_file_path_name: &Path,
//         file_provider: &T,
//         file_cache: &mut HeaderFileCache,
//         predefinitions: &HashMap<String, String>,
//         source_file_number: usize,
//     ) -> PreprocessResult
//     where
//         T: FileProvider,
//     {

//         process_source_file(
//             file_provider,
//             file_cache,
//             &C23_KEYWORDS,
//             predefinitions,
//             false,
//             source_file_number,
//             source_file_path_name,
//             &resolve_source_file_canonical_path(source_file_path_name).unwrap(),
//         )
//         .unwrap()
//     }

//     #[test]
//     fn test_native_file_provider() {
//         let mut file_cache = HeaderFileCache::new();
//         let file_provider = build_test_provider();
//         let predefinitions = HashMap::new();

//         let file_data_c = Path::new("src/data.c");
//         let file_data_c_result = process(
//             file_data_c,
//             &file_provider,
//             &mut file_cache,
//             &predefinitions,
//             FILE_NUMBER_SOURCE_FILE_BEGIN,
//         );

//         assert_eq!(
//             print_tokens(&file_data_c_result.output),
//             "\
//             int sum ( ) ; \
//             char data [ ] = { 0x01 , 0x02 , 0x03 , 0x04 , 0x05 , 0x06 , 0x07 , 0x08 , 0x09 , 0x0a } ; \
//             int sum ( ) { \
//                 int total = 0 ; \
//                 for ( int i = 0 ; i < sizeof ( data ) ; i ++ ) { \
//                     total += data [ i ] ; \
//                 } return total ; \
//             }"
//         );

//         let file_lib_c = Path::new("src/lib.c");
//         let file_lib_c_result = process(
//             file_lib_c,
//             &file_provider,
//             &mut file_cache,
//             &predefinitions,
//             FILE_NUMBER_SOURCE_FILE_BEGIN + 1,
//         );
//         assert_eq!(
//             print_tokens(&file_lib_c_result.output),
//             "\
//             int add ( int , int ) ; \
//             int add ( int a , int b ) { \
//                 return a + b ; \
//             }"
//         );

//         let file_main_c = Path::new("src/main.c");
//         let file_main_c_result = process(
//             file_main_c,
//             &file_provider,
//             &mut file_cache,
//             &predefinitions,
//             FILE_NUMBER_SOURCE_FILE_BEGIN + 1,
//         );
//         assert_eq!(
//             print_tokens(&file_main_c_result.output),
//             "\
//             int puts ( const char * s ) ; \
//             int printf ( const char * format , ... ) ; \
//             int sum ( ) ; \
//             int add ( int , int ) ; \
//             int main ( ) { \
//                 puts ( \"Hello, world!\" ) ; \
//                 int result = add ( 1 , 2 ) ; \
//                 printf ( \"1 + 2 = %d\\n\" , result ) ; \
//                 int total = sum ( ) ; \
//                 printf ( \"Sum of data: %d\\n\" , total ) ; \
//                 return 0 ; \
//             }"
//         );
//     }
// }
