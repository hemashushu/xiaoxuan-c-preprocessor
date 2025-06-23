// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    fs,
    io::{self, Read, Seek},
    path::{Path, PathBuf},
};

use crate::file_provider::FileProvider;

pub struct NativeFileProvider {
    /// Directories to search for system headers.
    system_directories: Vec<PathBuf>,

    /// Directories to search for user headers and binary files.
    user_directories: Vec<PathBuf>,
}

impl NativeFileProvider {
    /// Creates a new `NativeFileProvider` with the specified directories for system and user headers.
    ///
    /// - `system_directories`: Directories to search for system and external module headers.
    ///   These are used when resolving `#include` and `embed` directives with angle brackets,
    ///   e.g., `#include <stdio.h>`.
    /// - `user_directories`: Directories to search for module-specific headers.
    ///   These are used when resolving `#include` and `embed` directives with double quotes,
    ///   e.g., `#include "header.h"`.
    pub fn new(user_directories: &[PathBuf], system_directories: &[PathBuf]) -> Self {
        Self {
            system_directories: system_directories.to_vec(),
            user_directories: user_directories.to_vec(),
        }
    }
}

impl FileProvider for NativeFileProvider {
    fn resolve_user_file(&self, relative_file_path: &Path) -> Option<PathBuf> {
        for dir in &self.user_directories {
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
        for dir in &self.system_directories {
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
        collections::HashMap,
        env, io,
        path::{Path, PathBuf},
    };

    use pretty_assertions::assert_eq;

    use crate::{
        FILE_NUMBER_SOURCE_FILE_BEGIN, TokenWithLocation, file_provider::FileProvider,
        header_file_cache::HeaderFileCache, native_file_provider::NativeFileProvider,
        process_source_file, processor::PreprocessResult,
    };

    fn get_test_resources_path() -> PathBuf {
        // `env::current_dir()` returns the current Rust project's root folder
        let mut dir = env::current_dir().unwrap();
        dir.push("tests");
        dir.push("resources");
        dir.push("fs");
        dir
    }

    fn build_test_provider() -> NativeFileProvider {
        let test_resources_path = get_test_resources_path();

        let project_root_path = test_resources_path.join("projects/test");
        let user_dirs = vec![
            project_root_path.join("include"),
            project_root_path.join("header"),
            project_root_path.join("resources"),
            project_root_path.join("src/common"),
        ];

        let system_include_path = test_resources_path.join("usr/include");
        let system_dirs = vec![system_include_path];

        NativeFileProvider::new(&user_dirs, &system_dirs)
    }

    fn resolve_source_file_canonical_path(relative_file_path: &Path) -> Result<PathBuf, io::Error> {
        let test_resources_path = get_test_resources_path();
        let project_root_path = test_resources_path.join("projects/test");

        let full_path = project_root_path.join(relative_file_path);
        let canonical_full_path = full_path.canonicalize()?;
        Ok(canonical_full_path)
    }

    fn print_tokens(token_with_location: &[TokenWithLocation]) -> String {
        token_with_location
            .iter()
            .map(|TokenWithLocation { token, .. }| token.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    fn preprocess_source_file<T>(
        source_file_relative_path: &Path,
        file_provider: &T,
        file_cache: &mut HeaderFileCache,
        predefinitions: &HashMap<String, String>,
        source_file_number: usize,
    ) -> PreprocessResult
    where
        T: FileProvider,
    {
        process_source_file(
            file_provider,
            file_cache,
            predefinitions,
            false,
            source_file_number,
            source_file_relative_path,
            &resolve_source_file_canonical_path(source_file_relative_path).unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn test_native_file_provider() {
        let mut file_cache = HeaderFileCache::new();
        let file_provider = build_test_provider();
        let predefinitions = HashMap::new();

        let file_data_c = Path::new("src/data.c");
        let file_data_c_result = preprocess_source_file(
            file_data_c,
            &file_provider,
            &mut file_cache,
            &predefinitions,
            FILE_NUMBER_SOURCE_FILE_BEGIN,
        );

        assert_eq!(
            print_tokens(&file_data_c_result.output),
            "\
            int sum ( ) ; \
            char data [ ] = { 0x01 , 0x02 , 0x03 , 0x04 , 0x05 , 0x06 , 0x07 , 0x08 , 0x09 , 0x0a } ; \
            int sum ( ) { \
                int total = 0 ; \
                for ( int i = 0 ; i < sizeof ( data ) ; i ++ ) { \
                    total += data [ i ] ; \
                } return total ; \
            }"
        );

        let file_lib_c = Path::new("src/lib.c");
        let file_lib_c_result = preprocess_source_file(
            file_lib_c,
            &file_provider,
            &mut file_cache,
            &predefinitions,
            FILE_NUMBER_SOURCE_FILE_BEGIN + 1,
        );
        assert_eq!(
            print_tokens(&file_lib_c_result.output),
            "\
            int add ( int , int ) ; \
            int add ( int a , int b ) { \
            return a + b ; \
            }"
        );

        let file_main_c = Path::new("src/main.c");
        let file_main_c_result = preprocess_source_file(
            file_main_c,
            &file_provider,
            &mut file_cache,
            &predefinitions,
            FILE_NUMBER_SOURCE_FILE_BEGIN + 1,
        );
        assert_eq!(
            print_tokens(&file_main_c_result.output),
            "\
            int puts ( const char * s ) ; \
            int printf ( const char * format , ... ) ; \
            int sum ( ) ; \
            int add ( int , int ) ; \
            int main ( ) { \
                puts ( \"Hello, world!\" ) ; \
                int result = add ( 1 , 2 ) ; \
                printf ( \"1 + 2 = %d\\n\" , result ) ; \
                int total = sum ( ) ; \
                printf ( \"Sum of data: %d\\n\" , total ) ; \
                return 0 ; \
            }"
        );
    }
}
