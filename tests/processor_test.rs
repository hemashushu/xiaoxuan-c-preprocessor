// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{collections::HashMap, env, path::PathBuf};

use pretty_assertions::assert_eq;

use ancpp::{
    FILE_NUMBER_SOURCE_FILE_BEGIN,
    context::{HeaderFileCache, PreprocessResult},
    error::PreprocessFileError,
    native_file_provider::NativeFileProvider,
    process_source_file,
    token::{C23_KEYWORDS, TokenWithLocation},
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

/// Help function to create a NativeFileProvider for the test project.
fn get_file_provider() -> NativeFileProvider {
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

    NativeFileProvider::new(&user_include_dirs, &system_include_dirs)
}

/// Help function to print tokens as a string.
fn print_tokens(token_with_location: &[TokenWithLocation]) -> String {
    token_with_location
        .iter()
        .map(|TokenWithLocation { token, .. }| token.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Help function to process a single source file with given predefinitions.
fn process_single_source_file(
    source_file_path_within_project_root: &str,
    predefinitions: &HashMap<String, String>,
) -> Result<PreprocessResult, PreprocessFileError> {
    let mut file_cache = HeaderFileCache::new();
    let file_provider = get_file_provider();

    let source_file_path = PathBuf::from(source_file_path_within_project_root);

    let project_root = get_rootfs_path().join("projects/hello");
    let source_file_canonical_full_path = project_root.join(&source_file_path);

    process_source_file(
        &file_provider,
        &mut file_cache,
        &C23_KEYWORDS,
        predefinitions,
        false,
        false,
        FILE_NUMBER_SOURCE_FILE_BEGIN,
        &source_file_path,
        &source_file_canonical_full_path,
    )
}

/// Help function to process a single source file and get the output tokens.
fn process_single_source_file_and_get_tokens(
    src: &str,
    predefinitions: &HashMap<String, String>,
) -> Vec<TokenWithLocation> {
    process_single_source_file(src, predefinitions)
        .unwrap()
        .output
}

#[test]
fn test_source_file_foo() {
    let predefinitions = HashMap::new();

    let tokens = process_single_source_file_and_get_tokens("src/foo.c", &predefinitions);

    assert_eq!(
        print_tokens(&tokens),
        "\
        int foo ( ) ; \
        int foo ( ) { \
            return 42 ; \
        }"
    );
}

#[test]
fn test_source_file_bar() {
    let predefinitions = HashMap::new();

    let tokens = process_single_source_file_and_get_tokens("src/bar.c", &predefinitions);

    assert_eq!(
        print_tokens(&tokens),
        "\
        int bar ( ) ; \
        char data [ ] = { 0x01 , 0x02 , 0x03 , 0x04 , 0x05 } ; \
        int bar ( ) { \
            int total = 0 ; \
            for ( int i = 0 ; i < sizeof ( data ) ; i ++ ) { \
                total += data [ i ] ; \
            } \
            return total ; \
        }"
    );
}

#[test]
fn test_source_file_lib() {
    let predefinitions = HashMap::new();

    let tokens = process_single_source_file_and_get_tokens("src/lib.c", &predefinitions);

    assert_eq!(
        print_tokens(&tokens),
        "\
        int hello ( ) ; \
        int foo ( ) ; \
        int bar ( ) ; \
        int hello ( ) { \
            return foo ( ) + bar ( ) ; \
        }"
    );
}

#[test]
fn test_source_file_main() {
    let predefinitions = HashMap::new();
    let tokens = process_single_source_file_and_get_tokens("src/main.c", &predefinitions);
    assert_eq!(
        print_tokens(&tokens),
        "\
        int puts ( const char * s ) ; \
        int printf ( const char * format , ... ) ; \
        int hello ( ) ; \
        int main ( ) { \
            puts ( \"Hello, World!\" ) ; \
            int value = hello ( ) ; \
            printf ( \"Value: %d\\n\" , value ) ; \
            return 0 ; \
        }"
    );
}
