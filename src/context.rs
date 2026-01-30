// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Component, Path, PathBuf},
};

use crate::{
    FILE_NUMBER_PREDEFINED,
    ast::Program,
    error::PreprocessError,
    lexer::lex_from_clean_str,
    location::Location,
    position::Position,
    range::Range,
    token::{TokenWithLocation, TokenWithRange},
};

/// The `Context` struct holds all state required during preprocessing.
/// It manages file access, macro definitions, file inclusion tracking, and prompt messages.
pub struct Context<'a, T>
where
    T: FileProvider,
{
    /// The file provider used to load source and header files.
    pub file_provider: &'a T,

    /// The cache for header files to avoid redundant parsing.
    /// Only header files (`*.h`) are cached, not source files (`*.c`).
    pub header_file_cache: &'a mut HeaderFileCache,

    /// Identifiers which are used to prevent defining macros with these names.
    /// They are usually C keywords, such as `int`, `return`, `if`, `else`, etc.
    /// You may simply use `token::C23_KEYWORDS`.
    pub reserved_identifiers: &'a [&'a str],

    /// Macro definitions and related state.
    pub macro_map: MacroMap,

    /// A flag to control whether to resolve relative paths within the current source file.
    ///
    /// Set to true to resolve relative paths to the source file,
    /// otherwise, preprocessor will only search in the specified including directories.
    ///
    /// For example, consider there are 3 defined include directories:
    ///
    /// - `./include`
    /// - `./src/headers`
    /// - `/usr/include`
    ///
    /// The statement `#include "foo.h"` in the source file `src/main.c` will try to
    /// match `foo.h` in the following order:
    ///
    /// - `./include/foo.h`
    /// - `./src/headers/foo.h`
    /// - `/usr/include/foo.h`
    ///
    /// If `resolve_relative_path_within_current_file` is true, then it will also
    /// try to match `src/foo.h`.
    pub resolve_relative_path_within_current_file: bool,

    // A flag that controls whether function-like macro arguments may consist of multiple tokens.
    //
    // When false (the default), ANCPP requires each macro argument to be a single token
    // (identifier, number, string literal, or character literal). When true, an argument
    // may be an arbitrary sequence of tokens, allowing more complex expressions to be
    // passed as a single parameter.
    pub enable_multiple_token_argument: bool,

    /// The file number currently being processed.
    pub current_file_item: FileItem,

    /// List of included files to prevent redundant inclusions.
    /// Besides the header files, the source file itself is also
    /// included to prevent self-inclusion or reversive inclusion.
    pub included_files: Vec<FileLocation>,

    /// User-facing messages, warnings, or information.
    pub prompts: Vec<Prompt>,

    /// Output tokens generated during preprocessing.
    pub output: Vec<TokenWithLocation>,
}

impl<'a, T> Context<'a, T>
where
    T: FileProvider,
{
    #[allow(clippy::too_many_arguments)]
    #[allow(dead_code)]
    pub fn new(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        resolve_relative_path_within_current_file: bool,
        enable_multiple_token_argument: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Self {
        let location = FileLocation::new(
            FilePathSource::from_source_file(current_file_relative_path),
            current_file_canonical_full_path,
        );
        let current_file_item = FileItem::new(current_file_number, location);

        Self {
            file_provider,
            header_file_cache: file_cache,
            reserved_identifiers,
            resolve_relative_path_within_current_file,
            enable_multiple_token_argument,
            current_file_item,
            macro_map: MacroMap::new(),
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from_predefinitions(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        predefinitions: &HashMap<String, String>,
        resolve_relative_path_within_current_file: bool,
        enable_multiple_token_argument: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessError> {
        let location = FileLocation::new(
            FilePathSource::from_source_file(current_file_relative_path),
            current_file_canonical_full_path,
        );
        let current_file_item = FileItem::new(current_file_number, location);

        Ok(Self {
            file_provider,
            header_file_cache: file_cache,
            reserved_identifiers,
            macro_map: MacroMap::from_key_values(predefinitions)?,
            resolve_relative_path_within_current_file,
            enable_multiple_token_argument,
            current_file_item,
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        })
    }

    pub fn contains_include_file(&self, canonical_full_path: &Path) -> bool {
        self.included_files
            .iter()
            .any(|file| file.canonical_full_path == canonical_full_path)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FileItem {
    pub number: usize,
    pub location: FileLocation,
}

impl FileItem {
    pub fn new(number: usize, location: FileLocation) -> Self {
        Self { number, location }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FileLocation {
    pub source: FilePathSource,
    pub canonical_full_path: PathBuf,
}

/// Represents the source of a file.
#[derive(Debug, PartialEq, Clone)]
pub enum FilePathSource {
    /// Represents a file that is part of the source code, such as `main.c` and `lib.c`.
    /// The `relative_file_path` is the path relative to the project root directory.
    SourceFile(/* relative_file_path */ PathBuf),

    /// Represents a user header file, which is typically a header file included by the user.
    /// For example, `#include "my_header.h"`.
    /// The `relative_file_path` is the path relative to the user header directory.
    UserHeader(/* relative_file_path */ PathBuf),

    /// Represents a system header file, which is typically a standard library header or a system-provided header.
    /// For example, `#include <stdio.h>`.
    /// The `external_file_path` is the path relative to the system header directory.
    SystemHeader(/* external_file_path */ PathBuf),
}

impl FileLocation {
    pub fn new(source: FilePathSource, canonical_full_path: &Path) -> Self {
        Self {
            source,
            canonical_full_path: canonical_full_path.to_path_buf(),
        }
    }
}

impl FilePathSource {
    pub fn from_source_file(relative_file_path: &Path) -> Self {
        FilePathSource::SourceFile(relative_file_path.to_path_buf())
    }

    pub fn from_user_header_file(relative_file_path: &Path) -> Self {
        FilePathSource::UserHeader(relative_file_path.to_path_buf())
    }

    pub fn from_system_header_file(relative_file_path: &Path) -> Self {
        FilePathSource::SystemHeader(relative_file_path.to_path_buf())
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct MacroMap {
    macros: HashMap<String, MacroDefinition>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum MacroDefinition {
    ObjectLike(Vec<TokenWithLocation>),
    FunctionLike(Vec<String>, Vec<TokenWithLocation>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MacroManipulationResult {
    Success,
    AlreadyExist,
    NotFound,
}

/// An abstract file system interface that provides
/// access to the user and system header files and binary resources.
pub trait FileProvider {
    /// Resolves a user header file path relative to the user header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file(&self, relative_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a user header file path relative to the current file.
    ///
    /// Examples:
    /// - If the current file is `/path/to/source.c` and the relative path
    ///   is `../include/foo.h`, this function will resolve it to `/path/to/include/foo.h`.
    /// - If the current file is `/path/to/include/foo.h` and the relative path is `./bar.h`,
    ///   it will resolve it to `/path/to/include/bar.h`.
    fn resolve_user_file_relative_to_current_file(
        &self,
        relative_file_path: &Path,
        source_file_canonical_full_path: &Path,
    ) -> Option<PathBuf>;

    /// Resolves a system header file path relative to the system header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_system_file(&self, relative_file_path: &Path) -> Option<PathBuf>;

    /// Resolves a user header file path using a fallback strategy.
    /// This mimics the behavior of the quoted include directive in C, e.g., `#include "header.h"`.
    ///
    /// The resolution process is as follows:
    /// 1. If `resolve_relative` is true, attempts to resolve the header file
    ///    relative to the source file's directory.
    /// 2. If the target file does not found, attempts to resolve the file
    ///    in the user header search directories.
    /// 3. If it does not found either, attempts to resolve the file in the
    ///    system header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file_with_fallback(
        &self,
        target_file_relative_path: &Path,
        source_file_canonical_full_path: &Path,
        resolve_relative_path_within_current_file: bool,
    ) -> Option<FilePathResolveResult> {
        if resolve_relative_path_within_current_file
            && let Some(resolved_path) = self.resolve_user_file_relative_to_current_file(
                target_file_relative_path,
                source_file_canonical_full_path,
            )
        {
            return Some(FilePathResolveResult::new(resolved_path, false));
        }

        if let Some(resolved_path) = self.resolve_user_file(target_file_relative_path) {
            return Some(FilePathResolveResult::new(resolved_path, false));
        }

        if let Some(resolved_path) = self.resolve_system_file(target_file_relative_path) {
            return Some(FilePathResolveResult::new(resolved_path, true));
        }

        None
    }

    /// Loads the contents of a file given its canonical path.
    /// Returns the file contents as a `String` if successful, or an `std::io::Error` if an error occurs.
    fn load_text_file(&self, canonical_full_path: &Path) -> Result<String, std::io::Error>;

    /// Loads the contents of a binary file given its canonical path.
    /// Returns the file contents as a `Vec<u8>` if successful, or an `std::io::Error` if an error occurs.
    fn load_binary_file(
        &self,
        canonical_full_path: &Path,

        // Optional offset to start reading from.
        offset: usize,

        // Optional maximum length to read from the file.
        // If `None`, reads the entire file.
        // If `Some(n)`, reads up to `n` bytes.
        // If the file is shorter than `n`, reads the entire file.
        max_length: Option<usize>,
    ) -> Result<Vec<u8>, std::io::Error>;

    /// Returns the size of the file in bytes given its canonical path.
    fn file_size(&self, canonical_full_path: &Path) -> Result<usize, std::io::Error>;
}

#[derive(Debug, PartialEq)]
pub struct FilePathResolveResult {
    /// The canonical path of the resolved file.
    pub canonical_full_path: PathBuf,

    /// Indicates whether the resolved file is a system header file.
    pub is_system_file: bool,
}

impl FilePathResolveResult {
    pub fn new(canonical_full_path: PathBuf, is_system_file: bool) -> Self {
        Self {
            canonical_full_path,
            is_system_file,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PreprocessResult {
    pub output: Vec<TokenWithLocation>,
    pub prompts: Vec<Prompt>,
}

/// Canonicalizes a given path without checking the real file system.
///
/// This function normalizes the path by resolving components like `.` and `..`,
/// but does not resolve symbolic links or check if the path exists on the file system.
/// Returns a new `PathBuf` with the normalized path.
pub fn normalize_path(src: &Path) -> PathBuf {
    let mut output = PathBuf::new();

    let components = src.components();
    for component in components {
        match component {
            Component::Prefix(..) => unimplemented!("todo: handle prefix components"),
            Component::RootDir => {
                output.push(component.as_os_str());
            }
            Component::CurDir => {}
            Component::ParentDir => {
                output.pop();
            }
            Component::Normal(c) => {
                output.push(c);
            }
        }
    }
    output
}

/// A cache for the processed header files.
///
/// Stores the canonical path, source code, and the parsed AST for each file.
/// This cache prevents redundant parsing of the same file multiple times.
///
/// Note that only header files (`*.h`) are cached, not source files (`*.c`).
pub struct HeaderFileCache {
    items: Vec<HeaderFileCacheItem>,
}

pub struct HeaderFileCacheItem {
    pub canonical_full_path: PathBuf,

    /// The source code of the file.
    /// It is used to generate the error message when parsing fails.
    pub text_content: String,

    pub file_number: usize,
    pub program: Program,
}

impl HeaderFileCache {
    /// Creates a new, empty `FileCache`.
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// File number is assigned automatically in the order of addition.
    /// The first file number is 1.
    /// File number 0 is reserved for predefined macros.
    pub fn next_file_number(&self) -> usize {
        self.items.len() + 1 // File number starts from 1
    }

    /// Adds a file to the cache.
    pub fn add(
        &mut self,
        canonical_full_path: &Path,
        text_content: &str,
        file_number: usize,
        program: Program,
    ) -> usize {
        let item = HeaderFileCacheItem {
            canonical_full_path: canonical_full_path.to_path_buf(),
            text_content: text_content.to_owned(),
            file_number,
            program,
        };

        self.items.push(item);

        file_number
    }

    pub fn get_by_canonical_full_path(
        &self,
        canonical_full_path: &Path,
    ) -> Option<&HeaderFileCacheItem> {
        self.items
            .iter()
            .find(|item| item.canonical_full_path == canonical_full_path)
    }

    /// Checks if a file with the given canonical path exists in the cache.
    pub fn exists(&self, canonical_full_path: &Path) -> bool {
        self.items
            .iter()
            .any(|item| item.canonical_full_path == canonical_full_path)
    }

    /// Returns a list of tuples containing the file number and canonical path for each cached file.
    /// For example, the first item will be `(1, "/path/to/file.h")`.
    ///
    /// The file number for the first item is 1, since 0 is reserved for predefined macros.
    pub fn get_cache_file_list(&self) -> &[HeaderFileCacheItem] {
        &self.items
    }
}

impl Default for HeaderFileCache {
    fn default() -> Self {
        Self::new()
    }
}

impl MacroMap {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
        }
    }

    pub fn from_key_values(
        predefinitions: &HashMap<String, String>,
    ) -> Result<Self, PreprocessError> {
        let mut macros = Self::new();
        for (key, value) in predefinitions {
            if macros.add_object_like_by_single_value_string(key, value)?
                == MacroManipulationResult::AlreadyExist
            {
                return Err(PreprocessError::Message(format!(
                    "Macro '{}' is already exist.",
                    key
                )));
            }
        }
        Ok(macros)
    }

    /// Adds a macro by a key and a single value string.
    ///
    /// `value` is a string represented with the source code style, e.g.,
    ///
    /// - `"123"` represents a number
    /// - `"\"abc\""` represents a string literal.
    ///
    /// This value should be a single literal, such as:
    ///
    /// - a number
    /// - a character
    /// - a string
    /// - an identifier
    /// - empty value (i.e., `""`)
    ///
    /// Returns `MacroManipulationResult::AlreadyExist` if the key was already present.
    fn add_object_like_by_single_value_string(
        &mut self,
        key: &str,
        value: &str,
    ) -> Result<MacroManipulationResult, PreprocessError> {
        // Tokenize the value and create TokenWithLocation instances.
        let tokens = lex_from_clean_str(value)?;
        let token_with_locations: Vec<TokenWithLocation> = tokens
            .into_iter()
            .map(|token_with_range| {
                let location = Location::new(FILE_NUMBER_PREDEFINED, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token, location)
            }) // Assuming line and column are 0 for simplicity
            .collect();

        if token_with_locations.len() > 1 {
            return Err(PreprocessError::Message(format!(
                "Adding macro '{}' failed: expected a single value, but got {} tokens.",
                key,
                token_with_locations.len()
            )));
        }

        let item = MacroDefinition::ObjectLike(token_with_locations);

        Ok(match self.macros.insert(key.to_owned(), item) {
            Some(_) => MacroManipulationResult::AlreadyExist, // Key was already present
            None => MacroManipulationResult::Success,         // Key was added successfully
        })
    }

    /// Adds a definition by a key and a list of tokens with ranges.
    /// Returns `MacroManipulationResult::AlreadyExist` if the key was already present.
    pub fn add_object_like(
        &mut self,
        file_number: usize,
        key: &str,
        definition: &[TokenWithRange], // Unexpanded tokens with ranges.
    ) -> MacroManipulationResult {
        let token_with_locations: Vec<TokenWithLocation> = definition
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        let macro_definition = MacroDefinition::ObjectLike(token_with_locations);

        match self.macros.insert(key.to_owned(), macro_definition) {
            Some(_) => MacroManipulationResult::AlreadyExist, // Key was already present
            None => MacroManipulationResult::Success,         // Key was added successfully
        }
    }

    /// Adds a function-like macro definition by a key, parameters, and a list of tokens with ranges.
    /// Returns `MacroManipulationResult::AlreadyExist` if the key was already present.
    pub fn add_function_like(
        &mut self,
        file_number: usize,
        key: &str,
        parameters: &[String],
        definition: &[TokenWithRange], // Unexpanded tokens with ranges.
    ) -> MacroManipulationResult {
        let token_with_locations: Vec<TokenWithLocation> = definition
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        let macro_definition =
            MacroDefinition::FunctionLike(parameters.to_vec(), token_with_locations);

        match self.macros.insert(key.to_owned(), macro_definition) {
            Some(_) => MacroManipulationResult::AlreadyExist, // Key was already present
            None => MacroManipulationResult::Success,         // Key was added successfully
        }
    }

    pub fn get(&self, key: &str) -> Option<&MacroDefinition> {
        self.macros.get(key)
    }

    /// Remove the definition for the given key.
    /// Returns `MacroManipulationResult::Success` if the key was present and removed,
    /// `MacroManipulationResult::NotFound` otherwise.
    pub fn remove(&mut self, key: &str) -> MacroManipulationResult {
        match self.macros.remove(key) {
            Some(_) => MacroManipulationResult::Success, // Key was present and removed
            None => MacroManipulationResult::NotFound,   // Key was not present
        }
    }

    pub fn contains_key(&self, key: &str) -> bool {
        self.macros.contains_key(key)
    }

    pub fn contains(&self, key: &str, is_invocation: bool) -> bool {
        match self.macros.get(key) {
            Some(MacroDefinition::ObjectLike(_)) => !is_invocation,
            Some(MacroDefinition::FunctionLike(_, _)) => is_invocation,
            None => false,
        }
    }
}

/// `Prompt` is similar to `PreprocessError`, but is intended for user-facing messages
/// that do not necessarily indicate an error. It can be used to display informational
/// messages, warnings, or other information relevant to the user.
#[derive(Debug, PartialEq)]
pub enum Prompt {
    Message(PromptLevel, /* file number */ usize, String),
    MessageWithPosition(PromptLevel, /* file number */ usize, String, Position),
    MessageWithRange(PromptLevel, /* file number */ usize, String, Range),
}

#[derive(Debug, PartialEq)]
pub enum PromptLevel {
    Info,
    Warning,
}
