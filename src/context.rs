// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use crate::{
    PreprocessError, TokenWithLocation, file_provider::FileProvider,
    header_file_cache::HeaderFileCache, macro_map::MacroMap, prompt::Prompt,
};

/// The `Context` struct holds all state required during preprocessing.
/// It manages file access, macro definitions, file inclusion tracking, and user-facing messages.
pub struct Context<'a, T>
where
    T: FileProvider,
{
    /// Reference to the file provider used for file access.
    pub file_provider: &'a T,

    /// Mutable reference to the file cache.
    pub header_file_cache: &'a mut HeaderFileCache,

    pub reserved_identifiers: &'a [&'a str],

    /// Macro definitions and related state.
    pub macro_map: MacroMap,

    /// Whether to resolve relative file paths base on the current file.
    /// Set to false to only search in the specified including directories.
    pub resolve_relative_path_within_current_file: bool,

    /// The file number currently being processed.
    pub current_file_item: FileItem,

    /// List of included files to prevent redundant inclusions.
    pub included_files: Vec<FileLocation>,

    /// User-facing messages, warnings, or notifications.
    pub prompts: Vec<Prompt>,

    /// Output tokens generated during preprocessing.
    pub output: Vec<TokenWithLocation>,
}

impl<'a, T> Context<'a, T>
where
    T: FileProvider,
{
    #[allow(dead_code)]
    pub fn new(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        resolve_relative_path_within_current_file: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Self {
        Self {
            file_provider,
            header_file_cache: file_cache,
            reserved_identifiers,
            resolve_relative_path_within_current_file,
            current_file_item: FileItem::new(
                current_file_number,
                FileLocation::new(
                    current_file_canonical_full_path,
                    FileOrigin::from_source_file(current_file_relative_path),
                ),
            ),
            macro_map: MacroMap::new(),
            included_files: Vec::new(),
            prompts: Vec::new(),
            output: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from_keyvalues(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        predefinitions: &HashMap<String, String>,
        resolve_relative_path_within_current_file: bool,
        current_file_number: usize,
        current_file_relative_path: &Path,
        current_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessError> {
        Ok(Self {
            file_provider,
            header_file_cache: file_cache,
            reserved_identifiers,
            macro_map: MacroMap::from_key_values(predefinitions)?,
            resolve_relative_path_within_current_file,
            current_file_item: FileItem::new(
                current_file_number,
                FileLocation::new(
                    current_file_canonical_full_path,
                    FileOrigin::from_source_file(current_file_relative_path),
                ),
            ),
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
    pub file_location: FileLocation,
}

impl FileItem {
    pub fn new(number: usize, file_location: FileLocation) -> Self {
        Self {
            number,
            file_location,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FileLocation {
    pub canonical_full_path: PathBuf,
    pub file_origin: FileOrigin,
}

/// Represents the source of a file.
#[derive(Debug, PartialEq, Clone)]
pub enum FileOrigin {
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
    pub fn new(canonical_full_path: &Path, file_origin: FileOrigin) -> Self {
        Self {
            canonical_full_path: canonical_full_path.to_path_buf(),
            file_origin,
        }
    }
}

impl FileOrigin {
    pub fn from_source_file(relative_file_path: &Path) -> Self {
        FileOrigin::SourceFile(relative_file_path.to_path_buf())
    }

    pub fn from_user_header_file(relative_file_path: &Path) -> Self {
        FileOrigin::UserHeader(relative_file_path.to_path_buf())
    }

    pub fn from_system_header_file(relative_file_path: &Path) -> Self {
        FileOrigin::SystemHeader(relative_file_path.to_path_buf())
    }
}


pub struct MacroMap {
    macros: HashMap<String, MacroDefinition>,
}

pub enum MacroDefinition {
    ObjectLike(Vec<TokenWithLocation>),
    FunctionLike(Vec<String>, Vec<TokenWithLocation>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum MacroManipulationResult {
    Success,
    Failure,
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
    /// - if the current file is `/path/to/source.c` and the relative path
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
    /// 1. If `resolve_relative` is true, attempts to resolve the header file relative to the source file's directory.
    /// 2. If not found, attempts to resolve the file in the user header search directories.
    /// 3. If still not found, attempts to resolve the file in the system header search directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file_with_fallback(
        &self,
        target_file_relative_path: &Path,
        source_file_canonical_full_path: &Path,
        resolve_relative_path_within_current_file: bool,
    ) -> Option<ResolvedResult> {
        if resolve_relative_path_within_current_file
            && let Some(resolved_path) = self.resolve_user_file_relative_to_current_file(
                target_file_relative_path,
                source_file_canonical_full_path,
            )
        {
            return Some(ResolvedResult::new(resolved_path, false));
        }

        if let Some(resolved_path) = self.resolve_user_file(target_file_relative_path) {
            return Some(ResolvedResult::new(resolved_path, false));
        }

        if let Some(resolved_path) = self.resolve_system_file(target_file_relative_path) {
            return Some(ResolvedResult::new(resolved_path, true));
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
pub struct ResolvedResult {
    /// The canonical path of the resolved file.
    pub canonical_full_path: PathBuf,

    /// Indicates whether the resolved file is a system header file.
    pub is_system_file: bool,
}

impl ResolvedResult {
    pub fn new(canonical_full_path: PathBuf, is_system_file: bool) -> Self {
        Self {
            canonical_full_path,
            is_system_file,
        }
    }
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
/// The souce code file (`*.c`) should not be cached.
pub struct HeaderFileCache {
    items: Vec<CacheItem>,
}

struct CacheItem {
    canonical_full_path: PathBuf,

    /// The source code of the file.
    /// It is used to generate the error message when parsing fails.
    text_content: String,

    program: Program,
}

impl HeaderFileCache {
    /// Creates a new, empty `FileCache`.
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Adds a file to the cache.
    ///
    /// - `canonical_full_path` - The canonical path of the file.
    /// - `text_content` - The source code of the file.
    ///
    /// Returns the file number assigned to this file.
    /// File number starts from 1 since 0 is reserved for predefined macros.
    pub fn add(&mut self, canonical_full_path: &Path, text_content: &str) -> usize {
        // File number starts from 1 since 0 is reserved for predefined macros.
        let file_number = self.items.len() + 1;

        let item = CacheItem {
            canonical_full_path: canonical_full_path.to_path_buf(),
            text_content: text_content.to_owned(),
            program: Program::default(),
        };

        self.items.push(item);

        file_number
    }

    /// Sets the program for a file in the cache.
    ///
    /// The `Program` is set after parsing the file instead of during the `add` method.
    /// This is because the `Program` is generated after parsing the file,
    /// and the parse operation may fail and raise an error, which contains
    /// the `file_number`, this number only exists after the file is `add` to the cache.
    pub fn set_program(&mut self, canonical_full_path: &Path, program: Program) {
        let file_number = self
            .items
            .iter()
            .position(|item| item.canonical_full_path == canonical_full_path)
            .unwrap();

        self.items[file_number].program = program;
    }

    pub fn get_file_number(&self, canonical_full_path: &Path) -> Option<usize> {
        self.items
            .iter()
            .position(|item| item.canonical_full_path == canonical_full_path)
            .map(|index| index + 1) // File number starts from 1
    }

    pub fn get_text_content(&self, canonical_full_path: &Path) -> Option<&String> {
        self.items
            .iter()
            .find(|item| item.canonical_full_path == canonical_full_path)
            .map(|item| &item.text_content)
    }

    pub fn get_program(&self, canonical_full_path: &Path) -> Option<&Program> {
        self.items
            .iter()
            .find(|item| item.canonical_full_path == canonical_full_path)
            .map(|item| &item.program)
    }

    /// Checks if a file with the given canonical path exists in the cache.
    pub fn contains(&self, canonical_full_path: &Path) -> bool {
        self.items
            .iter()
            .any(|item| item.canonical_full_path == canonical_full_path)
    }

    /// Returns a list of tuples containing the file number and canonical path for each cached file.
    ///
    /// The file number for the first item is 1, since 0 is reserved for predefined macros.
    /// For example, the first item will be `(1, "/path/to/file.h")`.
    pub fn get_cache_file_list(&self) -> Vec<(usize, PathBuf)> {
        self.items
            .iter()
            .enumerate()
            .map(|(index, item)| (index + 1, item.canonical_full_path.clone()))
            .collect()
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
        let mut definition = Self::new();
        for (key, value) in predefinitions {
            if definition.add_by_key_value(key, value)? == MacroManipulationResult::Failure {
                return Err(PreprocessError::Message(format!(
                    "Macro '{}' is already defined.",
                    key
                )));
            }
        }
        Ok(definition)
    }

    /// Adds a definition by a key and a value string.
    ///
    /// `value`: A string represented with the source code style.
    /// e.g. `"123"` represents a number, and `"\"abc\""` represents a string literal.
    /// This value should be a single literal, such as a number, character, string, or identifier,
    /// an empty value is allowed.
    ///
    /// Returns `Ok(false)` if the key was already present.
    pub fn add_by_key_value(
        &mut self,
        key: &str,
        value: &str,
    ) -> Result<MacroManipulationResult, PreprocessError> {
        // Tokenize the value and create TokenWithLocation instances.
        let tokens = lex_from_str(value)?;
        let token_with_locations: Vec<TokenWithLocation> = tokens
            .into_iter()
            .map(|token_with_range| {
                let location = Location::new(FILE_NUMBER_PREDEFINED, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token, location)
            }) // Assuming line and column are 0 for simplicity
            .collect();

        if token_with_locations.len() > 1 {
            return Err(PreprocessError::Message(format!(
                "Expected a single value for key '{}', but got {} tokens.",
                key,
                token_with_locations.len()
            )));
        }

        let item = MacroDefinition::ObjectLike(token_with_locations);

        Ok(match self.macros.insert(key.to_owned(), item) {
            Some(_) => MacroManipulationResult::Failure, // Key was already present
            None => MacroManipulationResult::Success,    // Key was added successfully
        })
    }

    /// Adds a definition by a key and a list of tokens with ranges.
    /// Returns `Ok(false)` if the key was already present.
    pub fn add_object_like(
        &mut self,
        file_number: usize,
        key: &str,
        definition: &[TokenWithRange],
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
            Some(_) => MacroManipulationResult::Failure, // Key was already present
            None => MacroManipulationResult::Success,    // Key was added successfully
        }
    }

    pub fn add_function_like(
        &mut self,
        file_number: usize,
        key: &str,
        parameters: &[String],
        definition: &[TokenWithRange],
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
            Some(_) => MacroManipulationResult::Failure, // Key was already present
            None => MacroManipulationResult::Success,    // Key was added successfully
        }
    }

    pub fn get(&self, key: &str) -> Option<&MacroDefinition> {
        self.macros.get(key)
    }

    pub fn contains(&self, key: &str) -> bool {
        self.macros.contains_key(key)
    }

    /// Remove the definition for the given key.
    /// Returns `true` if the key was present and removed, `false` otherwise.
    pub fn remove(&mut self, key: &str) -> MacroManipulationResult {
        match self.macros.remove(key) {
            Some(_) => MacroManipulationResult::Success, // Key was present and removed
            None => MacroManipulationResult::Failure,    // Key was not present
        }
    }
}
