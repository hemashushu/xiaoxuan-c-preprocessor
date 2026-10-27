// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::{HashMap, HashSet},
    path::{Component, Path, PathBuf},
};

use crate::{
    ast::{FunctionParameter, Program},
    consts::FILE_NUMBER_PREDEFINED,
    error::PreprocessError,
    lexer::lex_from_clean_str,
    linter::Lint,
    location::Location,
    token::{Token, TokenWithLocation, TokenWithRange},
};

/// The `Context` struct holds all state required during preprocessing.
/// It manages file access, macro definitions, file inclusion tracking,
/// and lint messages.
pub struct Context<'a, T>
where
    T: FileProvider,
{
    /// The file provider used to load source and header files.
    pub file_provider: &'a T,

    /// Parsed included files, cached to avoid repeated parsing across runs.
    /// Included files are cached regardless of extension; the main file is not.
    pub header_file_cache: &'a mut HeaderFileCache,

    /// Identifiers that cannot be defined as macros.
    /// Callers can pass `consts::C23_KEYWORD_STRS` to reserve selected keywords.
    pub reserved_identifiers: &'a [&'a str],

    pub compile_features: &'a HashMap<String, bool>,
    pub suppress_lints: &'a HashSet<String>,

    /// Macro definitions and related state.
    pub macro_map: MacroMap,

    /// The number and location of the file currently being processed.
    pub current_file_item: FileItem,

    /// Files already included in this run, including the main file, to prevent
    /// repeated inclusion and direct or indirect self-inclusion.
    pub included_files: Vec<FileLocation>,

    /// User-facing messages, warnings, or information.
    pub lints: Vec<Lint>,

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
        compile_features: &'a HashMap<String, bool>,
        suppress_lints: &'a HashSet<String>,
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
            compile_features,
            suppress_lints,
            current_file_item,
            macro_map: MacroMap::new(),
            included_files: Vec::new(),
            lints: Vec::new(),
            output: Vec::new(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub fn from_predefinitions(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        compile_features: &'a HashMap<String, bool>,
        suppress_lints: &'a HashSet<String>,
        predefinitions: &HashMap<String, String>,
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
            compile_features,
            suppress_lints,
            macro_map: MacroMap::from_key_values(predefinitions)?,
            current_file_item,
            included_files: Vec::new(),
            lints: Vec::new(),
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

/// Records how a file was supplied or found, along with its display path.
#[derive(Debug, PartialEq, Clone)]
pub enum FilePathSource {
    /// The translation unit's main file, such as `main.c`.
    /// The caller supplies its display path, usually relative to the project root.
    SourceFile(/* relative_file_path */ PathBuf),

    /// A header resolved through a user search path or relative to the current file.
    /// Stores the path used by the inclusion directive, e.g., `my_header.h`.
    UserHeader(/* relative_file_path */ PathBuf),

    /// A header resolved through a system search path, including quoted inclusions
    /// that fall back to system directories. Stores the requested header path.
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
    FunctionLike(
        // Parameters; the final entry is `FunctionParameter::Variadic` for `...`.
        Vec<FunctionParameter>,
        // The macro definition tokens with locations.
        Vec<TokenWithLocation>,
    ),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AddMacroResult {
    Success,
    AlreadyExist,
    IdenticalDefinitionAlreadyExist, // macro type and definition are identical to an existing one
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum RemoveMacroResult {
    Success,
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

    /// Resolves a quoted header or resource path using ANCPP's fallback search.
    ///
    /// The resolution process is as follows:
    /// 1. If `enable_include_file_path_relative_to_current_file` is true, search
    ///    relative to the file containing the directive.
    /// 2. If not found, search the user include directories.
    /// 3. If still not found, search the system include directories.
    ///
    /// Returns the canonical path if found, or `None` if not found.
    fn resolve_user_file_with_fallback(
        &self,
        target_file_relative_path: &Path,
        source_file_canonical_full_path: &Path,
        enable_include_file_path_relative_to_current_file: bool,
    ) -> Option<FilePathResolveResult> {
        if enable_include_file_path_relative_to_current_file
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

        // Byte offset at which to start reading.
        offset: usize,

        // Maximum bytes to read from the offset, or all remaining bytes if None.
        // Reading stops at the end of the file even if the limit is larger.
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
    pub token_with_locations: Vec<TokenWithLocation>,
    pub lints: Vec<Lint>,
}

/// Normalizes path components without accessing the filesystem.
///
/// This function normalizes the path by resolving components like `.` and `..`,
/// but does not resolve symbolic links or check if the path exists on the file system.
/// Returns a new `PathBuf` with the normalized path.
pub fn normalize_path(src: &Path) -> PathBuf {
    let mut output = PathBuf::new();

    let components = src.components();
    for component in components {
        match component {
            Component::Prefix(disk) => {
                output.push(disk.as_os_str());
                // unimplemented!("todo: handle prefix components"),
            }
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

/// A cache of parsed included files, reusable across preprocessing runs.
///
/// Stores the canonical path, source code, and the parsed AST for each file.
/// This cache prevents redundant parsing of the same file multiple times.
///
/// Included files are cached regardless of extension; main source files are not.
pub struct HeaderFileCache {
    items: Vec<HeaderFileCacheItem>,
}

pub struct HeaderFileCacheItem {
    pub canonical_full_path: PathBuf,

    /// Source text retained alongside the parsed program for diagnostics.
    pub text_content: String,

    pub file_number: usize,
    pub program: Program,
}

impl HeaderFileCache {
    /// Creates an empty `HeaderFileCache`.
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Returns the next header file number, starting at 1.
    /// The processor uses this when adding entries; 0 is reserved for predefinitions.
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

    /// Returns all cache entries, including their file numbers, paths, source text,
    /// and parsed programs. A reused cache also contains entries from earlier runs.
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
                == AddMacroResult::AlreadyExist
            {
                return Err(PreprocessError::Message(format!(
                    "Macro '{}' is already exist.",
                    key
                )));
            }
        }
        Ok(macros)
    }

    /// Adds an object-like macro from a name and a replacement in C source notation.
    ///
    /// For example:
    ///
    /// - `"123"` represents a number
    /// - `"\"abc\""` represents a string literal.
    ///
    /// The replacement must be empty or contain exactly one token, such as:
    ///
    /// - a number
    /// - a character
    /// - a string
    /// - an identifier
    /// - an empty replacement (i.e., `""`)
    ///
    /// Returns `AddMacroResult::AlreadyExist` if the name was already present.
    fn add_object_like_by_single_value_string(
        &mut self,
        key: &str,
        value: &str,
    ) -> Result<AddMacroResult, PreprocessError> {
        // Tokenize the value and create TokenWithLocation instances.
        let tokens = lex_from_clean_str(value)?;
        let token_with_locations: Vec<TokenWithLocation> = tokens
            .into_iter()
            .map(|token_with_range| {
                let location = Location::new(FILE_NUMBER_PREDEFINED, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token, location)
            })
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
            Some(_) => AddMacroResult::AlreadyExist, // Key was already present
            None => AddMacroResult::Success,         // Key was added successfully
        })
    }

    /// Adds an object-like macro from unexpanded replacement tokens.
    /// Returns `AddMacroResult::AlreadyExist` for a conflicting definition or
    /// `AddMacroResult::IdenticalDefinitionAlreadyExist` for an equivalent one.
    pub fn add_object_like(
        &mut self,
        file_number: usize,
        key: &str,
        definition: &[TokenWithRange], // Unexpanded tokens with ranges.
    ) -> AddMacroResult {
        let token_with_locations: Vec<TokenWithLocation> = definition
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        match self.macros.get(key) {
            Some(existing_definition) => {
                if let MacroDefinition::ObjectLike(existing_token_with_locations) =
                    existing_definition
                {
                    if is_object_like_macro_equal(
                        &token_with_locations,
                        existing_token_with_locations,
                    ) {
                        AddMacroResult::IdenticalDefinitionAlreadyExist
                    } else {
                        AddMacroResult::AlreadyExist
                    }
                } else {
                    AddMacroResult::AlreadyExist
                }
            }
            None => {
                // Key was not present, add the new definition
                let macro_definition = MacroDefinition::ObjectLike(token_with_locations);
                self.macros.insert(key.to_owned(), macro_definition);
                AddMacroResult::Success
            }
        }
    }

    /// Adds a function-like macro from parameters and unexpanded replacement tokens.
    /// Returns `AddMacroResult::AlreadyExist` for a conflicting definition or
    /// `AddMacroResult::IdenticalDefinitionAlreadyExist` for an equivalent one.
    pub fn add_function_like(
        &mut self,
        file_number: usize,
        key: &str,
        parameters: &[FunctionParameter],
        definition: &[TokenWithRange], // Unexpanded tokens with ranges.
    ) -> AddMacroResult {
        let token_with_locations: Vec<TokenWithLocation> = definition
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        match self.macros.get(key) {
            Some(existing_definition) => {
                if let MacroDefinition::FunctionLike(
                    existing_parameters,
                    existing_token_with_locations,
                ) = existing_definition
                {
                    if is_function_like_macro_equal(
                        parameters,
                        &token_with_locations,
                        existing_parameters,
                        existing_token_with_locations,
                    ) {
                        AddMacroResult::IdenticalDefinitionAlreadyExist
                    } else {
                        AddMacroResult::AlreadyExist
                    }
                } else {
                    AddMacroResult::AlreadyExist
                }
            }
            None => {
                // Key was not present, add the new definition
                let macro_definition =
                    MacroDefinition::FunctionLike(parameters.to_vec(), token_with_locations);
                self.macros.insert(key.to_owned(), macro_definition);
                AddMacroResult::Success
            }
        }
    }

    pub fn get(&self, key: &str) -> Option<&MacroDefinition> {
        self.macros.get(key)
    }

    /// Removes the definition for the given key.
    /// Returns:
    /// - `RemoveMacroResult::Success` if the key was present and removed.
    /// - `RemoveMacroResult::NotFound` otherwise.
    pub fn remove(&mut self, key: &str) -> RemoveMacroResult {
        match self.macros.remove(key) {
            Some(_) => RemoveMacroResult::Success, // Key was present and removed
            None => RemoveMacroResult::NotFound,   // Key was not present
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

fn is_object_like_macro_equal(
    tokens_left: &[TokenWithLocation],
    tokens_right: &[TokenWithLocation],
) -> bool {
    if tokens_left.len() != tokens_right.len() {
        return false;
    }

    for (token1, token2) in tokens_left.iter().zip(tokens_right.iter()) {
        if token1.token != token2.token {
            return false;
        }
    }

    true
}

fn is_function_like_macro_equal(
    parameters_left: &[FunctionParameter],
    tokens_left: &[TokenWithLocation],
    parameters_right: &[FunctionParameter],
    tokens_right: &[TokenWithLocation],
) -> bool {
    if parameters_left.len() != parameters_right.len() {
        return false;
    }

    if tokens_left.len() != tokens_right.len() {
        return false;
    }

    for (param1, param2) in parameters_left.iter().zip(parameters_right.iter()) {
        if param1 == &FunctionParameter::Variadic && param2 != &FunctionParameter::Variadic {
            return false;
        }
    }

    for (token1, token2) in tokens_left.iter().zip(tokens_right.iter()) {
        if let TokenWithLocation {
            token: Token::Identifier(id1),
            ..
        } = &token1
        {
            let id2 = if let TokenWithLocation {
                token: Token::Identifier(id2),
                ..
            } = &token2
            {
                id2
            } else {
                return false;
            };

            let pos1 = parameters_left.iter().position(
                |param| matches!(param, FunctionParameter::Identifier(name) if name == id1),
            );

            let pos2 = parameters_right.iter().position(
                |param| matches!(param, FunctionParameter::Identifier(name) if name == id2),
            );

            if pos1 != pos2 {
                return false;
            }
        }
    }

    true
}
