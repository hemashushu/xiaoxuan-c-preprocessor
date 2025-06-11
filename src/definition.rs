// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::collections::HashMap;

use crate::{
    PreprocessError, TokenWithLocation, TokenWithRange, lexer::lex_from_str, location::Location,
};

pub const PREDEFINED_DEFINITION_FILE_NUMBER: usize = 0;

pub struct Definition {
    items: HashMap<String, DefinitionItem>,
}

pub enum DefinitionItem {
    ObjectLike(Vec<TokenWithLocation>),
    FunctionLike(Vec<String>, Vec<TokenWithLocation>),
}

impl Definition {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn from_key_values(
        predefinitions: &HashMap<String, String>,
    ) -> Result<Self, PreprocessError> {
        let mut definition = Self::new();
        for (key, value) in predefinitions {
            if !definition.add_by_key_value(key, value)? {
                return Err(PreprocessError::Message(format!(
                    "Duplicate definition for key '{}'",
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
    pub fn add_by_key_value(&mut self, key: &str, value: &str) -> Result<bool, PreprocessError> {
        // Tokenize the value and create TokenWithLocation instances.
        let tokens = lex_from_str(value)?;
        let token_with_locations: Vec<TokenWithLocation> = tokens
            .into_iter()
            .map(|token_with_range| {
                let location =
                    Location::new(PREDEFINED_DEFINITION_FILE_NUMBER, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token, location)
            }) // Assuming line and column are 0 for simplicity
            .collect();

        if token_with_locations.len() > 1 {
            return Err(PreprocessError::Message(format!(
                "Expected a single token for key '{}', but got {} tokens.",
                key,
                token_with_locations.len()
            )));
        }

        let item = DefinitionItem::ObjectLike(token_with_locations);

        Ok(self.items.insert(key.to_owned(), item).is_none())
    }

    /// Adds a definition by a key and a list of tokens with ranges.
    /// Returns `Ok(false)` if the key was already present.
    pub fn add_object_like(
        &mut self,
        file_number: usize,
        key: &str,
        token_with_ranges: &[TokenWithRange],
    ) -> Result</* success */ bool, PreprocessError> {
        let token_with_locations: Vec<TokenWithLocation> = token_with_ranges
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        let item = DefinitionItem::ObjectLike(token_with_locations);

        Ok(self.items.insert(key.to_owned(), item).is_none())
    }

    pub fn add_function_like(
        &mut self,
        file_number: usize,
        key: &str,
        parameters: &[String],
        token_with_ranges: &[TokenWithRange],
    ) -> Result</* success */ bool, PreprocessError> {
        let token_with_locations: Vec<TokenWithLocation> = token_with_ranges
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        let item = DefinitionItem::FunctionLike(parameters.to_vec(), token_with_locations);

        Ok(self.items.insert(key.to_owned(), item).is_none())
    }

    pub fn get(&self, key: &str) -> Option<&DefinitionItem> {
        self.items.get(key)
    }

    pub fn contains(&self, key: &str) -> bool {
        self.items.contains_key(key)
    }

    /// Remove the definition for the given key.
    /// Returns `true` if the key was present and removed, `false` otherwise.
    pub fn remove(&mut self, key: &str) -> bool {
        self.items.remove(key).is_some()
    }
}
