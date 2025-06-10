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
    items: HashMap<String, Vec<TokenWithLocation>>,
}

impl Definition {
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn from_keyvalues(
        predefinitions: &HashMap<String, String>,
    ) -> Result<Self, PreprocessError> {
        let mut items = HashMap::new();
        for (key, value) in predefinitions {
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

            items.insert(key.to_owned(), token_with_locations);
        }

        Ok(Self { items })
    }

    /// Adds a definition by a key and a value string.
    /// Returns `Ok(false)` if the key was already present and the value was updated,
    /// or `Ok(true)` if the key was not present and a new entry was created.
    pub fn add_by_string(&mut self, key: &str, value: &str) -> Result<bool, PreprocessError> {
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

        Ok(self
            .items
            .insert(key.to_owned(), token_with_locations)
            .is_none())
    }

    pub fn add_by_token(
        &mut self,
        key: &str,
        file_number: usize,
        token_with_ranges: &[TokenWithRange],
    ) -> Result</* is_duplicated */ bool, PreprocessError> {
        let token_with_locations: Vec<TokenWithLocation> = token_with_ranges
            .iter()
            .map(|token_with_range| {
                let location = Location::new(file_number, &token_with_range.range);
                TokenWithLocation::new(token_with_range.token.clone(), location)
            })
            .collect();

        Ok(self
            .items
            .insert(key.to_owned(), token_with_locations)
            .is_some())
    }

    pub fn get(&self, key: &str) -> Option<&[TokenWithLocation]> {
        self.items.get(key).map(|e| &e[..])
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
