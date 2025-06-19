// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::collections::HashMap;

use crate::{
    FILE_NUMBER_PREDEFINED, PreprocessError, TokenWithLocation, TokenWithRange,
    lexer::lex_from_str, location::Location,
};

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
