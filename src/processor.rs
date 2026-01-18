// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use chrono::Local;

use crate::{
    PreprocessError, PreprocessFileError, TokenWithLocation, TokenWithRange,
    ast::{Branch, Condition, Define, Embed, If, Include, Pragma, Program, Statement},
    context::{Context, FileItem, FileLocation, FileOrigin},
    expression::evaluate_token_with_locations,
    file_provider::{FileProvider, ResolvedResult},
    header_file_cache::HeaderFileCache,
    location::Location,
    macro_map::{MacroDefinition, MacroManipulationResult},
    parser::parse_from_str,
    peekable_iter::PeekableIter,
    prompt::{Prompt, PromptLevel},
    range::Range,
    token::{IntegerNumber, IntegerNumberLength, Number, Punctuator, StringEncoding, Token},
};

const PEEK_BUFFER_LENGTH_PARSE_CODE: usize = 2;
const PEEK_BUFFER_LENGTH_CONCATENATE_STRING: usize = 2;

/// Preprocesses a C source file.
///
/// see also:
/// - https://en.cppreference.com/w/c/language.html
/// - https://en.cppreference.com/w/c/preprocessor.html
/// - https://en.cppreference.com/w/c/language/translation_phases.html
#[allow(clippy::too_many_arguments)]
pub fn process_source_file<T>(
    file_provider: &T,
    file_cache: &mut HeaderFileCache,

    // Keywords which are used to prevent defining macros with these names.
    // Such as `int`, `return`, `if`, `else`, etc.
    reserved_identifiers: &[&str],

    // The predefined macros to be used during preprocessing.
    // Such as `__STDC__` and `__STDC_VERSION__`, they are provided by the compiler.
    // see:
    // - https://gcc.gnu.org/onlinedocs/cpp/Standard-Predefined-Macros.html
    // - https://en.cppreference.com/w/c/preprocessor/replace.html
    predefinitions: &HashMap<String, String>,

    // A flag to control whether to resolve relative paths within the current source file.
    //
    // Set to true to resolve relative paths to the source file,
    // otherwise, preprocessor will only search in the specified including directories.
    //
    // For example, consider there are 3 defined include directories:
    //
    // - `./include`
    // - `./src/headers`
    // - `/usr/include`
    //
    // The statement `#include "foo.h"` in the source file `src/main.c` will try to
    // match `foo.h` in the following order:
    //
    // - `./include/foo.h`
    // - `./src/headers/foo.h`
    // - `/usr/include/foo.h`
    //
    // If `resolve_relative_path_within_current_file` is true, then it will also
    // try to match `src/foo.h`.
    resolve_relative_path_within_current_file: bool,

    // The number of the source file, used for error reporting.
    // It should begin with `FILE_NUMBER_SOURCE_FILE_BEGIN` (which default to 65536)
    // and incremented by 1 for each source file.
    //
    // Note that the term "source file" refer only to the C source file (e.g. `main.c`),
    // it does not include the header files (e.g. `header.h`).
    source_file_number: usize,

    // The path name of the source file.
    // It is used to generate the value of the `__FILE__` macro and the parser wouldn't use it to load the file.
    // Usually, it's value is the relative path to the project root directory.
    source_file_path_name: &Path,

    // The canonical full path of the source file.
    // This is used to load the source file content actually.
    source_file_canonical_full_path: &Path,
) -> Result<PreprocessResult, PreprocessFileError>
where
    T: FileProvider,
{
    let mut processor = Processor::new(
        file_provider,
        file_cache,
        reserved_identifiers,
        predefinitions,
        resolve_relative_path_within_current_file,
        source_file_number,
        source_file_path_name,
        source_file_canonical_full_path,
    )?;

    // Add source file to the `included_files` list to
    // prevent recursive inclusion.
    processor.context.included_files.push(FileLocation::new(
        source_file_canonical_full_path,
        FileOrigin::from_source_file(source_file_path_name),
    ));

    // Load and parse the source file.
    let src = file_provider
        .load_text_file(source_file_canonical_full_path)
        .map_err(|error| {
            PreprocessFileError::new(
                source_file_number,
                PreprocessError::Message(format!(
                    "Failed to load file '{}': {}",
                    source_file_canonical_full_path.to_string_lossy(),
                    error
                )),
            )
        })?;

    let program = parse_from_str(&src)
        .map_err(|error| PreprocessFileError::new(source_file_number, error))?;

    processor.process_program(&program)?;

    let Context {
        prompts, output, ..
    } = processor.context;

    // concatenates adjacent string literals
    let mut iter = output.into_iter();
    let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CONCATENATE_STRING);
    let concatenated = concatenate_adjacent_strings(&mut peekable_iter)?;

    let result = PreprocessResult {
        output: concatenated,
        prompts,
    };

    Ok(result)
}

#[derive(Debug, PartialEq)]
pub struct PreprocessResult {
    pub output: Vec<TokenWithLocation>,
    pub prompts: Vec<Prompt>,
}

struct Processor<'a, T>
where
    T: FileProvider,
{
    context: Context<'a, T>,
}

impl<'a, T> Processor<'a, T>
where
    T: FileProvider,
{
    #[allow(clippy::too_many_arguments)]
    fn new(
        file_provider: &'a T,
        file_cache: &'a mut HeaderFileCache,
        reserved_identifiers: &'a [&'a str],
        predefinitions: &HashMap<String, String>,
        resolve_relative_path_within_current_file: bool,

        source_file_number: usize,
        source_file_path_name: &Path,
        source_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessFileError> {
        let context = Context::from_keyvalues(
            file_provider,
            file_cache,
            reserved_identifiers,
            predefinitions,
            resolve_relative_path_within_current_file,
            source_file_number,
            source_file_path_name,
            source_file_canonical_full_path,
        )
        .map_err(|error| PreprocessFileError {
            file_number: source_file_number,
            error,
        })?;

        Ok(Self { context })
    }
}

impl<'a, T> Processor<'a, T>
where
    T: FileProvider,
{
    fn process_program(&mut self, program: &Program) -> Result<(), PreprocessFileError> {
        for statement in &program.statements {
            self.process_statement(statement)?
        }

        Ok(())
    }

    fn process_statement(&mut self, statement: &Statement) -> Result<(), PreprocessFileError> {
        match statement {
            Statement::Pragma(pragma) => self.process_pragma(pragma),
            Statement::Define(define) => self.process_define(define),
            Statement::Undef(identifier, range) => self.process_undefine(identifier, range),
            Statement::Include(include) => self.process_include(include),
            Statement::Embed(embed) => self.process_embed(embed),
            Statement::If(if_) => self.process_if(if_),
            Statement::Error(message, range) => self.process_error(message, range),
            Statement::Warning(message, range) => self.process_warnning(message, range),
            Statement::Code(token_with_ranges) => self.process_code(token_with_ranges),
        }
    }

    fn process_pragma(&mut self, _pragma: &Pragma) -> Result<(), PreprocessFileError> {
        // Supported pragmas are:
        // - `#pragma once`: Include guard to prevent multiple inclusions of the same file.
        // - `#pragma STDC FENV_ACCESS arg`: Where arg is either ON or OFF or DEFAULT
        // - `#pragma STDC FP_CONTRACT arg`
        // - `#pragma STDC CX_LIMITED_RANGE arg`

        // Pragmas are compiler-specific options, not preprocessor directives.
        // They should ideally be passed to the compiler for handling.
        // However, I have not yet found a suitable way to do this.
        // todo

        Ok(())
    }

    fn process_code(
        &mut self,
        token_with_ranges: &[TokenWithRange],
    ) -> Result<(), PreprocessFileError> {
        let token_with_locations = token_with_ranges
            .iter()
            .map(|token_with_range| {
                TokenWithLocation::new(
                    token_with_range.token.clone(),
                    Location::new(
                        self.context.current_file_item.number,
                        &token_with_range.range,
                    ),
                )
            })
            .collect::<Vec<_>>();

        let expanded_tokens = self.expand_marco(
            token_with_locations,
            &HashMap::new(),
            ExpandContextType::Normal,
        )?;

        self.context.output.extend(expanded_tokens);

        Ok(())
    }

    fn process_define(&mut self, define: &Define) -> Result<(), PreprocessFileError> {
        match define {
            Define::ObjectLike {
                identifier: (name, range),
                definition,
            } => {
                if name == "defined" {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            "The identifier 'defined' cannot be used as a macro name.".to_string(),
                            *range,
                        ),
                    });
                }

                if self.context.reserved_identifiers.contains(&name.as_str()) {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            format!(
                                "The identifier '{}' is reversed and cannot be used as a macro name.",
                                name
                            ),
                            *range,
                        ),
                    });
                }

                let add_result = self.context.macro_map.add_object_like(
                    self.context.current_file_item.number,
                    name,
                    definition,
                );

                if add_result == MacroManipulationResult::Failure {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            format!("Macro '{}' is already defined.", name),
                            *range,
                        ),
                    });
                }
            }
            Define::FunctionLike {
                identifier: (name, range),
                parameters,
                definition,
            } => {
                if name == "defined" {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            "The identifier 'defined' cannot be used as a macro name.".to_string(),
                            *range,
                        ),
                    });
                }

                if self.context.reserved_identifiers.contains(&name.as_str()) {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            format!(
                                "The identifier '{}' is reversed and cannot be used as a macro name.",
                                name
                            ),
                            *range,
                        ),
                    });
                }

                let result = self.context.macro_map.add_function_like(
                    self.context.current_file_item.number,
                    name,
                    parameters,
                    definition,
                );

                if result == MacroManipulationResult::Failure {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            format!("Macro '{}' is already defined.", name),
                            *range,
                        ),
                    });
                }
            }
        }

        Ok(())
    }

    fn process_undefine(
        &mut self,
        identifier: &str,
        range: &Range,
    ) -> Result<(), PreprocessFileError> {
        // if identifier == "defined" {
        //     return Err(PreprocessFileError {
        //         file_number: self.context.current_file_item.number,
        //         error: PreprocessError::MessageWithRange(
        //             "The identifier 'defined' cannot be used as a macro name.".to_string(),
        //             *range,
        //         ),
        //     });
        // }

        // if self.context.reserved_identifiers.contains(&identifier) {
        //     return Err(PreprocessFileError {
        //         file_number: self.context.current_file_item.number,
        //         error: PreprocessError::MessageWithRange(
        //             format!(
        //                 "The identifier '{}' is a C keyword and cannot be undefined.",
        //                 identifier
        //             ),
        //             *range,
        //         ),
        //     });
        // }

        let result = self.context.macro_map.remove(identifier);

        if result == MacroManipulationResult::Failure {
            return Err(PreprocessFileError {
                file_number: self.context.current_file_item.number,
                error: PreprocessError::MessageWithRange(
                    format!("Macro '{}' is not defined.", identifier),
                    *range,
                ),
            });
        }

        Ok(())
    }

    fn process_include(&mut self, include: &Include) -> Result<(), PreprocessFileError> {
        // The `include` directive is used to include the contents of another file.
        // The file can be a user header or a system header.

        let (relative_path, stick_to_system, file_path_location) = match include {
            Include::Identifier(id, range) => {
                let expended_tokens = self.expand_marco(
                    vec![TokenWithLocation::new(
                        Token::Identifier(id.to_owned()),
                        Location::new(self.context.current_file_item.number, range),
                    )],
                    &HashMap::new(),
                    ExpandContextType::Normal,
                )?;

                if expended_tokens.is_empty() {
                    return Err(PreprocessFileError {
                    file_number: self.context.current_file_item.number,
                    error: PreprocessError::MessageWithRange(
                        "The macro expands to empty; expected a single string literal representing the file path.".to_owned(),
                        *range,
                    ),
                });
                }

                if expended_tokens.len() > 1 {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            "The macro expands to multiple tokens; expected a single string literal representing the file path."
                                .to_owned(),
                            *range,
                        ),
                    });
                }

                match expended_tokens.first().unwrap() {
                    TokenWithLocation {
                        token: Token::String(relative_path, _),
                        location,
                    } => (PathBuf::from(relative_path), false, *location),
                    TokenWithLocation { location, .. } => {
                        return Err(PreprocessFileError {
                        file_number: location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Macro expansion resulted in an unexpected type; a single string literal representing the file path was expected.".to_owned(),
                            location.range,
                        ),
                    });
                    }
                }
            }
            Include::FilePath {
                file_path: (relative_path, range),
                is_system_header,
            } => (
                PathBuf::from(relative_path),
                *is_system_header,
                Location::new(self.context.current_file_item.number, range),
            ),
        };

        let (canonical_path, is_system_header) = if stick_to_system {
            match self
                .context
                .file_provider
                .resolve_system_file(&relative_path)
            {
                Some(resolved_path) => (resolved_path, true),
                None => {
                    return Err(PreprocessFileError::new(
                        file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "System header file '{}' not found.",
                                relative_path.to_string_lossy()
                            ),
                            file_path_location.range,
                        ),
                    ));
                }
            }
        } else {
            match self.context.file_provider.resolve_user_file_with_fallback(
                &relative_path,
                &self
                    .context
                    .current_file_item
                    .file_location
                    .canonical_full_path,
                self.context.resolve_relative_path_within_current_file,
            ) {
                Some(ResolvedResult {
                    canonical_full_path,
                    is_system_file,
                }) => (canonical_full_path, is_system_file),
                None => {
                    return Err(PreprocessFileError::new(
                        file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "Header file '{}' not found.",
                                relative_path.to_string_lossy()
                            ),
                            file_path_location.range,
                        ),
                    ));
                }
            }
        };

        // Check if the file is already included.
        if self.context.contains_include_file(&canonical_path) {
            // If the file is already included, we skip it.
            return Ok(());
        }

        // add the file to the included files to prevent multiple inclusions.
        // Note that ANCPP only includes header files once, even if the header file
        // has no include guards or `#pragma once`.
        self.context.included_files.push(FileLocation::new(
            &canonical_path,
            if is_system_header {
                FileOrigin::from_system_header_file(&relative_path)
            } else {
                FileOrigin::from_user_header_file(&relative_path)
            },
        ));

        // Load the file content from the file cache.
        if let Some(program) = self.context.header_file_cache.get_program(&canonical_path) {
            let file_number = self
                .context
                .header_file_cache
                .get_file_number(&canonical_path)
                .unwrap();
            let program_owned = program.clone();

            self.processing_sub_program(
                file_number,
                &canonical_path,
                &relative_path,
                is_system_header,
                &program_owned,
            )?;
        } else {
            // Load the file content.
            let file_content = self
                .context
                .file_provider
                .load_text_file(&canonical_path)
                .map_err(|error| {
                    PreprocessFileError::new(
                        file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!("Failed to load header file: {}", error),
                            file_path_location.range,
                        ),
                    )
                })?;

            // add the file to the cache
            let file_number = self
                .context
                .header_file_cache
                .add(&canonical_path, &file_content);

            // Parse the file content to get the program.
            let program = parse_from_str(&file_content)
                .map_err(|error| PreprocessFileError::new(file_number, error))?;

            // update cache
            self.context
                .header_file_cache
                .set_program(&canonical_path, program.clone());

            // check header guard or `#pragma once`
            self.check_include_guard(file_number, &relative_path, &program);

            self.processing_sub_program(
                file_number,
                &canonical_path,
                &relative_path,
                is_system_header,
                &program,
            )?;
        }

        Ok(())
    }

    fn processing_sub_program(
        &mut self,
        file_number: usize,
        canonical_path: &Path,
        relative_path: &Path,
        is_system_header: bool,
        program: &Program,
    ) -> Result<(), PreprocessFileError> {
        // Preprocess the program in the context of the included file.
        // This will handle any macros, directives, and other preprocessing tasks.

        // store the current context file as `last_context_file`
        let last_context_file = self.context.current_file_item.clone();

        // update the current context file to the included file
        let current_context_file = FileItem::new(
            file_number,
            FileLocation::new(
                canonical_path,
                if is_system_header {
                    FileOrigin::from_system_header_file(relative_path)
                } else {
                    FileOrigin::from_user_header_file(relative_path)
                },
            ),
        );
        self.context.current_file_item = current_context_file;

        // process the program
        self.process_program(program)?;

        // restore the `last_context_file`
        self.context.current_file_item = last_context_file;

        Ok(())
    }

    fn process_embed(&mut self, embed: &Embed) -> Result<(), PreprocessFileError> {
        // The `embed` directive output a sequence of `u8` numbers separated by commas,
        // which represent the bytes of the file.
        //
        // e.g.
        //
        // `#embed "path/to/file"` will output:
        //
        // ```diagram
        // 104, 105, 112, 112, 111          // decimal
        // or
        // 0x68, 0x69, 0x70, 0x70, 0x6F     // hexadecimal
        // ```
        //
        // The content in the parameters `prefix`, `suffix`, and `if_empty` are
        // output with the same format.

        let (
            relative_path,
            stack_to_system_file,
            file_path_location,
            limit,
            suffix,
            prefix,
            if_empty,
        ) = match embed {
            Embed::Identifier(id, range) => {
                let expended_tokens = self.expand_marco(
                    vec![TokenWithLocation::new(
                        Token::Identifier(id.to_owned()),
                        Location::new(self.context.current_file_item.number, range),
                    )],
                    &HashMap::new(),
                    ExpandContextType::Normal,
                )?;

                if expended_tokens.is_empty() {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            "The macro expands to empty; expected a single string literal representing the file path.".to_owned(),
                            *range,
                        ),
                    });
                }

                if expended_tokens.len() > 1 {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            "The macro expands to multiple tokens; expected a single string literal representing the file path."
                                .to_owned(),
                            *range,
                        ),
                    });
                }

                match expended_tokens.first().unwrap() {
                    TokenWithLocation {
                        token: Token::String(relative_path, _),
                        location,
                    } => (
                        PathBuf::from(relative_path),
                        false,
                        *location,
                        None,
                        vec![],
                        vec![],
                        None,
                    ),
                    TokenWithLocation { location, .. } => {
                        return Err(PreprocessFileError {
                            file_number: location.file_number,
                            error: PreprocessError::MessageWithRange(
                                "Macro expansion resulted in an unexpected type; a single string literal representing the file path was expected.".to_owned(),
                                location.range,
                            ),
                        });
                    }
                }
            }
            Embed::FilePath {
                file_path: (relative_path, range),
                is_system_file: is_system_header,
                limit,
                suffix,
                prefix,
                if_empty,
            } => (
                PathBuf::from(relative_path),
                *is_system_header,
                Location::new(self.context.current_file_item.number, range),
                *limit,
                suffix.to_vec(),
                prefix.to_vec(),
                if_empty.to_owned(),
            ),
        };

        let (canonical_path, _is_system_file) = if stack_to_system_file {
            match self
                .context
                .file_provider
                .resolve_system_file(&relative_path)
            {
                Some(resolved_path) => (resolved_path, true),
                None => {
                    return Err(PreprocessFileError::new(
                        file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "System binary file '{}' not found.",
                                relative_path.to_string_lossy()
                            ),
                            file_path_location.range,
                        ),
                    ));
                }
            }
        } else {
            match self.context.file_provider.resolve_user_file_with_fallback(
                &relative_path,
                &self
                    .context
                    .current_file_item
                    .file_location
                    .canonical_full_path,
                self.context.resolve_relative_path_within_current_file,
            ) {
                Some(ResolvedResult {
                    canonical_full_path,
                    is_system_file,
                }) => (canonical_full_path, is_system_file),
                None => {
                    return Err(PreprocessFileError::new(
                        file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "Binary file '{}' not found.",
                                relative_path.to_string_lossy()
                            ),
                            file_path_location.range,
                        ),
                    ));
                }
            }
        };

        let binary_data = self
            .context
            .file_provider
            .load_binary_file(&canonical_path, 0, limit)
            .map_err(|error| {
                PreprocessFileError::new(
                    file_path_location.file_number,
                    PreprocessError::MessageWithRange(
                        format!("Failed to load binary file: {}", error),
                        file_path_location.range,
                    ),
                )
            })?;

        let convert_binary_data_to_tokens = |data: &[u8]| -> Vec<TokenWithLocation> {
            data.iter()
                .map(|byte| {
                    TokenWithLocation::new(
                        Token::Number(Number::Integer(IntegerNumber::new(
                            format!("0x{:02x}", byte),
                            false,
                            IntegerNumberLength::Default,
                        ))),
                        Location::default(),
                    )
                })
                .flat_map(|token| {
                    vec![
                        token,
                        TokenWithLocation::new(
                            Token::Punctuator(Punctuator::Comma),
                            Location::default(),
                        ),
                    ]
                })
                .take(data.len() * 2 - 1) // Avoid trailing comma
                .collect()
        };

        if binary_data.is_empty() {
            if let Some(alternate_data) = if_empty {
                // If the file is empty, we output the `if_empty` content.
                let tokens = convert_binary_data_to_tokens(&alternate_data);
                self.context.output.extend(tokens);
            }
        } else {
            // Output the binary data as a sequence of numbers.
            if !prefix.is_empty() {
                self.context
                    .output
                    .extend(convert_binary_data_to_tokens(&prefix));
                self.context.output.push(TokenWithLocation::new(
                    Token::Punctuator(Punctuator::Comma),
                    Location::default(),
                ));
            }

            self.context
                .output
                .extend(convert_binary_data_to_tokens(&binary_data));

            if !suffix.is_empty() {
                self.context.output.push(TokenWithLocation::new(
                    Token::Punctuator(Punctuator::Comma),
                    Location::default(),
                ));
                self.context
                    .output
                    .extend(convert_binary_data_to_tokens(&suffix));
            }
        }

        Ok(())
    }

    fn process_if(&mut self, if_: &If) -> Result<(), PreprocessFileError> {
        let mut found = false;

        let extra_operator_names = ["__has_include", "__has_embed", "__has_c_attribute"];

        for branch in &if_.branches {
            // Evaluate the condition of the branch.
            let condition_result: isize = match &branch.condition {
                Condition::Defined(identifier, _) => {
                    if self.context.macro_map.contains(identifier)
                        || extra_operator_names.contains(&identifier.as_str())
                    {
                        1
                    } else {
                        0
                    }
                }
                Condition::NotDefined(identifier, _) => {
                    if self.context.macro_map.contains(identifier)
                        || extra_operator_names.contains(&identifier.as_str())
                    {
                        0
                    } else {
                        1
                    }
                }
                Condition::Expression(tokens) => {
                    // Evaluate the expression.
                    let token_with_locations = tokens
                        .iter()
                        .map(|item| {
                            TokenWithLocation::new(
                                item.token.clone(),
                                Location::new(self.context.current_file_item.number, &item.range),
                            )
                        })
                        .collect::<Vec<_>>();

                    let expanded_tokens = self.expand_marco(
                        token_with_locations,
                        &HashMap::new(),
                        ExpandContextType::ConditionalExpression,
                    )?;

                    evaluate_token_with_locations(
                        &expanded_tokens,
                        self.context.current_file_item.number,
                    )?
                }
            };

            if condition_result != 0 {
                found = true;

                for statement in &branch.consequence {
                    self.process_statement(statement)?;
                }
                break; // Exit after processing the first true branch.
            }
        }

        if !found {
            // If no branch was true, process the alternative if it exists.
            if let Some(alternative) = &if_.alternative {
                for statement in alternative {
                    self.process_statement(statement)?;
                }
            }
        }

        Ok(())
    }

    fn process_error(&mut self, message: &str, range: &Range) -> Result<(), PreprocessFileError> {
        Err(PreprocessFileError {
            file_number: self.context.current_file_item.number,
            error: PreprocessError::MessageWithRange(
                format!("User defined error: {}", message),
                *range,
            ),
        })
    }

    fn process_warnning(
        &mut self,
        message: &str,
        range: &Range,
    ) -> Result<(), PreprocessFileError> {
        let prompt = Prompt::MessageWithRange(
            PromptLevel::Warning,
            self.context.current_file_item.number,
            format!("User defined warning: {}", message),
            *range,
        );

        self.context.prompts.push(prompt);
        Ok(())
    }

    fn check_include_guard(
        &mut self,
        file_number_of_header_file: usize,
        relative_path: &Path,
        program: &Program,
    ) {
        // Check if the file has an include guard or `#pragma once`.
        // If it does, we can skip the preprocessing of the file.
        // Otherwise, we will preprocess the file normally.
        let first_statement_opt = program.statements.first();

        if first_statement_opt.is_none() {
            // File is empty
            self.context.prompts.push(Prompt::Message(
                PromptLevel::Warning,
                file_number_of_header_file,
                "Consider adding `#pragma once` to this file to prevent multiple inclusions."
                    .to_string(),
            ));
            return;
        }

        let first_statement = first_statement_opt.unwrap();

        if matches!(
            first_statement,
            Statement::Pragma(Pragma { components, .. })
            if matches!(
                components.first(),
                Some(TokenWithRange { token: Token::Identifier(name), .. }) if name == "once"))
        {
            // The file has `#pragma once`, we can skip further checks.
            return;
        }

        let suggested_include_guard_macro_name = relative_path
            .to_string_lossy()
            .replace(['/', '-', '.'], "_")
            .to_uppercase();

        // Check if the entire file consists of a single `#ifndef` structure,
        //
        // ```diagram
        // #ifndef INCLUDE_GUARD_MACRO_NAME
        // #define INCLUDE_GUARD_MACRO_NAME
        // ... file content ...
        // #endif
        // ```
        if program.statements.len() == 1
            && matches!(
                first_statement,
                Statement::If(If {
                    branches,
                    alternative: None
                }) if branches.len() == 1 &&
                matches!(
                    branches.first().unwrap(),
                    Branch {
                        condition: Condition::NotDefined(name0, _),
                        consequence
                    } if matches!(
                        consequence.first(),
                        Some(Statement::Define(Define::ObjectLike {
                            identifier: (name1, _),
                            definition
                        })) if name0 == name1 && definition.is_empty()
                    )
                )
            )
        {
            let branches = if let Statement::If(If { branches, .. }) = first_statement {
                branches
            } else {
                unreachable!()
            };

            let branch = branches.first().unwrap();

            let (name, range) = match &branch.condition {
                Condition::NotDefined(name, range) => (name, range),
                _ => unreachable!(),
            };

            // The file has an include guard.
            if name.ends_with(&suggested_include_guard_macro_name) {
                // The include guard macro name matches the suggested name.
                // Suggest using `#pragma once` instead.
                self.context.prompts.push(Prompt::Message(
                    PromptLevel::Info,
                    file_number_of_header_file,
                    "Consider using `#pragma once` instead of include guards.".to_owned(),
                ));
            } else {
                // The include guard macro name does not match the suggested name.
                // Suggest renaming the include guard macro to follow the convention, which
                // prevents potential conflicts with other macros.
                self.context.prompts.push(Prompt::MessageWithRange(
                    PromptLevel::Info,
                    file_number_of_header_file,
                    format!(
                        "Consider renaming the include guard macro to `{}` to follow the convention.",
                        suggested_include_guard_macro_name
                    ),
                    *range,
                ));
            }
        } else {
            // The file does not have an include guard or `#pragma once`.
            // We suggest adding `#pragma once`.
            self.context.prompts.push(Prompt::Message(
                PromptLevel::Warning,
                file_number_of_header_file,
                "Consider adding `#pragma once` to this file to prevent multiple inclusions."
                    .to_owned(),
            ));
        }
    }

    fn expand_marco(
        &mut self,

        // The tokens to expand.
        // They may be the "code statements", or the
        // definition (body) of a function-like macro.
        token_with_locations: Vec<TokenWithLocation>,

        // The arguments of function-like macros invocations.
        // The values should be the expanded tokens (which would not include any marcos).
        argument_map: &HashMap<String, ArgumentValue>,

        // Indicates the context in which this expansion occurs.
        // Some preprocessor operators are only valid in specific contexts:
        // - Token concatenation (`##`) is only allowed within macro definitions.
        // - Stringizing (`#`) is only allowed in function-like macros.
        // - `__VA_ARGS__` and `__VA_OPT__` are only valid in variadic function-like macros.
        //
        // Possible values:
        // - 0: Normal code statements.
        // - 1: Object-like macro definition.
        // - 2: Function-like macro definition.
        // - 3: Variadic function-like macro definition.
        expand_context_type: ExpandContextType,
    ) -> Result<Vec<TokenWithLocation>, PreprocessFileError> {
        let mut output = Vec::new();

        let mut iter = token_with_locations.into_iter();
        let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_PARSE_CODE);
        let mut code_parser =
            CodeParser::new(&mut peekable_iter, self.context.current_file_item.number);

        while let Some(current_token_with_location) = code_parser.next_token_with_location() {
            match &current_token_with_location.token {
                Token::Identifier(name) => {
                    match name.as_str() {
                        "__VA_ARGS__" => {
                            // `__VA_ARGS__` is a special macro that represents the variable arguments
                            // in function-like macros.
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/replace.html

                            if expand_context_type
                                != ExpandContextType::VariadicFunctionLikeDefinition
                            {
                                return Err(PreprocessFileError {
                                file_number: current_token_with_location.location.file_number,
                                error: PreprocessError::MessageWithRange(
                                    "__VA_ARGS__ can only be used in variadic function-like macros."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            });
                            }

                            if let Some(ArgumentValue::Concatenated(tokens)) =
                                argument_map.get("...")
                            {
                                // If there are variadic arguments, copy them into the output with comma separation.
                                let values = tokens.join(&TokenWithLocation::new(
                                    Token::Punctuator(Punctuator::Comma),
                                    Location::default(),
                                ));

                                // the `values` is a vector of argument values, which
                                // are already expanded tokens, so we can just extend the output with it.
                                output.extend(values);
                            }
                        }
                        "__VA_OPT__" => {
                            // `__VA_OPT__ ( content )` is replaced by content if __VA_ARGS__ is non-empty,
                            // and expands to nothing otherwise.

                            if expand_context_type
                                != ExpandContextType::VariadicFunctionLikeDefinition
                            {
                                return Err(PreprocessFileError {
                                file_number: current_token_with_location.location.file_number,
                                error: PreprocessError::MessageWithRange(
                                    "__VA_OPT__ can only be used in variadic function-like macros."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            });
                            }

                            // collect content
                            let mut content = Vec::new();
                            code_parser.expect_and_consume_opening_paren()?; // Consumes '('

                            while let Some(next_token_with_location) =
                                code_parser.next_token_with_location()
                            {
                                match &next_token_with_location.token {
                                    Token::Punctuator(Punctuator::ParenthesisClose) => {
                                        // If the next token is a closing parenthesis, we have reached the end of the content.
                                        break;
                                    }
                                    _ => {
                                        // Otherwise, we collect the token as part of the content.
                                        content.push(next_token_with_location);
                                    }
                                }
                            }

                            // Note that the closing parenthesis has already been consumed by the loop above.

                            let exist_variadic_args = matches!(
                                argument_map.get("..."),
                                Some(ArgumentValue::Concatenated(tokens)) if !tokens.is_empty()
                            );

                            if exist_variadic_args {
                                // If there are variadic arguments, we expand the content.
                                let expanded_content =
                                    self.expand_marco(content, argument_map, expand_context_type)?;
                                output.extend(expanded_content);
                            }
                        }
                        "__FILE__" => {
                            let file_path =
                                match &self.context.current_file_item.file_location.file_origin {
                                    FileOrigin::SourceFile(path_buf) => path_buf,
                                    FileOrigin::UserHeader(path_buf) => path_buf,
                                    FileOrigin::SystemHeader(path_buf) => path_buf,
                                };

                            // expands to the name of the current file, as a character string literal
                            let value = file_path.to_string_lossy().to_string();
                            let token = TokenWithLocation::new(
                                Token::String(value, StringEncoding::Default),
                                Location::default(),
                            );
                            output.push(token);
                        }
                        "__LINE__" => {
                            // expands to the source file line number, an integer constant
                            let value = current_token_with_location.location.range.start.line + 1;
                            let token = TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    value.to_string(),
                                    false,
                                    IntegerNumberLength::Default,
                                ))),
                                Location::default(),
                            );
                            output.push(token);
                        }
                        "__DATE__" => {
                            // expands to the date of translation, a character string literal of the form “Mmm dd yyyy”.
                            // The name of the month is as if generated by asctime() and
                            // the first character of “dd” is a space if the day of the month is less than 10
                            let now = Local::now();

                            // Format the date as "Mmm dd yyyy", see:
                            // https://docs.rs/chrono/latest/chrono/format/strftime/index.html
                            let date_string = now.format("%b %e %Y").to_string();
                            let token = TokenWithLocation::new(
                                Token::String(date_string, StringEncoding::Default),
                                Location::default(),
                            );
                            output.push(token);
                        }
                        "__TIME__" => {
                            // expands to the time of translation, a character string literal of the form “hh:mm:ss”,
                            // as in the time generated by asctime()
                            let now = Local::now();

                            // Format the time as "hh:mm:ss", see:
                            // https://docs.rs/chrono/latest/chrono/format/strftime/index.html
                            let time_string = now.format("%H:%M:%S").to_string();
                            let token = TokenWithLocation::new(
                                Token::String(time_string, StringEncoding::Default),
                                Location::default(),
                            );
                            output.push(token);
                        }
                        "__STDC_EMBED_NOT_FOUND__"
                        | "__STDC_EMBED_FOUND__"
                        | "__STDC_EMBED_EMPTY__" => {
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/replace.html
                            let value = match name.as_str() {
                                "__STDC_EMBED_NOT_FOUND__" => 0,
                                "__STDC_EMBED_FOUND__" => 1,
                                "__STDC_EMBED_EMPTY__" => 2,
                                _ => unreachable!(),
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    value.to_string(),
                                    false,
                                    IntegerNumberLength::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__Pragma" => {
                            if expand_context_type != ExpandContextType::ObjectLikeDefinition
                                || expand_context_type != ExpandContextType::FunctionLikeDefinition
                                || expand_context_type
                                    != ExpandContextType::VariadicFunctionLikeDefinition
                            {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "__Pragma can only be used in macro definitions."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            code_parser.expect_and_consume_opening_paren()?; // Consumes '('

                            let (_pragma_content, _location) = match code_parser.peek_token(0) {
                                Some(Token::String(content, _)) => {
                                    let content_owned = content.to_owned();
                                    let location = *code_parser.peek_location(0).unwrap();
                                    code_parser.next_token(); // consumes the string token

                                    (content_owned, location)
                                }
                                Some(_) => {
                                    let location = code_parser.peek_location(0).unwrap();
                                    return Err(PreprocessFileError {
                                        file_number: location.file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "__Pragma must be followed by a string literal."
                                                .to_owned(),
                                            location.range,
                                        ),
                                    });
                                }
                                None => {
                                    return Err(PreprocessFileError {
                                        file_number: current_token_with_location
                                            .location
                                            .file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "__Pragma must be followed by a string literal."
                                                .to_owned(),
                                            current_token_with_location.location.range,
                                        ),
                                    });
                                }
                            };

                            code_parser.expect_and_consume_closing_paren()?; // Consumes ')'

                            // Pragmas are compiler-specific options, not preprocessor directives.
                            // They should ideally be passed to the compiler for handling.
                            // However, I have not yet found a suitable way to do this.
                            // todo
                        }
                        "defined" => {
                            // `defined` is a preprocessor operator that checks if a macro is defined.
                            // It can be used in conditional expressions and as part of `#if` and `#elif`.
                            //
                            // Syntax:
                            // - `defined identifier`
                            // - `defined(identifier)`

                            if expand_context_type != ExpandContextType::ConditionalExpression {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "`defined` can only be used in conditional expressions."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            let has_parentheses = code_parser.peek_token_and_equals(
                                0,
                                &Token::Punctuator(Punctuator::ParenthesisOpen),
                            );

                            if has_parentheses {
                                code_parser.expect_and_consume_opening_paren()?;
                            }

                            let identifier = match code_parser.peek_token(0) {
                                Some(Token::Identifier(id)) => {
                                    let id_owned = id.to_owned();
                                    code_parser.next_token(); // consumes the identifier token

                                    id_owned
                                }
                                Some(_) => {
                                    let location = code_parser.peek_location(0).unwrap();
                                    return Err(PreprocessFileError {
                                        file_number: location.file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "`defined` must be followed by an identifier."
                                                .to_owned(),
                                            location.range,
                                        ),
                                    });
                                }
                                None => {
                                    return Err(PreprocessFileError {
                                        file_number: current_token_with_location
                                            .location
                                            .file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "`defined` must be followed by an identifier."
                                                .to_owned(),
                                            current_token_with_location.location.range,
                                        ),
                                    });
                                }
                            };

                            if has_parentheses {
                                code_parser.expect_and_consume_closing_paren()?; // Consumes ')'
                            }

                            let extra_operator_names =
                                ["__has_include", "__has_embed", "__has_c_attribute"];

                            let number = if self.context.macro_map.contains(&identifier)
                                || extra_operator_names.contains(&identifier.as_str())
                            {
                                1
                            } else {
                                0
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    number.to_string(),
                                    false,
                                    IntegerNumberLength::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__has_include" => {
                            // `__has_include` is a preprocessor operator that checks if a file can be included.
                            // It can be used in conditional expressions and as part of `#if` and `#elif`.
                            //
                            // Syntax:
                            // - `__has_include("file")`
                            // - `__has_include(<file>)`
                            // - `__has_include(file)`
                            //
                            // The __has_include expression evaluates to 1 if the search for the source file
                            // succeeds, and to ​0​ if the search fails.
                            //
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/include.html

                            if expand_context_type != ExpandContextType::ConditionalExpression {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "`__has_include` can only be used in conditional expressions."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            code_parser.expect_and_consume_opening_paren()?; // Consumes '('

                            let (relative_path, stick_to_system, _file_path_location) =
                                match code_parser.peek_token(0) {
                                    Some(Token::Identifier(id)) => {
                                        let id_owned = id.to_owned();
                                        let location = *code_parser.peek_location(0).unwrap();
                                        code_parser.next_token(); // consumes the identifier token

                                        let expended_tokens = self.expand_marco(
                                            vec![TokenWithLocation::new(
                                                Token::Identifier(id_owned),
                                                location,
                                            )],
                                            &HashMap::new(),
                                            ExpandContextType::Normal,
                                        )?;

                                        if expended_tokens.is_empty() {
                                            return Err(PreprocessFileError {
                                                file_number: location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "The macro expands to empty; expected a single string literal representing the file path.".to_owned(),
                                                    location.range,
                                                ),
                                            });
                                        }

                                        if expended_tokens.len() > 1 {
                                            return Err(PreprocessFileError {
                                                file_number: location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "The macro expands to multiple tokens; expected a single string literal representing the file path."
                                                        .to_owned(),
                                                    location.range,
                                                ),
                                            });
                                        }

                                        match expended_tokens.first().unwrap() {
                                            TokenWithLocation {
                                                token: Token::String(relative_path, _),
                                                location,
                                            } => (PathBuf::from(relative_path), false, *location),
                                            _ => {
                                                return Err(PreprocessFileError {
                                                    file_number: location.file_number,
                                                    error: PreprocessError::MessageWithRange(
                                                        "Macro expansion resulted in an unexpected type; a single string literal representing the file path was expected.".to_owned(),
                                                        location.range,
                                                    ),
                                                });
                                            }
                                        }
                                    }
                                    Some(Token::HeaderFile(relative_path, is_system_header)) => {
                                        let relative_path_owned = relative_path.to_owned();
                                        let is_system_header_owned = *is_system_header;
                                        let location = *code_parser.peek_location(0).unwrap();
                                        code_parser.next_token(); // consumes the file path token

                                        (
                                            PathBuf::from(relative_path_owned),
                                            is_system_header_owned,
                                            location,
                                        )
                                    }
                                    Some(_) => {
                                        let location = code_parser.peek_location(0).unwrap();
                                        return Err(PreprocessFileError {
                                            file_number: location.file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "`__has_include` must be followed by a file path or identifier."
                                                    .to_owned(),
                                                location.range,
                                            ),
                                        });
                                    }
                                    None => {
                                        return Err(PreprocessFileError {
                                            file_number: current_token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "`__has_include` must be followed by a file path or identifier."
                                                    .to_owned(),
                                                current_token_with_location.location.range,
                                            ),
                                        });
                                    }
                                };

                            let result = if stick_to_system {
                                if self
                                    .context
                                    .file_provider
                                    .resolve_system_file(&relative_path)
                                    .is_some()
                                {
                                    1
                                } else {
                                    0
                                }
                            } else if self
                                .context
                                .file_provider
                                .resolve_user_file_with_fallback(
                                    &relative_path,
                                    &self
                                        .context
                                        .current_file_item
                                        .file_location
                                        .canonical_full_path,
                                    self.context.resolve_relative_path_within_current_file,
                                )
                                .is_some()
                            {
                                1
                            } else {
                                0
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    result.to_string(),
                                    false,
                                    IntegerNumberLength::Default,
                                ))),
                                Location::default(),
                            ));

                            code_parser.expect_and_consume_closing_paren()?; // Consumes ')'
                        }
                        "__has_embed" => {
                            // `__has_embed` is a preprocessor operator that checks if a file can be embedded.
                            // It can be used in conditional expressions and as part of `#if` and `#elif`.
                            //
                            // Syntax:
                            // - `__has_embed("file")`
                            // - `__has_embed(<file>)`
                            // - `__has_embed(file)`
                            //
                            // Returns:
                            // - `__STDC_EMBED_FOUND__` (1_isize) if the search for the resource succeeds,
                            //   the resource is non empty and all the parameters are supported.
                            // - `__STDC_EMBED_EMPTY__` (2_isize) if the resource is empty and all the parameters are supported.
                            // - `__STDC_EMBED_NOT_FOUND__` (0_isize) if the search fails or one of the parameters
                            //    passed is not supported by the implementation.

                            if expand_context_type != ExpandContextType::ConditionalExpression {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "`__has_embed` can only be used in conditional expressions."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            code_parser.expect_and_consume_opening_paren()?; // Consumes '('

                            let (relative_path, stick_to_system, _file_path_location) =
                                match code_parser.peek_token(0) {
                                    Some(Token::Identifier(id)) => {
                                        let id_owned = id.to_owned();
                                        let location = *code_parser.peek_location(0).unwrap();
                                        code_parser.next_token(); // consumes the identifier token

                                        let expended_tokens = self.expand_marco(
                                            vec![TokenWithLocation::new(
                                                Token::Identifier(id_owned),
                                                location,
                                            )],
                                            &HashMap::new(),
                                            ExpandContextType::Normal,
                                        )?;

                                        if expended_tokens.is_empty() {
                                            return Err(PreprocessFileError {
                                                file_number: location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "The macro expands to empty; expected a single string literal representing the file path.".to_owned(),
                                                    location.range,
                                                ),
                                            });
                                        }

                                        if expended_tokens.len() > 1 {
                                            return Err(PreprocessFileError {
                                                file_number: location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "The macro expands to multiple tokens; expected a single string literal representing the file path."
                                                        .to_owned(),
                                                    location.range
                                                ),
                                            });
                                        }

                                        match expended_tokens.first().unwrap() {
                                            TokenWithLocation {
                                                token: Token::String(relative_path, _),
                                                location,
                                            } => (PathBuf::from(relative_path), false, *location),
                                            _ => {
                                                return Err(PreprocessFileError {
                                                    file_number: location.file_number,
                                                    error: PreprocessError::MessageWithRange(
                                                        "Macro expansion resulted in an unexpected type; a single string literal representing the file path was expected.".to_owned(),
                                                        location.range,
                                                    ),
                                                });
                                            }
                                        }
                                    }
                                    Some(Token::HeaderFile(relative_path, is_system_file)) => {
                                        let relative_path_owned = relative_path.to_owned();
                                        let is_system_file_owned = *is_system_file;
                                        let location = *code_parser.peek_location(0).unwrap();
                                        code_parser.next_token(); // consumes the file path token

                                        (
                                            PathBuf::from(relative_path_owned),
                                            is_system_file_owned,
                                            location,
                                        )
                                    }
                                    Some(_) => {
                                        let location = code_parser.peek_location(0).unwrap();
                                        return Err(PreprocessFileError {
                                            file_number: location.file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "`__has_embed` must be followed by a file path or identifier."
                                                    .to_owned(),
                                                location.range,
                                            ),
                                        });
                                    }
                                    None => {
                                        return Err(PreprocessFileError {
                                            file_number: current_token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "`__has_include` must be followed by a file path or identifier."
                                                    .to_owned(),
                                                current_token_with_location.location.range,
                                            ),
                                        });
                                    }
                                };

                            // The possible return values are:
                            //
                            // - `__STDC_EMBED_FOUND__`
                            // - `__STDC_EMBED_EMPTY__`
                            // - `__STDC_EMBED_NOT_FOUND__`
                            //
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/embed.html
                            let result = if stick_to_system {
                                self.context
                                    .file_provider
                                    .resolve_system_file(&relative_path)
                                    .map_or(0, |canonical_full_path| {
                                        self.context
                                            .file_provider
                                            .file_size(&canonical_full_path)
                                            .map_or(0, |size| {
                                                if size == 0 {
                                                    2 // __STDC_EMBED_EMPTY__
                                                } else {
                                                    1 // __STDC_EMBED_FOUND__
                                                }
                                            })
                                    })
                            } else {
                                self.context
                                    .file_provider
                                    .resolve_user_file_with_fallback(
                                        &relative_path,
                                        &self
                                            .context
                                            .current_file_item
                                            .file_location
                                            .canonical_full_path,
                                        self.context.resolve_relative_path_within_current_file,
                                    )
                                    .map_or(
                                        0,
                                        |ResolvedResult {
                                             canonical_full_path,
                                             ..
                                         }| {
                                            self.context
                                                .file_provider
                                                .file_size(&canonical_full_path)
                                                .map_or(0, |size| {
                                                    if size == 0 {
                                                        2 // __STDC_EMBED_EMPTY__
                                                    } else {
                                                        1 // __STDC_EMBED_FOUND__
                                                    }
                                                })
                                        },
                                    )
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    result.to_string(),
                                    false,
                                    IntegerNumberLength::Default,
                                ))),
                                Location::default(),
                            ));

                            code_parser.expect_and_consume_closing_paren()?; // Consumes ')'
                        }
                        "__has_c_attribute" => {
                            // __has_c_attribute(...)
                            //
                            // __has_c_attribute can be expanded in the expression of #if and #elif.
                            // It is treated as a defined macro by #ifdef, #ifndef and defined but
                            // cannot be used anywhere else.
                            //
                            // attribute-token  Attribute           Value       Standard
                            // deprecated       [[deprecated]]      201904L     (C23)
                            // fallthrough      [[fallthrough]]     201904L     (C23)
                            // maybe_unused     [[maybe_unused]]    201904L     (C23)
                            // nodiscard        [[nodiscard]]       202003L     (C23)
                            // noreturn         [[noreturn]]        202202L     (C23)
                            // _Noreturn        [[_Noreturn]]       202202L     (C23)
                            // unsequenced      [[unsequenced]]     202207L     (C23)
                            // reproducible     [[reproducible]]    202207L     (C23)
                            //
                            // see:
                            // https://en.cppreference.com/w/c/language/attributes.html#Attribute_testing

                            if expand_context_type != ExpandContextType::ConditionalExpression {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "`__has_c_attribute` can only be used in conditional expressions."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            code_parser.expect_and_consume_opening_paren()?; // Consumes '('

                            let (attribute_name, _location) = match code_parser.peek_token(0) {
                                Some(Token::Identifier(id)) => {
                                    let id_owned = id.to_owned();
                                    let location = *code_parser.peek_location(0).unwrap();
                                    code_parser.next_token(); // consumes the identifier token

                                    (id_owned, location)
                                }
                                Some(_) => {
                                    let location = code_parser.peek_location(0).unwrap();
                                    return Err(PreprocessFileError {
                                        file_number: location.file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "`__has_c_attribute` must be followed by an attribute token."
                                                .to_owned(),
                                            location.range,
                                        ),
                                    });
                                }
                                None => {
                                    return Err(PreprocessFileError {
                                        file_number: current_token_with_location
                                            .location
                                            .file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "`__has_c_attribute` must be followed by an attribute token."
                                                .to_owned(),
                                            current_token_with_location.location.range,
                                        ),
                                    });
                                }
                            };

                            code_parser.expect_and_consume_closing_paren()?; // Consumes ')'

                            let value = match attribute_name.as_str() {
                                "deprecated" => 201904,
                                "fallthrough" => 201904,
                                "maybe_unused" => 201904,
                                "nodiscard" => 202003,
                                "noreturn" => 202202,
                                "_Noreturn" => 202202,
                                "unsequenced" => 202207,
                                "reproducible" => 202207,
                                _ => {
                                    // If the attribute is not recognized, return 0.
                                    0
                                }
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    value.to_string(),
                                    false,
                                    IntegerNumberLength::Long,
                                ))),
                                Location::default(),
                            ));
                        }
                        _ if code_parser.peek_token_and_equals(0, &Token::PoundPound) => {
                            // _Token concatenation_ (the `##` operator).
                            //
                            // Token concatenation joins two or more tokens into a single identifier.
                            // The tokens are concatenated without any whitespace in between, and the tokens
                            // are not expanded before concatenation.
                            // Token concatenation can only be used in macro definitions (both object-like and function-like).
                            //
                            // The result of _Token concatenation_ operator (`##`) must be a valid C identifier:
                            // the first token must be an identifier, and the remaining tokens must be either
                            // identifiers or integer numbers. For example, `foo##bar` and `sprite##2##b` are valid,
                            // while `9##s` and `+##=` are invalid.

                            if expand_context_type != ExpandContextType::ObjectLikeDefinition
                                && expand_context_type != ExpandContextType::FunctionLikeDefinition
                                && expand_context_type
                                    != ExpandContextType::VariadicFunctionLikeDefinition
                            {
                                let location = code_parser.peek_location(0).unwrap();
                                return Err(PreprocessFileError {
                                file_number: location.file_number,
                                error: PreprocessError::MessageWithRange(
                                    "Token concatenation (##) can only be used in macro definitions."
                                        .to_owned(),
                                    location.range,
                                ),
                            });
                            }

                            let get_valid_identifier_part_from_token =
                            |token_with_location: &TokenWithLocation,
                             is_first_part: bool|
                             -> Result<String, PreprocessFileError> {
                                let identifier_part = match &token_with_location.token {
                                    Token::Identifier(original_name) => {
                                        if original_name == "__VA_ARGS__" {
                                            return Err(PreprocessFileError {
                                                file_number: token_with_location.location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "__VA_ARGS__ cannot be used in token concatenation (##)."
                                                        .to_owned(),
                                                    token_with_location.location.range,
                                                ),
                                            });
                                        }

                                        // If the token is an identifier, check if it is a macro parameter.
                                        match argument_map.get(original_name) {
                                            Some(ArgumentValue::Single(arg_values)) => {
                                                if arg_values.is_empty() {
                                                    // If the identifier is a macro parameter but it's value is empty.
                                                    return Err(PreprocessFileError {
                                                        file_number: token_with_location
                                                            .location
                                                            .file_number,
                                                        error: PreprocessError::MessageWithRange(
                                                            format!(
                                                                "Token concatenation (##) cannot be followed by an empty macro parameter: '{}'.",
                                                                original_name
                                                            ),
                                                            token_with_location.location.range,
                                                        ),
                                                    });
                                                }

                                                if arg_values.len() > 1 {
                                                    // If the identifier is a macro parameter but it's value is a list of tokens.
                                                    return Err(PreprocessFileError {
                                                        file_number: token_with_location
                                                            .location
                                                            .file_number,
                                                        error: PreprocessError::MessageWithRange(
                                                            "Token concatenation (##) failed because the value of parameter contains multiple values.".to_owned(),
                                                            token_with_location.location.range,
                                                        ),
                                                    });
                                                }

                                                match arg_values.first().unwrap() {
                                                    TokenWithLocation {
                                                        token: Token::Identifier(value),
                                                        ..
                                                    } => {
                                                        // The argument value is an identifier
                                                        value.to_owned()
                                                    }
                                                    TokenWithLocation {
                                                        token:
                                                            Token::Number(Number::Integer(
                                                                IntegerNumber { value, .. },
                                                            )),
                                                        location
                                                    } => {
                                                        // The argument value is an integer number
                                                        if is_first_part {
                                                            return Err(PreprocessFileError {
                                                                file_number: location.file_number,
                                                                error: PreprocessError::MessageWithRange(
                                                                    "Token concatenation (##) cannot start with an integer number."
                                                                        .to_owned(),
                                                                    location.range,
                                                                ),
                                                            });
                                                        } else {
                                                            value.to_owned()
                                                        }
                                                    }
                                                    _ => {
                                                        // The argument value is not an identifier or integer number,
                                                        return Err(PreprocessFileError {
                                                            file_number: token_with_location.location.file_number,
                                                            error: PreprocessError::MessageWithRange(
                                                                "Token concatenation (##) can only be followed by identifiers or integer numbers."
                                                                    .to_owned(),
                                                                token_with_location.location.range,
                                                            ),
                                                        });
                                                    }
                                                }
                                            }
                                            Some(ArgumentValue::Concatenated(_)) => {
                                                unreachable!()
                                            }
                                            None => {
                                                // If the next token is an identifier that is not a macro parameter,
                                                original_name.to_owned()
                                            }
                                        }
                                    }
                                    Token::Number(Number::Integer(IntegerNumber {
                                        value, ..
                                    })) => {
                                        // If the next token is an integer number
                                        if is_first_part {
                                            return Err(PreprocessFileError {
                                                file_number: token_with_location.location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "Token concatenation (##) cannot start with an integer number."
                                                        .to_owned(),
                                                    token_with_location.location.range,
                                                ),
                                            });
                                        } else {
                                            value.to_owned()
                                        }
                                    }
                                    _ => {
                                        // If the ne token is not an identifier or integer number,
                                        // we cannot concatenate it.
                                        return Err(PreprocessFileError {
                                            file_number: token_with_location.location.file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "Token concatenation (##) can only apply to identifiers or integer numbers."
                                                    .to_owned(),
                                                token_with_location.location.range,
                                            ),
                                        });
                                    }
                                };

                                Ok(identifier_part)
                            };

                            let mut concatenated_name = vec![get_valid_identifier_part_from_token(
                                &current_token_with_location,
                                true,
                            )?];
                            let mut concatenated_location = current_token_with_location.location;

                            code_parser.next_token(); // consumes the `##` token

                            loop {
                                if let Some(next_token_with_location) =
                                    code_parser.next_token_with_location()
                                {
                                    concatenated_name.push(get_valid_identifier_part_from_token(
                                        &next_token_with_location,
                                        false,
                                    )?);
                                    concatenated_location.range.end_included =
                                        next_token_with_location.location.range.end_included;

                                    if code_parser.peek_token_and_equals(0, &Token::PoundPound) {
                                        // If the next token is another `##`, we continue concatenating.
                                        code_parser.next_token(); // consumes the `##` token
                                    } else {
                                        // Otherwise, we have reached the end of the concatenation.
                                        break;
                                    }
                                } else {
                                    return Err(PreprocessFileError {
                                        file_number: code_parser.last_location.file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "Token concatenation (##) must be followed by an identifier or number."
                                                .to_owned(),
                                            code_parser.last_location.range,
                                        ),
                                    });
                                }
                            }

                            let concatenated_token = TokenWithLocation::new(
                                Token::Identifier(concatenated_name.join("")),
                                concatenated_location,
                            );

                            // Push the concatenated identifier to the output.
                            output.push(concatenated_token);
                        }
                        _ if argument_map.contains_key(name) => {
                            // The identifier is a parameter.
                            let arg_value = argument_map.get(name).unwrap();
                            if let ArgumentValue::Single(tokens) = arg_value {
                                output.extend(tokens.to_owned());
                            } else {
                                unreachable!()
                            }
                        }
                        _ if self.context.macro_map.contains(name) => {
                            // The identifier is a macro.
                            let definition = self.context.macro_map.get(name).unwrap();
                            match definition {
                                MacroDefinition::ObjectLike(tokens) => {
                                    // The identifier is an object-like macro.
                                    // Replace macro with its corresponding tokens.
                                    let tokens_owned = tokens.to_owned();
                                    let expanded_tokens = self.expand_marco(
                                        tokens_owned,
                                        argument_map,
                                        ExpandContextType::ObjectLikeDefinition,
                                    )?;
                                    output.extend(expanded_tokens);
                                }
                                MacroDefinition::FunctionLike(params, tokens) => {
                                    // The identifier is a function-like macro.
                                    let params_owned = params.to_owned();
                                    let tokens_owned = tokens.to_owned();

                                    // collect the arguments for the macro invocation.
                                    code_parser.expect_and_consume_opening_paren()?; // Consumes '('

                                    let mut args = Vec::new();

                                    while let Some(arg_token) = code_parser.peek_token(0) {
                                        match &arg_token {
                                            Token::Punctuator(Punctuator::ParenthesisClose) => {
                                                // If the next token is a closing parenthesis, we have reached the end of the arguments.
                                                break;
                                            }
                                            Token::String(first_string, first_string_type) => {
                                                // If the argument is a string, we need to handle it specially.

                                                // concatenates adjacent strings
                                                let arg_location =
                                                    code_parser.peek_location(0).unwrap();
                                                let mut merged_string =
                                                    vec![first_string.to_owned()];
                                                let mut merged_range = arg_location.range;

                                                let merged_string_type = *first_string_type;
                                                let merged_file_number = arg_location.file_number;

                                                code_parser.next_token(); // consumes the first string literal token

                                                // concatenate the next token if it is also a string literal.
                                                while let Some(next_arg_token) =
                                                    code_parser.peek_token(0)
                                                {
                                                    if let Token::String(
                                                        next_string,
                                                        next_string_type,
                                                    ) = next_arg_token
                                                    {
                                                        let next_arg_location =
                                                            code_parser.peek_location(0).unwrap();
                                                        if merged_string_type != *next_string_type {
                                                            // If the string types are different, we cannot concatenate them.
                                                            return Err(
                                                                PreprocessFileError::new(
                                                                    next_arg_location.file_number,
                                                                PreprocessError::MessageWithRange(
                                                                        "Cannot concatenate string literals with different encoding types."
                                                                            .to_owned(),
                                                                        next_arg_location.range,
                                                                        )
                                                                )
                                                            );
                                                        }

                                                        // merge the next string literal
                                                        merged_string.push(next_string.to_owned());
                                                        merged_range.end_included =
                                                            next_arg_location.range.end_included;

                                                        code_parser.next_token(); // consumes the next string literal token
                                                    } else {
                                                        break;
                                                    }
                                                }

                                                let merged_arg = TokenWithLocation::new(
                                                    Token::String(
                                                        merged_string.join(""),
                                                        merged_string_type,
                                                    ),
                                                    Location::new(
                                                        merged_file_number,
                                                        &merged_range,
                                                    ),
                                                );

                                                // Push the merged string literal token to the arguments.
                                                args.push(merged_arg);
                                            }
                                            Token::Identifier(_)
                                            | Token::Number(_)
                                            | Token::Char(_, _) => {
                                                // Valid argument type for macro invocation.
                                                let arg_location =
                                                    code_parser.peek_location(0).unwrap();
                                                args.push(TokenWithLocation::new(
                                                    arg_token.clone(),
                                                    *arg_location,
                                                ));

                                                code_parser.next_token(); // consumes the token
                                            }
                                            _ => {
                                                // Invalid argument type for macro invocation.
                                                let arg_location =
                                                    code_parser.peek_location(0).unwrap();
                                                return Err(PreprocessFileError {
                                                    file_number: arg_location.file_number,
                                                    error: PreprocessError::MessageWithRange(
                                                        "Invalid argument type for macro invocation, only single identifier, number, string (or adjacent strings), or char are allowed.".to_string(),
                                                        arg_location.range,
                                                    ),
                                                });
                                            }
                                        }

                                        if code_parser.peek_token_and_equals(
                                            0,
                                            &Token::Punctuator(Punctuator::Comma),
                                        ) {
                                            // If the next token is a comma, consume it.
                                            code_parser.next_token(); // consumes the comma
                                        }
                                    }

                                    code_parser.expect_and_consume_closing_paren()?; // Consumes ')'

                                    let is_variadic =
                                        matches!(params.last(), Some(name) if name =="...");
                                    if (is_variadic && args.len() < params.len() - 1)
                                        || (!is_variadic && args.len() != params.len())
                                    {
                                        return Err(PreprocessFileError {
                                            file_number: current_token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                format!(
                                                    "Not enough arguments provided for macro: {}.",
                                                    name
                                                ),
                                                current_token_with_location.location.range,
                                            ),
                                        });
                                    }

                                    // Expand arguments
                                    let mut expanded_args = Vec::new();
                                    for arg in args {
                                        let expanded_token = self.expand_marco(
                                            vec![arg],
                                            argument_map,
                                            expand_context_type,
                                        )?;
                                        expanded_args.push(expanded_token);
                                    }

                                    // Construct the arguments with name-value pairs.
                                    let mut map = HashMap::<String, ArgumentValue>::new();
                                    for idx in 0..params_owned.len() {
                                        if params_owned[idx] == "..." {
                                            // Variadic arguments are represented as a concatenated list of tokens.
                                            let values = if expanded_args.len() > idx {
                                                expanded_args[idx..].to_vec()
                                            } else {
                                                vec![]
                                            };

                                            map.insert(
                                                "...".to_owned(),
                                                ArgumentValue::Concatenated(values),
                                            );
                                        } else {
                                            // Regular arguments are represented as a single token.
                                            map.insert(
                                                params_owned[idx].to_owned(),
                                                ArgumentValue::Single(expanded_args[idx].clone()),
                                            );
                                        }
                                    }

                                    // Expand the macro body with the arguments.
                                    let function_type = if matches!(params_owned.last(), Some(name) if name == "...")
                                    {
                                        ExpandContextType::VariadicFunctionLikeDefinition
                                    } else {
                                        ExpandContextType::FunctionLikeDefinition
                                    };

                                    let expanded_tokens =
                                        self.expand_marco(tokens_owned, &map, function_type)?;

                                    output.extend(expanded_tokens);
                                }
                            }
                        }
                        _ => {
                            // The identifier is not a macro or parameter, just push it as is.
                            output.push(current_token_with_location);
                        }
                    }
                }
                Token::Pound => {
                    // _Stringizing_ (the `#` operator).
                    //
                    // Stringizing is used to produce a string literal and can only be applied to
                    // macro parameters (includes `__VA_ARGS__`).
                    // For example, `#define FOO(x) #x` will expand `FOO(abc)` to `"abc"` and `FOO(123)` to `"123"`.

                    if expand_context_type != ExpandContextType::FunctionLikeDefinition
                        && expand_context_type != ExpandContextType::VariadicFunctionLikeDefinition
                    {
                        return Err(PreprocessFileError {
                            file_number: current_token_with_location.location.file_number,
                            error: PreprocessError::MessageWithRange(
                                "Stringizing (#) can only be used in function-like macro definitions."
                                    .to_owned(),
                                current_token_with_location.location.range,
                            ),
                        });
                    }

                    // Consume the next token, which should be a macro parameter.
                    if let Some(next_token_with_location) = code_parser.next_token_with_location() {
                        match &next_token_with_location.token {
                            Token::Identifier(name) => {
                                if name == "__VA_ARGS__" {
                                    if expand_context_type
                                        != ExpandContextType::VariadicFunctionLikeDefinition
                                    {
                                        return Err(PreprocessFileError {
                                            file_number: next_token_with_location.location.file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "__VA_ARGS__ can only be used in variadic function-like macros."
                                                    .to_owned(),
                                                next_token_with_location.location.range,
                                            ),
                                        });
                                    }

                                    // If the next token is `__VA_ARGS__`, we need to handle it specially.
                                    // It should be STRd as a concatenated list of tokens.
                                    if let Some(ArgumentValue::Concatenated(tokens)) =
                                        argument_map.get("...")
                                    {
                                        // Convert the tokens to a string literal.
                                        let generated_string = stringify_arguments(tokens)?;
                                        let generated_token = TokenWithLocation::new(
                                            Token::String(
                                                generated_string,
                                                StringEncoding::Default,
                                            ),
                                            Location::default(),
                                        );
                                        output.push(generated_token);
                                    } else {
                                        unreachable!()
                                    }
                                } else {
                                    match argument_map.get(name) {
                                        Some(ArgumentValue::Single(tokens)) => {
                                            // Convert the tokens to a string literal.
                                            let generated_string = stringify_tokens(tokens)?;
                                            let generated_token = TokenWithLocation::new(
                                                Token::String(
                                                    generated_string,
                                                    StringEncoding::Default,
                                                ),
                                                Location::default(),
                                            );
                                            output.push(generated_token);
                                        }
                                        _ => {
                                            return Err(PreprocessFileError {
                                                file_number: next_token_with_location
                                                    .location
                                                    .file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Stringizing can only be applied to macro parameters, but '{}' is not a parameter.",
                                                        name
                                                    ),
                                                    next_token_with_location.location.range,
                                                ),
                                            });
                                        }
                                    }
                                }
                            }
                            _ => {
                                return Err(PreprocessFileError {
                                    file_number: next_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "Stringizing (#) can only be applied to macro parameters."
                                            .to_owned(),
                                        next_token_with_location.location.range,
                                    ),
                                });
                            }
                        }
                    } else {
                        return Err(PreprocessFileError {
                            file_number: current_token_with_location.location.file_number,
                            error: PreprocessError::MessageWithRange(
                                "Stringizing (#) operator must be followed by a macro parameter."
                                    .to_owned(),
                                current_token_with_location.location.range,
                            ),
                        });
                    }
                }
                Token::PoundPound => {
                    // Token concatenation (the `##` operator) is handled above.
                    // If we reach here, it means the token was not an identifier or a macro.
                    return Err(PreprocessFileError {
                        file_number: current_token_with_location.location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Token concatenation (##) must follow an identifier.".to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }
                _ => {
                    // The current token is not an identifier, just push it to the output.
                    output.push(current_token_with_location);
                }
            }
        }

        Ok(output)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ExpandContextType {
    /// Normal code statements, no macro expansion.
    Normal = 0,

    /// Object-like macro definition, where token concatenation is allowed.
    ObjectLikeDefinition = 1,

    /// Function-like macro definition, where stringizing is allowed.
    FunctionLikeDefinition = 2,

    /// Variadic function-like macro definition, where `__VA_ARGS__` and `__VA_OPT__` are allowed.
    VariadicFunctionLikeDefinition = 3,

    /// Conditional expression, i.e., `#if` and `#elif`.
    ConditionalExpression = 4,
}

/// Convert a token to a string.
fn token_to_string(token_with_location: &TokenWithLocation) -> Result<String, PreprocessFileError> {
    let s = match &token_with_location.token {
        Token::Char(_, _) | Token::Number(_) | Token::Identifier(_) | Token::String(_, _) => {
            token_with_location.token.to_string()
        }
        _ => {
            return Err(PreprocessFileError {
                file_number: token_with_location.location.file_number,
                error: PreprocessError::MessageWithRange(
                    "Stringizing (#) can only be applied to identifiers, numbers, chars, or string literals."
                        .to_owned(),
                    token_with_location.location.range,
                ),
            });
        }
    };
    Ok(s)
}

/// Stringify a list of tokens.
/// Tokens are separated by spaces.
fn stringify_tokens(
    token_with_locations: &[TokenWithLocation],
) -> Result<String, PreprocessFileError> {
    let mut items = vec![];
    for token_with_location in token_with_locations {
        let token_string = token_to_string(token_with_location)?;
        items.push(token_string);
    }
    Ok(items.join(" "))
}

/// Stringify a list of arguments.
/// Arguments are separated by commas.
fn stringify_arguments(args: &[Vec<TokenWithLocation>]) -> Result<String, PreprocessFileError> {
    let mut outputs = vec![];
    for arg in args {
        let mut items = vec![];
        for token_with_location in arg {
            let token_string = token_to_string(token_with_location)?;
            items.push(token_string);
        }
        outputs.push(items.join(" "))
    }
    Ok(outputs.join(", "))
}

#[derive(Debug, PartialEq)]
enum ArgumentValue {
    /// The value of a single identifier, number, string, or char.
    /// Since the argument may be a macro, it is represented as a list of tokens,
    /// so the value can be a Vector.
    Single(Vec<TokenWithLocation>),

    /// The value list of variadic arguments.
    Concatenated(Vec<Vec<TokenWithLocation>>),
}

struct CodeParser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
    last_location: Location,
    current_file_number: usize,
}

impl<'a> CodeParser<'a> {
    fn new(
        upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
        current_file_number: usize,
    ) -> Self {
        let last_location = if let Some(first_token) = upstream.peek(0) {
            // Initialize last_location with the first token's location.
            first_token.location
        } else {
            Location::new(current_file_number, &Range::default())
        };

        Self {
            upstream,
            last_location,
            current_file_number,
        }
    }

    fn next_token_with_location(&mut self) -> Option<TokenWithLocation> {
        match self.upstream.next() {
            Some(token_with_location) => {
                self.last_location = token_with_location.location;
                Some(token_with_location)
            }
            None => None,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithLocation { token, location }) => {
                self.last_location = location;
                Some(token)
            }
            None => None,
        }
    }

    fn peek_location(&self, offset: usize) -> Option<&Location> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { location, .. }) => Some(location),
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { token, .. }) => Some(token),
            None => None,
        }
    }

    fn peek_token_and_equals(&self, offset: usize, expected_token: &Token) -> bool {
        matches!(
            self.peek_token(offset),
            Some(token) if token == expected_token)
    }

    fn expect_and_consume_token(
        &mut self,
        expected_token: &Token,
        token_description: &str,
    ) -> Result<(), PreprocessFileError> {
        match self.next_token() {
            Some(token) => {
                if &token == expected_token {
                    Ok(())
                } else {
                    Err(PreprocessFileError::new(
                        self.last_location.file_number,
                        PreprocessError::MessageWithPosition(
                            format!("Expect token: {}.", token_description),
                            self.last_location.range.start,
                        ),
                    ))
                }
            }
            None => Err(PreprocessFileError::new(
                self.current_file_number,
                PreprocessError::UnexpectedEndOfDocument(format!(
                    "Expect token: {}.",
                    token_description
                )),
            )),
        }
    }

    // expects open parenthesis '(' and consumes it.
    fn expect_and_consume_opening_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    // expects close parenthesis ')' and consumes it.
    fn expect_and_consume_closing_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisClose),
            "closing parenthesis",
        )
    }
}

/// Concatenates adjacent string literals into a single string literal token.
/// Only string literals with identical encoding prefixes can be concatenated.
/// For example, `"abc" "def"` becomes `"abcdef"`, but `u8"abc" "def"` is not allowed.
///
/// See:
/// - https://en.cppreference.com/w/c/language/string_literal.html
fn concatenate_adjacent_strings(
    tokens: &mut PeekableIter<TokenWithLocation>,
) -> Result<Vec<TokenWithLocation>, PreprocessFileError> {
    let mut output = Vec::new();

    while let Some(token_with_location) = tokens.next() {
        if let TokenWithLocation {
            token: Token::String(first_string, first_type),
            location: first_location,
        } = &token_with_location
        {
            let mut merged_string = vec![first_string.to_owned()];
            let mut merged_location = *first_location;

            // Check if the next token is also a string literal with the same encoding.
            while let Some(next_token_with_location) = tokens.peek(0) {
                if let TokenWithLocation {
                    token: Token::String(next_string, next_type),
                    location: next_location,
                } = next_token_with_location
                {
                    if first_type != next_type {
                        // Cannot concatenate string literals with different encoding types.
                        return Err(PreprocessFileError::new(
                            next_location.file_number,
                            PreprocessError::MessageWithRange(
                                "Cannot concatenate string literals with different encoding types."
                                    .to_owned(),
                                next_location.range,
                            ),
                        ));
                    }

                    // Merge the next string literal.
                    merged_string.push(next_string.to_owned());
                    merged_location.range.end_included = next_location.range.end_included;

                    tokens.next(); // Consume the string literal token.
                } else {
                    break;
                }
            }

            let merged_token_with_location = TokenWithLocation::new(
                Token::String(merged_string.join(""), *first_type),
                merged_location,
            );

            output.push(merged_token_with_location);
        } else {
            output.push(token_with_location);
        }
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use chrono::Local;
    use pretty_assertions::assert_eq;
    use std::{collections::HashMap, path::Path};

    use crate::{
        FILE_NUMBER_SOURCE_FILE_BEGIN, PreprocessError, PreprocessFileError, TokenWithLocation,
        header_file_cache::HeaderFileCache,
        location::Location,
        memory_file_provider::MemoryFileProvider,
        position::Position,
        processor::{PreprocessResult, process_source_file},
        prompt::{Prompt, PromptLevel},
        range::Range,
        token::{C23_KEYWORDS, IntegerNumber, IntegerNumberLength, Number, Punctuator, Token},
    };

    fn process_single(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Result<PreprocessResult, PreprocessFileError> {
        let mut file_cache = HeaderFileCache::new();
        let mut file_provider = MemoryFileProvider::new();
        file_provider.add_user_text_file(Path::new("src/main.c"), src);

        process_source_file(
            &file_provider,
            &mut file_cache,
            &C23_KEYWORDS,
            predefinitions,
            false,
            FILE_NUMBER_SOURCE_FILE_BEGIN,
            Path::new("src/main.c"),
            Path::new("/projects/test/src/main.c"),
        )
    }

    fn process_single_get_tokens(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Vec<TokenWithLocation> {
        process_single(src, predefinitions).unwrap().output
    }

    fn process_multiple(
        main_src: &str,
        user_header_files: &[(&str, &str)],
        user_binary_files: &[(&str, &[u8])],
        system_header_files: &[(&str, &str)],
    ) -> Result<PreprocessResult, PreprocessFileError> {
        let mut file_cache = HeaderFileCache::new();
        let mut file_provider = MemoryFileProvider::new();

        file_provider.add_user_text_file(Path::new("src/main.c"), main_src);

        for (path, content) in user_header_files {
            file_provider.add_user_text_file(Path::new(path), content);
        }

        for (path, content) in user_binary_files {
            file_provider.add_user_binary_file(Path::new(path), content.to_vec());
        }

        for (path, content) in system_header_files {
            file_provider.add_system_file(Path::new(path), content);
        }

        let predefinitions: HashMap<String, String> = HashMap::new();

        process_source_file(
            &file_provider,
            &mut file_cache,
            &C23_KEYWORDS,
            &predefinitions,
            false,
            FILE_NUMBER_SOURCE_FILE_BEGIN,
            Path::new("src/main.c"),
            Path::new("/projects/test/src/main.c"),
        )
    }

    fn process_multiple_get_tokens(
        main_src: &str,
        user_header_files: &[(&str, &str)],
        user_binary_files: &[(&str, &[u8])],
        system_header_files: &[(&str, &str)],
    ) -> Vec<TokenWithLocation> {
        process_multiple(
            main_src,
            user_header_files,
            user_binary_files,
            system_header_files,
        )
        .unwrap()
        .output
    }

    fn print_tokens(token_with_location: &[TokenWithLocation]) -> String {
        token_with_location
            .iter()
            .map(|TokenWithLocation { token, .. }| token.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn test_process_code_without_directive() {
        let filenum = FILE_NUMBER_SOURCE_FILE_BEGIN;
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            "\
int main() {
    return 0;
}",
            &predefinitions,
        );

        assert_eq!(
            tokens,
            vec![
                TokenWithLocation::new(
                    Token::Identifier("int".to_string()),
                    Location::from_detail(filenum, 0, 0, 0, 3)
                ),
                TokenWithLocation::new(
                    Token::Identifier("main".to_string()),
                    Location::from_detail(filenum, 4, 0, 4, 4)
                ),
                TokenWithLocation::new(
                    Token::Punctuator(Punctuator::ParenthesisOpen),
                    Location::from_detail(filenum, 8, 0, 8, 1)
                ),
                TokenWithLocation::new(
                    Token::Punctuator(Punctuator::ParenthesisClose),
                    Location::from_detail(filenum, 9, 0, 9, 1),
                ),
                TokenWithLocation::new(
                    Token::Punctuator(Punctuator::BraceOpen),
                    Location::from_detail(filenum, 11, 0, 11, 1),
                ),
                TokenWithLocation::new(
                    Token::Identifier("return".to_string()),
                    Location::from_detail(filenum, 17, 1, 4, 6),
                ),
                TokenWithLocation::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "0".to_string(),
                        false,
                        IntegerNumberLength::Default
                    ))),
                    Location::from_detail(filenum, 24, 1, 11, 1),
                ),
                TokenWithLocation::new(
                    Token::Punctuator(Punctuator::Semicolon),
                    Location::from_detail(filenum, 25, 1, 12, 1),
                ),
                TokenWithLocation::new(
                    Token::Punctuator(Punctuator::BraceClose),
                    Location::from_detail(filenum, 27, 2, 0, 1),
                ),
            ]
        );

        assert_eq!(print_tokens(&tokens), "int main ( ) { return 0 ; }");
    }

    #[test]
    fn test_process_pragma() {
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            "\
#pragma STDC FENV_ACCESS ON
#pragma STDC FP_CONTRACT OFF
#pragma STDC CX_LIMITED_RANGE DEFAULT",
            &predefinitions,
        );

        // pragmas are ignored currently, so the output should be empty.
        assert!(tokens.is_empty());
    }

    #[test]
    fn test_process_error_directive() {
        let predefinitions = HashMap::new();

        assert!(matches!(
            process_single(
                "\
#error \"foobar\"",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 7,
                            line: 0,
                            column: 7
                        },
                        end_included: Position {
                            index: 14,
                            line: 0,
                            column: 14
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_warning_directive() {
        let predefinitions = HashMap::new();

        assert!(matches!(
            process_single(
                "\
#warning \"foobar\"",
                &predefinitions,
            )
            .unwrap()
            .prompts
            .first()
            .unwrap(),
            Prompt::MessageWithRange(
                PromptLevel::Warning,
                FILE_NUMBER_SOURCE_FILE_BEGIN,
                _,
                Range {
                    start: Position {
                        index: 9,
                        line: 0,
                        column: 9
                    },
                    end_included: Position {
                        index: 16,
                        line: 0,
                        column: 16
                    }
                }
            )
        ));
    }

    #[test]
    fn test_process_predefinitions() {
        let mut predefinitions = HashMap::new();
        predefinitions.insert("FOO".to_string(), "'🦛'".to_string());
        predefinitions.insert("BAR".to_string(), "\"✨ abc\"".to_string());
        predefinitions.insert("A".to_string(), "123".to_string());
        predefinitions.insert("B".to_string(), "".to_string());
        predefinitions.insert("C".to_string(), "FOO".to_string()); // Reference to FOO

        let tokens = process_single_get_tokens(
            "\
hello FOO BAR A B C world.",
            &predefinitions,
        );
        assert_eq!(
            print_tokens(&tokens),
            "hello '🦛' \"✨ abc\" 123 '🦛' world ."
        );
    }

    #[test]
    fn test_process_dynamic_macro() {
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            "\
__FILE__
__LINE__
__DATE__
2025 // Prevent ajacent string concatenation
__TIME__",
            &predefinitions,
        );

        let now = Local::now();
        let date_string = now.format("%b %e %Y").to_string();
        let time_string = now.format("%H:%M:%S").to_string();

        assert_eq!(
            print_tokens(&tokens),
            format!(r#""src/main.c" 2 "{}" 2025 "{}""#, date_string, time_string)
        );
    }

    #[test]
    fn test_process_define() {
        let mut predefinitions = HashMap::new();
        predefinitions.insert("FOO".to_string(), "'🦛'".to_string());
        predefinitions.insert("BAR".to_string(), "\"✨ abc\"".to_string());

        let tokens = process_single_get_tokens(
            "\
#define A 123
#define B
#define C FOO
hello FOO BAR A B C world.",
            &predefinitions,
        );
        assert_eq!(
            print_tokens(&tokens),
            "hello '🦛' \"✨ abc\" 123 '🦛' world ."
        );

        // err: redefine FOO
        assert!(matches!(
            process_single(
                "\
#define FOO 'a'
#define FOO 'b'",
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 24,
                            line: 1,
                            column: 8
                        },
                        end_included: Position {
                            index: 26,
                            line: 1,
                            column: 10
                        }
                    }
                )
            })
        ));

        // err: define 'defined' macro
        assert!(matches!(
            process_single(
                "\
#define defined 123",
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 8,
                            line: 0,
                            column: 8
                        },
                        end_included: Position {
                            index: 14,
                            line: 0,
                            column: 14
                        }
                    }
                )
            })
        ));

        // err: define a macro with the same name as C keyword `return`
        assert!(matches!(
            process_single(
                "\
        #define return break",
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                )
            })
            if range == Range::from_detail(8, 0, 8, 6)
        ));
    }

    #[test]
    fn test_process_undefine() {
        let tokens = process_single_get_tokens(
            "\
#define A 123
#undef A
#define A 456
hello A world.",
            &HashMap::new(),
        );
        assert_eq!(print_tokens(&tokens), "hello 456 world .");

        // err: undefine non-existing macro
        assert!(matches!(
            process_single(
                "\
#undef NONE
",
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 7,
                            line: 0,
                            column: 7
                        },
                        end_included: Position {
                            index: 10,
                            line: 0,
                            column: 10
                        }
                    }
                )
            })
        ));
        // err: undefine 'defined' macro
        assert!(matches!(
            process_single(
                "\
#undef defined
",
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 7,
                            line: 0,
                            column: 7
                        },
                        end_included: Position {
                            index: 13,
                            line: 0,
                            column: 13
                        }
                    }
                )
            })
        ));

        // err: undefine a macro with the same name as C keyword `return`
        assert!(matches!(
            process_single(
                "\
        #undef return",
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                )
            })
            if range == Range::from_detail(7, 0, 7, 6)
        ));
    }

    #[test]
    fn test_process_define_function() {
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            r#"#define FOO(x) x
#define BAR(x, y) x y
#define A 'a'
#define B A

// string as argument
FOO("foo")

// identifier as argument
FOO(foo)

// number and char as arguments
BAR(123, '✨')

// macros as arguments
BAR(A,B)"#,
            &predefinitions,
        );

        assert_eq!(print_tokens(&tokens), r#""foo" foo 123 '✨' 'a' 'a'"#);

        let tokens_nested = process_single_get_tokens(
            r#"#define FOO(a) 1 a
#define BAR(x, y) 2 FOO(x) y
#define BUZ(z) 3 BAR(z, spark)
BUZ(hippo)"#,
            &predefinitions,
        );

        assert_eq!(print_tokens(&tokens_nested), "3 2 1 hippo spark");

        // err: redefine function-like macro
        assert!(matches!(
            process_single(
                "\
#define FOO(x) x
#define FOO(y) y + 1",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 25,
                            line: 1,
                            column: 8
                        },
                        end_included: Position {
                            index: 27,
                            line: 1,
                            column: 10
                        }
                    }
                )
            })
        ));

        // err: not enough arguments for function-like macro
        assert!(matches!(
            process_single(
                "\
#define FOO(x,y) x y
FOO(1)",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 21,
                            line: 1,
                            column: 0
                        },
                        end_included: Position {
                            index: 23,
                            line: 1,
                            column: 2
                        }
                    }
                )
            })
        ));

        // err: too many arguments for function-like macro
        assert!(matches!(
            process_single(
                "\
#define FOO(x,y) x y
FOO(1, 2, 3)",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 21,
                            line: 1,
                            column: 0
                        },
                        end_included: Position {
                            index: 23,
                            line: 1,
                            column: 2
                        }
                    }
                )
            })
        ));

        // err: invalid argument type for function-like macro
        assert!(matches!(
            process_single(
                "\
#define FOO(x) x
FOO(i++)",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 22,
                            line: 1,
                            column: 5
                        },
                        end_included: Position {
                            index: 23,
                            line: 1,
                            column: 6
                        }
                    }
                )
            })
        ));

        // err: define 'defined' macro
        assert!(matches!(
            process_single(
                "\
#define defined(x) 123",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 8,
                            line: 0,
                            column: 8
                        },
                        end_included: Position {
                            index: 14,
                            line: 0,
                            column: 14
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_define_function_variadic() {
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            "\
// Outputs argument list as is
#define FOO(...) __VA_ARGS__

// Consumes the first two arguments and outputs the rest
#define BAR(x,y,...) __VA_ARGS__

// Outputs `name = { values }`
#define BUZ(x,...) x __VA_OPT__(= { __VA_ARGS__ })
FOO()
FOO(1, 2, 3)
BAR(A, B)
BAR(a, b, c, d, e)
BUZ(t)
BUZ(u,v,w)",
            &predefinitions,
        );

        assert_eq!(print_tokens(&tokens), "1 , 2 , 3 c , d , e t u = { v , w }");

        // err: not enough arguments for variadic macro
        assert!(matches!(
            process_single(
                "\
#define FOO(x,y,...) __VA_ARGS__
FOO(123)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 33,
                            line: 1,
                            column: 0
                        },
                        end_included: Position {
                            index: 35,
                            line: 1,
                            column: 2
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_stringizing() {
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            r#"
#define FOO 3.14 "world" '🐘'
#define STR(x) #x
#define STR2(x) STR(x)
#define STR3(...) #__VA_ARGS__
STR(hello)
11              // Prevent adjacent string concatenation
STR("world")
13
STR(FOO)
17
STR('🦛')
19
STR2('🦛')
23
STR3(42)
29
STR3(1, '2', "3", id, FOO)"#,
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            r#""hello" 11 "\"world\"" 13 "3.14 \"world\" \'🐘\'" 17 "\'🦛\'" 19 "\'🦛\'" 23 "42" 29 "1, \'2\', \"3\", id, 3.14 \"world\" \'🐘\'""#
        );

        // err: stringizing in a not function-like macro
        assert!(matches!(
            process_single(
                "\
#define STR #123
STR",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 12,
                            line: 0,
                            column: 12
                        },
                        end_included: Position {
                            index: 12,
                            line: 0,
                            column: 12
                        }
                    }
                )
            })
        ));

        // err: stringizing on punctuator
        assert!(matches!(
            process_single(
                "\
#define PLUS 1+2
#define STR(x) #x
STR(PLUS)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 14,
                            line: 0,
                            column: 14
                        },
                        end_included: Position {
                            index: 14,
                            line: 0,
                            column: 14
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_token_concatenation() {
        let predefinitions = HashMap::new();
        let tokens = process_single_get_tokens(
            "\
#define FOO a##1
#define BAR sprite##2##b
#define BUZ is##FOO
#define CONCAT(a, b) a##b
#define CONCAT2(a, b) sprite##a##b
FOO
BAR
BUZ
CONCAT(hello, world)
CONCAT2(9, s)
            ",
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            "a1 sprite2b isFOO helloworld sprite9s"
        );

        // err: concatenate a number and an identifier
        assert!(matches!(
            process_single(
                "\
#define CONCAT 1##hello
CONCAT",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 16,
                            line: 0,
                            column: 16
                        },
                        end_included: Position {
                            index: 17,
                            line: 0,
                            column: 17
                        }
                    }
                )
            })
        ));

        // err: concatenate a number and an identifier through a function-like macro
        assert!(matches!(
            process_single(
                "\
#define CONCAT(a, b) a##b
CONCAT(1, hello)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                )
            })
            if range == Range::from_detail(33, 1, 7, 1)
        ));

        // err: concatenate an identifier and a string
        assert!(matches!(
            process_single(
                "\
#define CONCAT hello##\"world\"
CONCAT",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 22,
                            line: 0,
                            column: 22
                        },
                        end_included: Position {
                            index: 28,
                            line: 0,
                            column: 28
                        }
                    }
                )
            })
        ));

        // err: concatenate an identifier and a string through a function-like macro
        assert!(matches!(
            process_single(
                "\
#define CONCAT(a, b) a##b
CONCAT(hello, \"world\")",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 24,
                            line: 0,
                            column: 24
                        },
                        end_included: Position {
                            index: 24,
                            line: 0,
                            column: 24
                        }
                    }
                )
            })
        ));

        // err: concatenate a argument which expands to multiple tokens
        assert!(matches!(
            process_single(
                "\
#define FOO 1 2
#define CONCAT(a, b) a##b
CONCAT(hello, FOO)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 40,
                            line: 1,
                            column: 24
                        },
                        end_included: Position {
                            index: 40,
                            line: 1,
                            column: 24
                        }
                    }
                )
            })
        ));

        // err: `##` followed by `__VA_ARGS__`
        assert!(matches!(
            process_single(
                "\
#define CONCAT(...) ##__VA_ARGS__
CONCAT(hello, world)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 20,
                            line: 0,
                            column: 20
                        },
                        end_included: Position {
                            index: 21,
                            line: 0,
                            column: 21
                        }
                    }
                )
            })
        ));

        // err: `##` followed by nothing
        assert!(matches!(
            process_single(
                "\
#define CONCAT(a, b) a##
CONCAT(hello, world)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 22,
                            line: 0,
                            column: 22
                        },
                        end_included: Position {
                            index: 23,
                            line: 0,
                            column: 23
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_include() {
        let tokens0 = process_multiple_get_tokens(
            r#"
#include "foo.h"
FOO
"#,
            &[("header/foo.h", "#define FOO 42")],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens0), "42");

        // include the same header file multiple times
        let tokens2 = process_multiple_get_tokens(
            r#"
#include "foo.h"
#include "foo.h"
FOO
"#,
            &[("header/foo.h", "#define FOO 42")],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens2), "42");

        // nested include
        let tokens3 = process_multiple_get_tokens(
            r#"
#include "foo.h"
FOO BAR
"#,
            &[
                (
                    "header/foo.h",
                    r#"
#include "bar.h"
#define FOO 11
"#,
                ),
                ("header/bar.h", "#define BAR 13"),
            ],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens3), "11 13");

        // include system header file
        let tokens4 = process_multiple_get_tokens(
            r#"
#include <std/io.h>
FOO
"#,
            &[],
            &[],
            &[("std/io.h", "#define FOO 11")],
        );

        assert_eq!(print_tokens(&tokens4), "11");

        // include system header file with quoted include
        let tokens5 = process_multiple_get_tokens(
            r#"
#include "std/io.h"
FOO
"#,
            &[],
            &[],
            &[("std/io.h", "#define FOO 11")],
        );

        assert_eq!(print_tokens(&tokens5), "11");

        // include header file with identifier
        let tokens6 = process_multiple_get_tokens(
            r#"
#include "foo.h"
FOO BAR
"#,
            &[
                (
                    "header/foo.h",
                    r#"
#define HEADER_FILE "bar.h"
#include HEADER_FILE
#define FOO 11
"#,
                ),
                ("header/bar.h", "#define BAR 13"),
            ],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens6), "11 13");

        // err: include non-existing file
        assert!(matches!(
            process_multiple(r#"#include "non_existing.h""#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 9,
                            line: 0,
                            column: 9
                        },
                        end_included: Position {
                            index: 24,
                            line: 0,
                            column: 24
                        }
                    }
                )
            })
        ));

        // err: include with identifier which expands to non-existing file
        assert!(matches!(
            process_multiple(
                r#"#define HEADER_FILE "non_existing.h"
#include HEADER_FILE
"#,
                &[],
                &[],
                &[],
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 20,
                            line: 0,
                            column: 20
                        },
                        end_included: Position {
                            index: 35,
                            line: 0,
                            column: 35
                        }
                    }
                )
            })
        ));

        // err: include with identifier which expands to empty
        assert!(matches!(
            process_multiple(
                r#"#define HEADER_FILE
#include HEADER_FILE
"#,
                &[],
                &[],
                &[],
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 29,
                            line: 1,
                            column: 9
                        },
                        end_included: Position {
                            index: 39,
                            line: 1,
                            column: 19
                        }
                    }
                )
            })
        ));

        // err: include with identifier which expands to a number
        assert!(matches!(
            process_multiple(
                r#"#define HEADER_FILE 123
#include HEADER_FILE
"#,
                &[],
                &[],
                &[],
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                )
            })
            if range == Range::from_detail(20, 0, 20, 3)
        ));

        // err: include with identifier which expands to multiple tokens
        assert!(matches!(
            process_multiple(
                r#"#define HEADER_FILE foo bar
#include HEADER_FILE
"#,
                &[],
                &[],
                &[],
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 37,
                            line: 1,
                            column: 9
                        },
                        end_included: Position {
                            index: 47,
                            line: 1,
                            column: 19
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_embed() {
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#embed "foo.bin"
"#,
                &[],
                &[("resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 , 0x04 , 0x05"
        );

        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#embed "foo.bin" limit(100)
"#,
                &[],
                &[("resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 , 0x04 , 0x05"
        );

        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#embed "foo.bin" limit(3) prefix(0xaa, 11, 'a') suffix(0)
"#,
                &[],
                &[("resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0xaa , 0x0b , 0x61 , 0x01 , 0x02 , 0x03 , 0x00"
        );

        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#embed "foo.bin" limit(3) if_empty(0x11,0x13,0x17,0x19) prefix(0x3,0x5,0x7) suffix(0x23, 0x29)
"#,
                &[],
                &[("resources/foo.bin", &[])],
                &[],
            )),
            "0x11 , 0x13 , 0x17 , 0x19"
        );

        // load binary multiple times
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#embed "foo.bin"
#embed "foo.bin"
"#,
                &[],
                &[("resources/foo.bin", &[1, 2, 3])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 0x01 , 0x02 , 0x03"
        );

        // Load binary with identifier
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#define FOO "foo.bin"
#embed FOO
"#,
                &[],
                &[("resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 , 0x04 , 0x05"
        );

        // err: include non-existing file
        assert!(matches!(
            process_multiple(r#"#embed "non_existing.bin""#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 7,
                            line: 0,
                            column: 7
                        },
                        end_included: Position {
                            index: 24,
                            line: 0,
                            column: 24
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_if() {
        let predefinitions = HashMap::new();

        // Test `ifdef`
        assert_eq!(
            print_tokens(&process_single_get_tokens(
                r#"
#define FOO 'a'
#define BAR

#ifdef FOO
    FOO
#else
    'b'
#endif

#ifdef BAR
    "hello"
#else
    "world"
#endif

#ifdef BAZ
    11
#else
    13
#endif
"#,
                &predefinitions,
            )),
            "'a' \"hello\" 13"
        );

        // Test `ifndef`
        assert_eq!(
            print_tokens(&process_single_get_tokens(
                r#"
#define FOO 'a'
#define BAR

#ifndef FOO
    FOO
#else
    'b'
#endif

#ifndef BAR
    "hello"
#else
    "world"
#endif

#ifndef BAZ
    11
#else
    13
#endif
"#,
                &predefinitions,
            )),
            "'b' \"world\" 11"
        );

        // Test `if` and operator `defined`
        assert_eq!(
            print_tokens(&process_single_get_tokens(
                r#"
#define FOO 'a'

#if defined(FOO)
    FOO
#else
    'b'
#endif

#if !defined(FOO)
    "hello"
#else
    "world"
#endif

#if defined BAR
    11
#else
    13
#endif

#if !defined BAR
    17
#else
    19
#endif

"#,
                &predefinitions,
            )),
            "'a' \"world\" 13 17"
        );

        // Test `if` and binary and unary operators
        assert_eq!(
            print_tokens(&process_single_get_tokens(
                r#"
#define FOO 11
#define BAR 3 + 7
#define BUZ 0

#if FOO
    a   // <--
#else
    b
#endif

#if FOO == 11
    c   // <--
#else
    d
#endif

#if FOO + BAR == 21
    e   // <--
#else
    f
#endif

#if FOO > BAR
    g   // <--
#else
    h
#endif

#if FOO || BUZ
    i   // <--
#else
    j
#endif

#if FOO && BUZ
    k
#else
    l   // <--
#endif

#if !FOO
    m
#else
    n   // <--
#endif
"#,
                &predefinitions,
            )),
            "a c e g i l n"
        );
    }

    #[test]
    fn test_process_operator_has_include() {
        // Test `#has_include`
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#if __has_include("foo.h")
    11
#else
    13
#endif

#if __has_include("bar.h")
    17
#else
    19
#endif
"#,
                &[("header/foo.h", "123")],
                &[],
                &[],
            )),
            "11 19"
        );
    }

    #[test]
    fn test_process_operator_has_embed() {
        // Test `#has_embed`
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#if __has_embed("foo.bin")
    11
#else
    13
#endif

#if __has_embed("bar.bin")
    17
#else
    19
#endif

#if __has_embed("baz.bin")
    23
#else
    29
#endif

#if __has_embed("foo.bin") == __STDC_EMBED_FOUND__
    foo_found
#elif __has_embed("foo.bin") == __STDC_EMBED_EMPTY__
    foo_empty
#elif __has_embed("foo.bin") == __STDC_EMBED_NOT_FOUND__
    foo_not_found
#else
    foo_otherwise
#endif

#if __has_embed("bar.bin") == __STDC_EMBED_FOUND__
    bar_found
#elif __has_embed("bar.bin") == __STDC_EMBED_EMPTY__
    bar_empty
#elif __has_embed("bar.bin") == __STDC_EMBED_NOT_FOUND__
    bar_not_found
#else
    bar_otherwise
#endif

#if __has_embed("baz.bin") == __STDC_EMBED_FOUND__
    baz_found
#elif __has_embed("baz.bin") == __STDC_EMBED_EMPTY__
    baz_empty
#elif __has_embed("baz.bin") == __STDC_EMBED_NOT_FOUND__
    baz_not_found
#else
    baz_otherwise
#endif
"#,
                &[],
                &[
                    ("resources/foo.bin", &[1, 2, 3]),
                    ("resources/bar.bin", &[])
                ],
                &[],
            )),
            "11 17 29 foo_found bar_empty baz_not_found"
        );
    }

    #[test]
    fn test_process_operator_has_c_attribute() {
        // Test `#has_c_attribute`
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#if __has_c_attribute(deprecated)
    11
#else
    13
#endif

#if __has_c_attribute(deprecated) == 201904
    17
#else
    19
#endif
                "#,
                &[],
                &[],
                &[],
            )),
            "11 17"
        );

        // Test `#has_c_attribute` with non-existing attribute
        assert_eq!(
            print_tokens(&process_multiple_get_tokens(
                r#"
#if __has_c_attribute(__non_existing__)
    11
#else
    13
#endif
                "#,
                &[],
                &[],
                &[],
            )),
            "13"
        );
    }

    #[test]
    fn test_process_include_guard_check() {
        // Empty header file
        assert!(matches!(
            process_multiple(
                r#"
#include "foo.h"
"#,
                &[("header/foo.h", "",)],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .first(),
            Some(Prompt::Message(PromptLevel::Warning, 1, _))
        ));

        // Header file without include guard or `#pragma once`
        assert!(matches!(
            process_multiple(
                r#"
#include "foo.h"
"#,
                &[(
                    "header/foo.h",
                    r#"
#define FOO 42
"#,
                )],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .first(),
            Some(Prompt::Message(PromptLevel::Warning, 1, _))
        ));

        // Header file with `#pragma once`
        assert!(
            process_multiple(
                r#"
#include "foo.h"
"#,
                &[(
                    "header/foo.h",
                    r#"
#pragma once
#define FOO 42
"#,
                )],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .is_empty()
        );

        // Header file with include guard
        assert!(matches!(
            process_multiple(
                r#"
#include "foo.h"
"#,
                &[(
                    "header/foo.h",
                    r#"
#ifndef FOO_H
#define FOO_H
    #define FOO 42
#endif
"#,
                )],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .first(),
            Some(Prompt::Message(PromptLevel::Info, 1, _))
        ));

        // Header file with include guard and the macro name following the suggested convention
        assert!(matches!(
            process_multiple(
                r#"
#include "foo.h"
"#,
                &[(
                    "header/foo.h",
                    r#"
#ifndef PREFIX_FOO_H
#define PREFIX_FOO_H
    #define FOO 42
#endif
"#,
                )],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .first(),
            Some(Prompt::Message(PromptLevel::Info, 1, _))
        ));

        // Header file with include guard but the macro name does not follow the suggested one
        assert!(matches!(
            process_multiple(
                r#"
#include "foo.h"
"#,
                &[(
                    "header/foo.h",
                    r#"
#ifndef BAR_H
#define BAR_H
    #define FOO 42
#endif
"#,
                )],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .first(),
            Some(Prompt::MessageWithRange(
                PromptLevel::Info,
                1,
                _,
                Range {
                    start: Position {
                        index: 9,
                        line: 1,
                        column: 8
                    },
                    end_included: Position {
                        index: 13,
                        line: 1,
                        column: 12
                    }
                }
            ))
        ));
    }

    #[test]
    fn test_process_adjacent_string_literals() {
        let predefinitions = HashMap::new();

        assert_eq!(
            print_tokens(&process_single_get_tokens(
                r#"
"foo" "bar"
"buz"

#define STR1 "Hello, "
#define STR2 "world!"
#define STR3 STR1 STR2

STR1
STR2
2025
STR3
"#,
                &predefinitions,
            )),
            r#""foobarbuzHello, world!" 2025 "Hello, world!""#
        );

        assert_eq!(
            print_tokens(&process_single_get_tokens(
                r#"
#define FOO(x) x

// adjacent strings as argument
FOO(
    "Hello"
    ","
    " "
    "World!"
)
"#,
                &predefinitions,
            )),
            r#""Hello, World!""#
        );

        // err: concatenates string literals with different encoding types.
        assert!(matches!(
            process_single(r#""abc" u8"xyz""#, &predefinitions),
            Err(
                PreprocessFileError {
                    file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                    error: PreprocessError::MessageWithRange(
                        _,
                        range
                    )
                }
            ) if range == Range::from_detail(6, 0, 6, 7)
        ));
    }
}
