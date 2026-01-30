// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    vec,
};

use chrono::Local;

use crate::{
    ast::{Branch, Condition, Define, If, Pragma, Program, Statement},
    context::{
        Context, FileItem, FileLocation, FilePathResolveResult, FilePathSource, FileProvider,
        HeaderFileCache, MacroDefinition, MacroManipulationResult, PreprocessResult, Prompt,
        PromptLevel,
    },
    error::{PreprocessError, PreprocessFileError},
    expression::evaluate,
    location::Location,
    parser::parse_from_str,
    peekable_iter::PeekableIter,
    range::Range,
    token::{
        CharEncoding, IntegerNumber, IntegerNumberWidth, Number, Punctuator, StringEncoding, Token,
        TokenWithLocation, TokenWithRange,
    },
};

const PEEK_BUFFER_LENGTH_CODE_PARSE: usize = 2;
const PEEK_BUFFER_LENGTH_CONCATENATE_STRING: usize = 2;

/// Extra operators supported in the conditional directives.
///
/// They can be used in the expressions of `#if` and `#elif` directives.
/// `#if __has_include("header.h")`
///
/// Their names also are used for checking if these operators are defined:
/// - `#ifdef __has_include`
/// - `#ifdef __has_embed`
/// - `#ifdef __has_c_attribute`
const EXTRA_OPERATORS: [&str; 3] = ["__has_include", "__has_embed", "__has_c_attribute"];

/// Preprocesses a C source file.
///
/// see also:
/// - https://en.cppreference.com/w/c/language.html
/// - https://en.cppreference.com/w/c/preprocessor.html
/// - https://en.cppreference.com/w/c/language/translation_phases.html
#[allow(clippy::too_many_arguments)]
pub fn process_source_file<T>(
    // The file provider used to load source and header files.
    file_provider: &T,

    // The cache for header files to avoid redundant parsing.
    // Only header files (`*.h`) are cached, not source files (`*.c`).
    header_file_cache: &mut HeaderFileCache,

    // Identifiers which are used to prevent defining macros with these names.
    // They are usually C keywords, such as `int`, `return`, `if`, `else`, etc.
    // You may simply use `token::C23_KEYWORDS`.
    reserved_identifiers: &[&str],

    // The predefined macros to be used during preprocessing.
    // Such as `__STDC__` and `__STDC_VERSION__` which are provided by the compiler.
    // Note that some macros like `__FILE__`, `__LINE__`, `__DATE__`, and `__TIME__`
    // are defined by the preprocessor and do not need to be included here.
    //
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

    // A flag that controls whether function-like macro arguments may consist of multiple tokens.
    //
    // When false (the default), ANCPP requires each macro argument to be a single token
    // (identifier, number, string literal, or character literal). When true, an argument
    // may be an arbitrary sequence of tokens, allowing more complex expressions to be
    // passed as a single parameter.
    enable_multiple_token_argument: bool,

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
        header_file_cache,
        reserved_identifiers,
        predefinitions,
        resolve_relative_path_within_current_file,
        enable_multiple_token_argument,
        source_file_number,
        source_file_path_name,
        source_file_canonical_full_path,
    )?;

    // Add source file to the `included_files` list to
    // prevent self-inclusion or reversive inclusion.
    processor.context.included_files.push(FileLocation::new(
        FilePathSource::from_source_file(source_file_path_name),
        source_file_canonical_full_path,
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

    // concatenates adjacent string literals
    // e.g. "hello" " " "world" -> "hello world"
    // see: https://en.cppreference.com/w/c/language/translation_phases.html
    let Context {
        prompts, output, ..
    } = processor.context;

    let mut iter = output.into_iter();
    let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CONCATENATE_STRING);
    let concatenated = concatenate_adjacent_strings(&mut peekable_iter)?;

    let result = PreprocessResult {
        output: concatenated,
        prompts,
    };

    Ok(result)
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
        enable_multiple_token_argument: bool,
        source_file_number: usize,
        source_file_path_name: &Path,
        source_file_canonical_full_path: &Path,
    ) -> Result<Self, PreprocessFileError> {
        let context = Context::from_predefinitions(
            file_provider,
            file_cache,
            reserved_identifiers,
            predefinitions,
            resolve_relative_path_within_current_file,
            enable_multiple_token_argument,
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
            Statement::Define(define, range) => self.process_define(define, range),
            Statement::Undef(identifier, directive_range, identifier_range) => {
                self.process_undefine(identifier, directive_range, identifier_range)
            }
            Statement::Include(components, range) => self.process_include(components, range),
            Statement::Embed(components, range) => self.process_embed(components, range),
            Statement::If(if_) => self.process_if(if_),
            Statement::Error(message, directive_range, message_range) => {
                self.process_error(message, directive_range, message_range)
            }
            Statement::Warning(message, directive_range, message_range) => {
                self.process_warnning(message, directive_range, message_range)
            }
            Statement::Pragma(pragma, range) => self.process_pragma(pragma, range),
            Statement::Code(token_with_ranges) => self.process_code(token_with_ranges),
        }
    }

    fn process_define(
        &mut self,
        define: &Define,
        _directive_range: &Range,
    ) -> Result<(), PreprocessFileError> {
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

                if add_result == MacroManipulationResult::AlreadyExist {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            format!("Macro '{}' is already exist.", name),
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

                if result == MacroManipulationResult::AlreadyExist {
                    return Err(PreprocessFileError {
                        file_number: self.context.current_file_item.number,
                        error: PreprocessError::MessageWithRange(
                            format!("Macro '{}' has already been defined.", name),
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
        _directive_range: &Range,
        identifier_range: &Range,
    ) -> Result<(), PreprocessFileError> {
        let result = self.context.macro_map.remove(identifier);

        if result == MacroManipulationResult::NotFound {
            return Err(PreprocessFileError {
                file_number: self.context.current_file_item.number,
                error: PreprocessError::MessageWithRange(
                    format!("Macro '{}' is not defined.", identifier),
                    *identifier_range,
                ),
            });
        }

        Ok(())
    }

    fn process_include(
        &mut self,
        components: &[TokenWithRange],
        directive_range: &Range,
    ) -> Result<(), PreprocessFileError> {
        // The `include` directive is used to include the contents of another file.

        // Convert TokenWithRange to TokenWithLocation
        let tokens = components
            .iter()
            .map(|twr| {
                TokenWithLocation::new(
                    twr.token.clone(),
                    Location::new(self.context.current_file_item.number, &twr.range),
                )
            })
            .collect::<Vec<_>>();

        let expanded_tokens =
            self.expand_replacement(ExpansionType::Normal, &[], &[], &[], tokens)?;

        if expanded_tokens.is_empty() {
            return Err(PreprocessFileError::new(
                self.context.current_file_item.number,
                PreprocessError::MessageWithRange(
                    "#include directive requires a file path.".to_owned(),
                    *directive_range,
                ),
            ));
        }

        let include_resolve_result = self.resolve_include(expanded_tokens)?;

        let (canonical_full_path, is_system_file) = if include_resolve_result.is_system_file {
            match self
                .context
                .file_provider
                .resolve_system_file(&include_resolve_result.file_path)
            {
                Some(canonical_full_path) => (canonical_full_path, true),
                None => {
                    return Err(PreprocessFileError::new(
                        include_resolve_result.file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "System header file '{}' not found.",
                                include_resolve_result.file_path.to_string_lossy()
                            ),
                            include_resolve_result.file_path_location.range,
                        ),
                    ));
                }
            }
        } else {
            match self.context.file_provider.resolve_user_file_with_fallback(
                &include_resolve_result.file_path,
                &self.context.current_file_item.location.canonical_full_path,
                self.context.resolve_relative_path_within_current_file,
            ) {
                Some(FilePathResolveResult {
                    canonical_full_path,
                    is_system_file,
                }) => (canonical_full_path, is_system_file),
                None => {
                    return Err(PreprocessFileError::new(
                        include_resolve_result.file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "Header file '{}' not found.",
                                include_resolve_result.file_path.to_string_lossy()
                            ),
                            include_resolve_result.file_path_location.range,
                        ),
                    ));
                }
            }
        };

        // Check if the file is already included.
        if self.context.contains_include_file(&canonical_full_path) {
            // If the file is already included, we skip it.
            return Ok(());
        }

        // add the file to the included files to prevent multiple inclusions.
        // Note that ANCPP only includes header files once, even if the header file
        // has no include guards or `#pragma once`.
        self.context.included_files.push(FileLocation::new(
            if is_system_file {
                FilePathSource::from_system_header_file(&include_resolve_result.file_path)
            } else {
                FilePathSource::from_user_header_file(&include_resolve_result.file_path)
            },
            &canonical_full_path,
        ));

        // Load the file content from the file cache.
        if let Some(cache_item) = self
            .context
            .header_file_cache
            .get_by_canonical_full_path(&canonical_full_path)
        {
            let program = cache_item.program.clone();
            self.process_included_header_file(
                cache_item.file_number,
                is_system_file,
                &canonical_full_path,
                &include_resolve_result.file_path,
                &program,
            )?;
        } else {
            // Load the file content.
            let file_content = self
                .context
                .file_provider
                .load_text_file(&canonical_full_path)
                .map_err(|error| {
                    PreprocessFileError::new(
                        include_resolve_result.file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!("Failed to load header file: {}", error),
                            include_resolve_result.file_path_location.range,
                        ),
                    )
                })?;

            // add the file to the cache
            let file_number = self.context.header_file_cache.next_file_number();

            // Parse the file content to get the program.
            let program = parse_from_str(&file_content)
                .map_err(|error| PreprocessFileError::new(file_number, error))?;

            // update cache
            self.context.header_file_cache.add(
                &canonical_full_path,
                &file_content,
                file_number,
                program.clone(),
            );

            // check header guard or `#pragma once`
            self.check_pragma_once_and_include_guard(
                file_number,
                &include_resolve_result.file_path,
                &program,
            );

            self.process_included_header_file(
                file_number,
                is_system_file,
                &canonical_full_path,
                &include_resolve_result.file_path,
                &program,
            )?;
        }

        Ok(())
    }

    fn process_embed(
        &mut self,
        components: &[TokenWithRange],
        directive_range: &Range,
    ) -> Result<(), PreprocessFileError> {
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

        // Convert TokenWithRange to TokenWithLocation
        let tokens = components
            .iter()
            .map(|twr| {
                TokenWithLocation::new(
                    twr.token.clone(),
                    Location::new(self.context.current_file_item.number, &twr.range),
                )
            })
            .collect::<Vec<_>>();

        let expanded_tokens =
            self.expand_replacement(ExpansionType::Normal, &[], &[], &[], tokens)?;

        if expanded_tokens.is_empty() {
            return Err(PreprocessFileError::new(
                self.context.current_file_item.number,
                PreprocessError::MessageWithRange(
                    "#embed directive requires a file path.".to_owned(),
                    *directive_range,
                ),
            ));
        }

        let embed_resolve_result = self.resolve_embed(expanded_tokens)?;

        let (canonical_full_path, _is_system_file) = if embed_resolve_result.is_system_file {
            match self
                .context
                .file_provider
                .resolve_system_file(&embed_resolve_result.file_path)
            {
                Some(resolved_path) => (resolved_path, true),
                None => {
                    return Err(PreprocessFileError::new(
                        embed_resolve_result.file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "System binary file '{}' not found.",
                                embed_resolve_result.file_path.to_string_lossy()
                            ),
                            embed_resolve_result.file_path_location.range,
                        ),
                    ));
                }
            }
        } else {
            match self.context.file_provider.resolve_user_file_with_fallback(
                &embed_resolve_result.file_path,
                &self.context.current_file_item.location.canonical_full_path,
                self.context.resolve_relative_path_within_current_file,
            ) {
                Some(FilePathResolveResult {
                    canonical_full_path,
                    is_system_file,
                }) => (canonical_full_path, is_system_file),
                None => {
                    return Err(PreprocessFileError::new(
                        embed_resolve_result.file_path_location.file_number,
                        PreprocessError::MessageWithRange(
                            format!(
                                "Binary file '{}' not found.",
                                embed_resolve_result.file_path.to_string_lossy()
                            ),
                            embed_resolve_result.file_path_location.range,
                        ),
                    ));
                }
            }
        };

        // Convert u8 array to `TokenWithLocation` array.
        // Each byte is represented as a hexadecimal number token,
        // and separated by commas.
        let convert_binary_data_to_tokens = |data: &[u8]| -> Vec<TokenWithLocation> {
            let mut tokens = Vec::new();

            for (i, byte) in data.iter().enumerate() {
                if i > 0 {
                    tokens.push(TokenWithLocation::new(
                        Token::Punctuator(Punctuator::Comma),
                        Location::default(),
                    ));
                }

                tokens.push(TokenWithLocation::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        format!("0x{:02x}", byte),
                        false,
                        IntegerNumberWidth::Default,
                    ))),
                    Location::default(),
                ));
            }

            tokens
        };

        let binary_data = self
            .context
            .file_provider
            .load_binary_file(&canonical_full_path, 0, embed_resolve_result.limit)
            .map_err(|error| {
                PreprocessFileError::new(
                    embed_resolve_result.file_path_location.file_number,
                    PreprocessError::MessageWithRange(
                        format!("Failed to load binary file: {}", error),
                        embed_resolve_result.file_path_location.range,
                    ),
                )
            })?;

        if binary_data.is_empty() {
            if let Some(fallback_data) = embed_resolve_result.if_empty {
                // If the file is empty, we output the `if_empty` content.
                let tokens = convert_binary_data_to_tokens(&fallback_data);
                self.context.output.extend(tokens);
            }
        } else {
            // Output the binary data as a sequence of numbers.
            if let Some(prefix_data) = embed_resolve_result.prefix {
                self.context
                    .output
                    .extend(convert_binary_data_to_tokens(&prefix_data));

                self.context.output.push(TokenWithLocation::new(
                    Token::Punctuator(Punctuator::Comma),
                    Location::default(),
                ));
            }

            self.context
                .output
                .extend(convert_binary_data_to_tokens(&binary_data));

            if let Some(suffix_data) = embed_resolve_result.suffix {
                self.context.output.push(TokenWithLocation::new(
                    Token::Punctuator(Punctuator::Comma),
                    Location::default(),
                ));

                self.context
                    .output
                    .extend(convert_binary_data_to_tokens(&suffix_data));
            }
        }

        Ok(())
    }

    fn process_if(&mut self, if_: &If) -> Result<(), PreprocessFileError> {
        // Indicates whether any branch has been taken.
        // To skip the alternative block if a branch is already taken.
        let mut hit_branch = false;

        for branch in &if_.branches {
            // Evaluate the condition of the branch.
            let evaluate_result: isize = match &branch.condition {
                Condition::Defined(identifier, _) => {
                    if self.context.macro_map.contains_key(identifier)
                        || EXTRA_OPERATORS.contains(&identifier.as_str())
                    {
                        1
                    } else {
                        0
                    }
                }
                Condition::NotDefined(identifier, _) => {
                    if self.context.macro_map.contains_key(identifier)
                        || EXTRA_OPERATORS.contains(&identifier.as_str())
                    {
                        0
                    } else {
                        1
                    }
                }
                Condition::Expression(tokens) => {
                    // Evaluate the expression.

                    // Convert `TokenWithRange` to `TokenWithLocation`.
                    let token_with_locations = tokens
                        .iter()
                        .map(|item| {
                            TokenWithLocation::new(
                                item.token.clone(),
                                Location::new(self.context.current_file_item.number, &item.range),
                            )
                        })
                        .collect::<Vec<_>>();

                    let expanded_tokens = self.expand_replacement(
                        ExpansionType::ConditionalExpression,
                        &[],
                        &[],
                        &[],
                        token_with_locations,
                    )?;

                    evaluate(&expanded_tokens, self.context.current_file_item.number)?
                }
            };

            if evaluate_result != 0 {
                hit_branch = true;

                for statement in &branch.consequence {
                    self.process_statement(statement)?;
                }

                // Exit after processing the first true branch.
                break;
            }
        }

        if !hit_branch {
            // If no branch was true, process the alternative if it exists.
            if let Some((alternative_statements, _)) = &if_.alternative {
                for statement in alternative_statements {
                    self.process_statement(statement)?;
                }
            }
        }

        Ok(())
    }

    fn process_error(
        &mut self,
        message: &str,
        directive_range: &Range,
        message_range: &Range,
    ) -> Result<(), PreprocessFileError> {
        let range = Range::new(&directive_range.start, &message_range.end_included);

        Err(PreprocessFileError {
            file_number: self.context.current_file_item.number,
            error: PreprocessError::MessageWithRange(
                format!("User defined error: {}", message),
                range,
            ),
        })
    }

    fn process_warnning(
        &mut self,
        message: &str,
        _directive_range: &Range,
        message_range: &Range,
    ) -> Result<(), PreprocessFileError> {
        let prompt = Prompt::MessageWithRange(
            PromptLevel::Warning,
            self.context.current_file_item.number,
            format!("User defined warning: {}", message),
            *message_range,
        );

        self.context.prompts.push(prompt);
        Ok(())
    }

    fn process_pragma(
        &mut self,
        _pragma: &Pragma,
        _directive_range: &Range,
    ) -> Result<(), PreprocessFileError> {
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

        let expanded_tokens =
            self.expand_replacement(ExpansionType::Normal, &[], &[], &[], token_with_locations)?;

        self.context.output.extend(expanded_tokens);

        Ok(())
    }

    /// Expand replacements.
    ///
    /// This function handles the expansion of:
    /// - Object-like macro replacement.
    /// - Function-like macro replacement.
    /// - Conditional expressions.
    /// - Tokens in the `#include` and `#embed` directives.
    /// - Tokens in code statements.
    ///
    /// This produre consists of several steps:
    /// 1. If expanding a function-like macro definition, substitute the parameters
    ///    with the provided arguments.
    /// 2. Expand macros within the replacement text.
    /// 3. If expanding a conditional expression, process special operators.
    fn expand_replacement(
        &mut self,

        // Indicates the context in which this expansion occurs.
        expansion_type: ExpansionType,

        // The stack of macro expansions.
        //
        // The "macro expansion stack" is used to keep track of the macros
        // that are currently being expanded. This is important to prevent
        // infinite recursion when a macro expands to itself, either directly
        // or indirectly through other macros. It is a bit like a call stack in
        // programming languages.
        //
        // The last item in the stack is the currently expanded macro.
        //
        // For non-macro expansions (i.e., normal code statements,
        // inclusions, conditional expressions), this stack is empty.
        macros_expansion_stack: &[MacroExpansionStackItem],

        // The parameter names of the function-like macro.
        // If the macro is variadic, the last parameter name is `...`.
        //
        // For non-function-like macros, this map is empty.
        parameter_names: &[String],

        // The actual arguments of function-like macros invocations.
        // All values should be the expanded first.
        //
        // The argument expansion is taken place **after** passing the arguments
        // and **before** substituting them into the replacement text.
        //
        // If the macro is variadic, the last parameter is the rest parameters,
        // which contains all remaining arguments, possibly empty.
        //
        // For non-function-like macros, this map is empty.
        actual_arguments: &[Argument],

        // The tokens to be expanded.
        //
        // - code statements.
        // - definition of a macro.
        body: Vec<TokenWithLocation>,
    ) -> Result<Vec<TokenWithLocation>, PreprocessFileError> {
        // If we are expanding a function-like macro definition,
        // substitute the parameters first.
        let mut output = if expansion_type == ExpansionType::FunctionLikeDefinition {
            self.substitute_macro_parameters(parameter_names, actual_arguments, body)?
        } else {
            body
        };

        // Expand macros, including object-like macros, function-like macros.
        output =
            self.expand_macros_within_replacement(expansion_type, macros_expansion_stack, output)?;

        // If we are expanding a conditional expression,
        // process special operators.
        if expansion_type == ExpansionType::ConditionalExpression {
            output = self.process_conditional_expression_operators(output)?;
        }

        Ok(output)
    }

    /// Substitute macro parameters in the function-likemacro body with
    /// the provided arguments.
    ///
    /// Invoke this function only when expanding a function-like macro definition.
    fn substitute_macro_parameters(
        &mut self,
        parameter_names: &[String],
        actual_arguments: &[Argument],
        body: Vec<TokenWithLocation>,
    ) -> Result<Vec<TokenWithLocation>, PreprocessFileError> {
        if body.is_empty() {
            return Ok(vec![]);
        }

        let first_token_with_location = body.first().unwrap().clone();

        let mut output = Vec::new();
        let mut iter = body.into_iter();
        let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CODE_PARSE);
        let mut code_parser = CodeParser::new(
            &mut peekable_iter,
            &first_token_with_location.token,
            &first_token_with_location.location,
        );

        while let Some(current_token_with_location) = code_parser.next_token_with_location() {
            match &current_token_with_location.token {
                _ if code_parser.peek_token_and_equals(0, &Token::PoundPound) => {
                    // Process token concatenation (the `##` operator).
                    //
                    // Token concatenation joins two or more tokens into a single identifier.
                    // The tokens are concatenated without any whitespace in between, and the tokens
                    // are not expanded before concatenation.
                    // Token concatenation can only be used in function-like macro replacements.
                    //
                    // The result of _Token concatenation_ operator (`##`) must be a valid C identifier:
                    // the first character  must be an alphabet or underscore, and the remaining characters
                    // must be either alphabet, underscore or integer numbers.
                    //
                    // e.g.:
                    //
                    // - `foo##bar`: valid
                    // - `sprite##2##b`: valid
                    // - `9##s`: invalid
                    // - `+##=`: invalid

                    // Collect tokens of token concatenation.
                    let mut components = vec![];

                    // Add the current token as the first component.
                    components.push(current_token_with_location);

                    // Consume `##` and collect following tokens.
                    while code_parser.peek_token_and_equals(0, &Token::PoundPound) {
                        code_parser.next_token(); // consume `##`

                        if let Some(next_token_with_location) =
                            code_parser.next_token_with_location()
                        {
                            components.push(next_token_with_location);
                        } else {
                            return Err(PreprocessFileError {
                                file_number: code_parser.last_location.file_number,
                                error: PreprocessError::MessageWithRange(
                                    "Token concatenation (##) must be followed by a token."
                                        .to_owned(),
                                    code_parser.last_location.range,
                                ),
                            });
                        }
                    }

                    // Convert tokens to identifier parts (strings).
                    let mut component_names = vec![];
                    for token_with_location in &components {
                        match &token_with_location.token {
                            Token::Identifier(name) => {
                                match name.as_str() {
                                    "__VA_ARGS__" => {
                                        // ANCPP does not support this operation.
                                        // e.g., `#define FOO(x) x ## __VA_ARGS__`
                                        return Err(PreprocessFileError {
                                            file_number: token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "Token concatenation (##) with __VA_ARGS__ is not supported."
                                                    .to_owned(),
                                                token_with_location.location.range,
                                            ),
                                        });
                                    }
                                    "__VA_OPT__" => {
                                        // ANCPP does not support this operation.
                                        return Err(PreprocessFileError {
                                            file_number: token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "Token concatenation (##) with __VA_OPT__ is not supported."
                                                    .to_owned(),
                                                token_with_location.location.range,
                                            ),
                                        });
                                    }
                                    _ => {
                                        let parameter_index_opt =
                                            parameter_names.iter().position(|p| p == name);

                                        match parameter_index_opt {
                                            Some(parameter_index) => {
                                                // The identifier is a parameter.

                                                // Substitute the parameter with the corresponding argument.
                                                let argument = &actual_arguments[parameter_index];
                                                let Argument::Position(argument_value) = argument
                                                else {
                                                    // This should not happen for regular parameters.
                                                    unreachable!()
                                                };

                                                if argument_value.origin.len() > 1 {
                                                    return Err(PreprocessFileError {
                                                        file_number: token_with_location
                                                            .location
                                                            .file_number,
                                                        error: PreprocessError::MessageWithRange(
                                                            "Token concatenation (##) with a parameter that expands to multiple tokens is not allowed."
                                                                .to_owned(),
                                                            token_with_location.location.range,
                                                        ),
                                                    });
                                                }

                                                let id = argument_value
                                                    .origin
                                                    .first()
                                                    .unwrap()
                                                    .token
                                                    .to_string();

                                                component_names.push(id);
                                            }
                                            _ => {
                                                // The identifier is not a parameter, just push it as is.
                                                component_names.push(name.clone());
                                            }
                                        }
                                    }
                                }
                            }
                            Token::Number(Number::Integer(value)) => {
                                component_names.push(value.value.clone());
                            }
                            Token::String(value, _) => {
                                component_names.push(value.clone());
                            }
                            Token::Char(value, _) => {
                                component_names.push(value.to_string());
                            }
                            _ => {
                                return Err(PreprocessFileError {
                                    file_number: token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "Token concatenation (##) can only apply to identifiers, integer numbers, strings, or character literals."
                                            .to_owned(),
                                        token_with_location.location.range,
                                    ),
                                });
                            }
                        }
                    }

                    // Create the concatenated identifier token.
                    let concatenated_name = component_names.join("");
                    let concatenated_location = Location::new(
                        components.first().unwrap().location.file_number,
                        &Range::new(
                            &components.first().unwrap().location.range.start,
                            &components.last().unwrap().location.range.end_included,
                        ),
                    );

                    // Check if the concatenated name is a valid identifier.
                    if !is_valid_identifier(&concatenated_name) {
                        return Err(PreprocessFileError {
                            file_number: concatenated_location.file_number,
                            error: PreprocessError::MessageWithRange(
                                format!(
                                    "The result of token concatenation (##) is not a valid identifier: '{}'.",
                                    concatenated_name
                                ),
                                concatenated_location.range,
                            ),
                        });
                    }

                    let concatenated_token = TokenWithLocation::new(
                        Token::Identifier(concatenated_name),
                        concatenated_location,
                    );

                    // Push the concatenated identifier to the output.
                    output.push(concatenated_token);
                }
                Token::Identifier(name) => {
                    // Process parameter substitution and special macros.
                    match name.as_str() {
                        "__VA_ARGS__" => {
                            // `__VA_ARGS__` is a special macro that represents the variable arguments
                            // in function-like macros.
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/replace.html

                            // Defence: __VA_ARGS__ can only be used in variadic function-like macros.
                            if !matches!(parameter_names.last(), Some(param) if param == "...") {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "__VA_ARGS__ can only be used in variadic function-like macros."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            // In the variadic function, the rest arguments are stored
                            // in the last parameter named `...`.
                            let Some(Argument::Rest(argument_values)) = actual_arguments.last()
                            else {
                                unreachable!()
                            };

                            // Push all variadic argument tokens to the output.
                            for (index, argument_value) in argument_values.iter().enumerate() {
                                if let Some(tls) = &argument_value.expanded {
                                    output.extend(tls.clone());
                                } else {
                                    output.extend(argument_value.origin.clone());
                                }

                                // Add comma separator between arguments.
                                if index < argument_values.len() - 1 {
                                    output.push(TokenWithLocation::new(
                                        Token::Punctuator(Punctuator::Comma),
                                        Location::default(),
                                    ));
                                }
                            }
                        }
                        "__VA_OPT__" => {
                            // `__VA_OPT__ ( content )` is replaced by content if __VA_ARGS__ is non-empty,
                            // and expands to nothing otherwise.

                            // Defence: __VA_OPT__ can only be used in variadic function-like macros.
                            if !matches!(parameter_names.last(), Some(param) if param == "...") {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "__VA_OPT__ can only be used in variadic function-like macros."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            // Collect tokens inside the parentheses.
                            let mut components = Vec::new();
                            let mut parenthises_depth = 0;

                            code_parser.consume_opening_paren()?; // Consumes '('

                            while let Some(next_token_with_location) =
                                code_parser.next_token_with_location()
                            {
                                match &next_token_with_location.token {
                                    Token::Punctuator(Punctuator::ParenthesisOpen) => {
                                        // Increase depth for nested parentheses.
                                        parenthises_depth += 1;
                                    }
                                    Token::Punctuator(Punctuator::ParenthesisClose) => {
                                        if parenthises_depth == 0 {
                                            // Reached the matching closing parenthesis.
                                            break;
                                        } else {
                                            // Decrease depth for nested parentheses.
                                            parenthises_depth -= 1;
                                        }
                                    }
                                    _ => {
                                        // Other tokens are just collected.
                                    }
                                }

                                components.push(next_token_with_location);
                            }

                            // The closing parenthesis has already been consumed by the loop above.
                            // Do not need to consume it again.

                            // In the variadic function, the rest arguments are stored
                            // in the last parameter named `...`.
                            let Some(Argument::Rest(argument_values)) = actual_arguments.last()
                            else {
                                unreachable!()
                            };

                            // `__VA_OPT__( content )` is only expanded if variadic arguments are provided.
                            if !argument_values.is_empty() {
                                let expanded_tokens = self.substitute_macro_parameters(
                                    parameter_names,
                                    actual_arguments,
                                    components,
                                )?;

                                output.extend(expanded_tokens);
                            }
                        }
                        _ => {
                            // Process regular identifiers.

                            let parameter_index_opt =
                                parameter_names.iter().position(|p| p == name);

                            match parameter_index_opt {
                                // The identifier is a parameter.
                                Some(parameter_index) => {
                                    // Substitute the parameter with the corresponding argument.
                                    let argument = &actual_arguments[parameter_index];
                                    let Argument::Position(argument_value) = argument else {
                                        // This should not happen for regular parameters.
                                        unreachable!()
                                    };

                                    if let Some(tls) = &argument_value.expanded {
                                        output.extend(tls.clone());
                                    } else {
                                        output.extend(argument_value.origin.clone());
                                    }
                                }
                                _ => {
                                    // The identifier is not a parameter, just push it as is.
                                    output.push(current_token_with_location);
                                }
                            }
                        }
                    }
                }
                Token::Pound => {
                    // _Stringizing_ (the `#` operator).
                    //
                    // Stringizing is used to produce a string literal and can only be applied to
                    // macro parameters (includes `__VA_ARGS__`).
                    // For example, `#define FOO(x) #x` will expand `FOO(abc)` to `"abc"` and `FOO(123)` to `"123"`.

                    // expect `# parameter`
                    if let Some(next_token_with_location) = code_parser.next_token_with_location() {
                        match &next_token_with_location.token {
                            Token::Identifier(name) => {
                                if name == "__VA_ARGS__" {
                                    // Stringizing __VA_ARGS__
                                    //
                                    // Example:
                                    //
                                    // ```c
                                    // #define SHOW_ALL(...) puts(#__VA_ARGS__);
                                    // SHOW_ALL(1, 2, foo, "bar");     // Expands to: `puts("1, 2, foo, \"bar\"");`
                                    // SHOW_ALL();                     // Expands to: `puts("");`
                                    // ```

                                    // Defence: __VA_ARGS__ can only be used in variadic function-like macros.
                                    if !matches!(parameter_names.last(), Some(param) if param == "...")
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

                                    // In the variadic function, the rest arguments are stored
                                    // in the last parameter named `...`.
                                    let Some(Argument::Rest(argument_values)) =
                                        actual_arguments.last()
                                    else {
                                        unreachable!()
                                    };

                                    // Convert the tokens to a string literal.
                                    let stringized_string = stringize_arguments(argument_values)?;
                                    let stringized_token = TokenWithLocation::new(
                                        Token::String(stringized_string, StringEncoding::Default),
                                        Location::default(),
                                    );
                                    output.push(stringized_token);
                                } else {
                                    // Stringizing regular parameter.

                                    let parameter_index_opt =
                                        parameter_names.iter().position(|p| p == name);

                                    match parameter_index_opt {
                                        // Found the parameter to be stringized.
                                        Some(parameter_index) => {
                                            // Convert the tokens to a string literal.
                                            let argument = &actual_arguments[parameter_index];
                                            let Argument::Position(argument_value) = argument
                                            else {
                                                unreachable!()
                                            };

                                            let stringized_string =
                                                stringize_tokens(&argument_value.origin)?;
                                            let stringized_token = TokenWithLocation::new(
                                                Token::String(
                                                    stringized_string,
                                                    StringEncoding::Default,
                                                ),
                                                Location::default(),
                                            );
                                            output.push(stringized_token);
                                        }
                                        _ => {
                                            // The identifier is not a macro parameter.
                                            // e.g.
                                            // `#define FOO(x) #y`   // 'y' is not a parameter.
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
                                // The next token is not an identifier.
                                // e.g.,
                                // `#define FOO(x) #123`
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
                        // The `#` operator is not followed by any token.
                        // e.g.,
                        // `#define FOO(x) #`
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
                    // Token concatenation (the `##` operator)
                    // The token concatenation operator has already been handled in the
                    // `_ if code_parser.peek_token_and_equals(0, &Token::PoundPound) => { ... }` branch above.
                    //
                    // If the code reaches here, it means the `##` operator is not preceded by an identifier.

                    // e.g.,
                    // `#define FOO(x) ##x`
                    return Err(PreprocessFileError {
                        file_number: current_token_with_location.location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Token concatenation (##) must follow an identifier, string, or char."
                                .to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }
                _ => {
                    // The current token is not a parameter or stringizing operator,
                    // we simply copy it to the output.
                    output.push(current_token_with_location);
                }
            }
        }

        Ok(output)
    }

    /// Expand macros in the given tokens.
    fn expand_macros_within_replacement(
        &mut self,
        expansion_type: ExpansionType,
        macros_expansion_stack: &[MacroExpansionStackItem],
        body: Vec<TokenWithLocation>,
    ) -> Result<Vec<TokenWithLocation>, PreprocessFileError> {
        if body.is_empty() {
            return Ok(vec![]);
        }

        let first_token_with_location = body.first().unwrap().clone();

        let mut output = Vec::new();
        let mut iter = body.into_iter();
        let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CODE_PARSE);
        let mut code_parser = CodeParser::new(
            &mut peekable_iter,
            &first_token_with_location.token,
            &first_token_with_location.location,
        );

        while let Some(current_token_with_location) = code_parser.next_token_with_location() {
            match &current_token_with_location.token {
                Token::Identifier(name) => {
                    match name.as_str() {
                        "__FILE__" => {
                            let file_path = match &self.context.current_file_item.location.source {
                                FilePathSource::SourceFile(path_buf) => path_buf,
                                FilePathSource::UserHeader(path_buf) => path_buf,
                                FilePathSource::SystemHeader(path_buf) => path_buf,
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
                                    IntegerNumberWidth::Default,
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
                        "__STDC_EMBED_NOT_FOUND__" => {
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/replace.html
                            // - `__STDC_EMBED_NOT_FOUND__` => 0
                            // - `__STDC_EMBED_FOUND__` => 1
                            // - `__STDC_EMBED_EMPTY__` => 2

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    0.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__STDC_EMBED_FOUND__" => {
                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    1.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__STDC_EMBED_EMPTY__" => {
                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    2.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__Pragma" => {
                            // `__Pragma(string-literal)` is an operator that has the same effect
                            // as a `#pragma ...` directive. The difference is that `__Pragma` can
                            // be used inside macros.
                            // see:
                            // https://gcc.gnu.org/onlinedocs/cpp/Pragmas.html
                            //
                            // ANCPP just ignores the pragmas for now.
                            if !matches!(
                                expansion_type,
                                ExpansionType::ObjectLikeDefinition
                                    | ExpansionType::FunctionLikeDefinition
                            ) {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "__Pragma can only be used in macro definitions."
                                            .to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            code_parser.consume_opening_paren()?; // Consumes '('

                            match code_parser.next_token() {
                                Some(Token::String(_, _)) => {
                                    // Just consume the string literal.
                                }
                                Some(_) => {
                                    return Err(PreprocessFileError {
                                        file_number: code_parser.last_location.file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "__Pragma must be followed by a string literal."
                                                .to_owned(),
                                            code_parser.last_location.range,
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

                            code_parser.consume_closing_paren()?; // Consumes ')'
                        }
                        "defined" => {
                            // `defined` is a preprocessor operator that checks if a macro is defined.
                            // It can be used in conditional expressions and as part of `#if` and `#elif`.
                            //
                            // Syntax:
                            // - `defined identifier`
                            // - `defined(identifier)`

                            if expansion_type != ExpansionType::ConditionalExpression {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "Operator `defined` can only be used in conditional expressions."
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
                                code_parser.consume_opening_paren()?; // Consumes '('
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
                                            "Operator `defined` must be followed by an identifier."
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
                                            "Operator `defined` must be followed by an identifier."
                                                .to_owned(),
                                            current_token_with_location.location.range,
                                        ),
                                    });
                                }
                            };

                            if has_parentheses {
                                code_parser.consume_closing_paren()?; // Consumes ')'
                            }

                            let number = if self.context.macro_map.contains_key(&identifier)
                                || EXTRA_OPERATORS.contains(&identifier.as_str())
                            {
                                1
                            } else {
                                0
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    number.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        _ => {
                            let is_invocation = code_parser.peek_token_and_equals(
                                0,
                                &Token::Punctuator(Punctuator::ParenthesisOpen),
                            );

                            let macro_item = self.context.macro_map.get(name);

                            match macro_item {
                                Some(MacroDefinition::ObjectLike(object_definition))
                                    if !is_invocation =>
                                {
                                    // Defence: check for self-reference to prevent infinite recursion.
                                    if contains_self_reference(macros_expansion_stack, name, false)
                                    {
                                        // The macro expansion stack contains the current macro,
                                        // which indicates a self-reference.
                                        // To prevent infinite recursion, we do not expand this macro.
                                        output.push(current_token_with_location);
                                    } else {
                                        // The identifier is an object-like macro.
                                        // Replace macro with its corresponding tokens.

                                        let mut new_macros_expansion_stack =
                                            macros_expansion_stack.to_vec();
                                        new_macros_expansion_stack
                                            .push(MacroExpansionStackItem::Object(name.to_owned()));

                                        let expanded_tokens = self.expand_replacement(
                                            ExpansionType::ObjectLikeDefinition,
                                            &new_macros_expansion_stack,
                                            &[],
                                            &[],
                                            object_definition.to_owned(),
                                        )?;

                                        output.extend(expanded_tokens);
                                    }
                                }
                                Some(MacroDefinition::FunctionLike(
                                    function_parameters_ref,
                                    function_definition_ref,
                                )) if is_invocation => {
                                    // Defence: check for self-reference to prevent infinite recursion.
                                    if contains_self_reference(macros_expansion_stack, name, true) {
                                        // The macro expansion stack contains the current macro,
                                        // which indicates a self-reference.
                                        // To prevent infinite recursion, we do not expand this macro.
                                        output.push(current_token_with_location);
                                    } else {
                                        // The identifier is a function-like macro.
                                        // Process the macro invocation.

                                        let function_parameters =
                                            function_parameters_ref.to_owned();
                                        let function_definition =
                                            function_definition_ref.to_owned();

                                        // Collect the arguments for the macro invocation.
                                        let mut argument_token_with_locationss = vec![];
                                        code_parser.consume_opening_paren()?; // Consumes '('

                                        while let Some(argument_token) = code_parser.peek_token(0) {
                                            if matches!(
                                                argument_token,
                                                Token::Punctuator(Punctuator::ParenthesisClose)
                                            ) {
                                                // If the next token is a closing parenthesis, we have reached the end of the arguments.
                                                break;
                                            }

                                            // Collect tokens for the current argument.
                                            // Until we reach a comma (`,`) or closing parenthesis (`)`) at group level 0.
                                            let mut argument_token_with_locations = vec![];
                                            let mut group_level = 0;

                                            while let Some(argument_token) =
                                                code_parser.peek_token(0)
                                            {
                                                match &argument_token {
                                                    Token::Punctuator(
                                                        Punctuator::ParenthesisOpen,
                                                    ) => {
                                                        group_level += 1;
                                                    }
                                                    Token::Punctuator(
                                                        Punctuator::ParenthesisClose,
                                                    ) => {
                                                        if group_level == 0 {
                                                            // If we reach a closing parenthesis at group level 0,
                                                            // it indicates the end of the arguments.
                                                            break;
                                                        } else {
                                                            group_level -= 1;
                                                        }
                                                    }
                                                    Token::Punctuator(Punctuator::Comma) => {
                                                        if group_level == 0 {
                                                            // If we reach a comma at group level 0,
                                                            // it indicates the end of the current argument.
                                                            break;
                                                        }
                                                    }
                                                    _ => {}
                                                }

                                                // Collect the token as part of the current argument.
                                                let argument_location =
                                                    code_parser.peek_location(0).unwrap();

                                                argument_token_with_locations.push(
                                                    TokenWithLocation::new(
                                                        argument_token.clone(),
                                                        *argument_location,
                                                    ),
                                                );

                                                code_parser.next_token(); // consumes the token
                                            }

                                            argument_token_with_locationss
                                                .push(argument_token_with_locations);

                                            if let Some(argument_token) = code_parser.peek_token(0)
                                            {
                                                if matches!(
                                                    argument_token,
                                                    Token::Punctuator(Punctuator::Comma)
                                                ) {
                                                    // Consume the comma and continue to the next argument.
                                                    code_parser.next_token(); // consumes ','

                                                    if code_parser.peek_token_and_equals(
                                                        0,
                                                        &Token::Punctuator(
                                                            Punctuator::ParenthesisClose,
                                                        ),
                                                    ) {
                                                        // Trailing comma before closing parenthesis is not allowed.
                                                        return Err(PreprocessFileError {
                                                            file_number: current_token_with_location
                                                                .location
                                                                .file_number,
                                                            error: PreprocessError::MessageWithRange(
                                                                "Trailing comma in macro invocation is not allowed."
                                                                    .to_owned(),
                                                                current_token_with_location
                                                                    .location
                                                                    .range,
                                                            ),
                                                        });
                                                    }
                                                }
                                            } else {
                                                // Missing closing parenthesis or next token.
                                                return Err(PreprocessFileError {
                                                    file_number: current_token_with_location
                                                        .location
                                                        .file_number,
                                                    error: PreprocessError::MessageWithRange(
                                                        "Missing closing parenthesis in macro invocation."
                                                            .to_owned(),
                                                        current_token_with_location
                                                            .location
                                                            .range,
                                                    ),
                                                });
                                            }
                                        }

                                        code_parser.consume_closing_paren()?; // Consumes ')'

                                        // Check `enable_multiple_token_argument` flag
                                        if !self.context.enable_multiple_token_argument {
                                            // Check that each argument consists of exactly one token.
                                            for argument_token_with_locations in
                                                &argument_token_with_locationss
                                            {
                                                if argument_token_with_locations.len() != 1 {
                                                    return Err(PreprocessFileError {
                                                        file_number: current_token_with_location
                                                            .location
                                                            .file_number,
                                                        error: PreprocessError::MessageWithRange(
                                                            "Each macro argument must be a single token. To allow multiple tokens as arguments, enable relevant options."
                                                                .to_owned(),
                                                            current_token_with_location
                                                                .location
                                                                .range,
                                                        ),
                                                    });
                                                }
                                            }
                                        }

                                        // Expand arguments before invoking the macro.
                                        let mut argument_values = vec![];
                                        for argument_token_with_locations in
                                            argument_token_with_locationss
                                        {
                                            let argument_expansion_type = if expansion_type
                                                == ExpansionType::ConditionalExpression
                                            {
                                                ExpansionType::ConditionalExpression
                                            } else {
                                                ExpansionType::Normal
                                            };

                                            let expanded_argument_tokens = self
                                                .expand_macros_within_replacement(
                                                    argument_expansion_type,
                                                    macros_expansion_stack,
                                                    argument_token_with_locations.clone(),
                                                )?;

                                            argument_values.push(ArgumentValue {
                                                origin: argument_token_with_locations,
                                                expanded: Some(expanded_argument_tokens),
                                            });
                                        }

                                        // Assemble the arguments according to the function parameters.
                                        let mut arguments = vec![];
                                        for function_parameter in &function_parameters {
                                            if function_parameter == "..." {
                                                // Variadic parameter
                                                // Collect the rest arguments
                                                let rest = std::mem::take(&mut argument_values); // argument_values.drain(0..).collect();
                                                arguments.push(Argument::Rest(rest));
                                                break;
                                            } else {
                                                // Regular parameter
                                                if argument_values.is_empty() {
                                                    return Err(PreprocessFileError {
                                                        file_number: current_token_with_location
                                                            .location
                                                            .file_number,
                                                        error: PreprocessError::MessageWithRange(
                                                            format!(
                                                                "Not enough arguments provided for macro '{}'.",
                                                                name
                                                            ),
                                                            current_token_with_location
                                                                .location
                                                                .range,
                                                        ),
                                                    });
                                                }

                                                arguments.push(Argument::Position(
                                                    argument_values.remove(0),
                                                ));
                                            }
                                        }

                                        if !argument_values.is_empty() {
                                            return Err(PreprocessFileError {
                                                file_number: current_token_with_location
                                                    .location
                                                    .file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Too many arguments provided for macro '{}'.",
                                                        name
                                                    ),
                                                    current_token_with_location.location.range,
                                                ),
                                            });
                                        }

                                        let mut new_macros_expansion_stack =
                                            macros_expansion_stack.to_vec();
                                        new_macros_expansion_stack.push(
                                            MacroExpansionStackItem::Function(name.to_owned()),
                                        );

                                        let expanded_tokens = self.expand_replacement(
                                            ExpansionType::FunctionLikeDefinition,
                                            &new_macros_expansion_stack,
                                            &function_parameters,
                                            &arguments,
                                            function_definition,
                                        )?;

                                        output.extend(expanded_tokens);
                                    }
                                }
                                _ => {
                                    // The identifier is not a macro, just push it as is.
                                    output.push(current_token_with_location);
                                }
                            }
                        }
                    }
                }
                Token::Pound => {
                    // _Stringizing_ (the `#` operator).
                    // This operator is handled in the argument substitution step above.

                    return Err(PreprocessFileError {
                        file_number: current_token_with_location.location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Stringizing (#) operator can only appear in function-like macro definitions."
                                .to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }
                Token::PoundPound => {
                    // Token concatenation (the `##` operator)
                    // This operator is handled in the argument substitution step above.

                    return Err(PreprocessFileError {
                        file_number: current_token_with_location.location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Token concatenation (##) operator can only appear in function-like macro definitions."
                                .to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }
                _ => {
                    // The current token is not an identifier,
                    // we simply copy it to the output.
                    output.push(current_token_with_location);
                }
            }
        }

        Ok(output)
    }

    /// Process operators `__has_include`, `__has_embed` and `__has_c_attribute`,
    /// and the `true` and `false` keywords.
    ///
    /// Invoke this function only when expanding the conditional expression.
    fn process_conditional_expression_operators(
        &mut self,
        body: Vec<TokenWithLocation>,
    ) -> Result<Vec<TokenWithLocation>, PreprocessFileError> {
        if body.is_empty() {
            return Ok(vec![]);
        }

        let first_token_with_location = body.first().unwrap().clone();

        let mut output = Vec::new();
        let mut iter = body.into_iter();
        let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CODE_PARSE);
        let mut code_parser = CodeParser::new(
            &mut peekable_iter,
            &first_token_with_location.token,
            &first_token_with_location.location,
        );

        while let Some(current_token_with_location) = code_parser.next_token_with_location() {
            match &current_token_with_location.token {
                Token::Identifier(name) => {
                    match name.as_str() {
                        "true" => {
                            // `true` => 1
                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    1.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "false" => {
                            // `false` => 0
                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    0.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__has_include"
                            if code_parser.peek_token_and_equals(
                                0,
                                &Token::Punctuator(Punctuator::ParenthesisOpen),
                            ) =>
                        {
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

                            // Collect the tokens for `__has_include(...)` and resolve it later.
                            let mut components = vec![];

                            code_parser.consume_opening_paren()?; // Consumes '('

                            loop {
                                match code_parser.next_token_with_location() {
                                    Some(TokenWithLocation {
                                        token: Token::Punctuator(Punctuator::ParenthesisClose),
                                        ..
                                    }) => {
                                        // Reached the closing parenthesis
                                        break;
                                    }
                                    Some(token_with_location) => {
                                        components.push(token_with_location);
                                    }
                                    None => {
                                        // Unexpected end of tokens
                                        return Err(PreprocessFileError {
                                            file_number: current_token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "Unclosed parenthesis in `__has_include`."
                                                    .to_owned(),
                                                current_token_with_location.location.range,
                                            ),
                                        });
                                    }
                                }
                            }

                            if components.is_empty() {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "`__has_include` requires a file path argument.".to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            let include_resolve_result = self.resolve_include(components)?;

                            #[allow(clippy::collapsible_else_if)]
                            let result = if include_resolve_result.is_system_file {
                                if self
                                    .context
                                    .file_provider
                                    .resolve_system_file(&include_resolve_result.file_path)
                                    .is_some()
                                {
                                    1
                                } else {
                                    0
                                }
                            } else {
                                if self
                                    .context
                                    .file_provider
                                    .resolve_user_file_with_fallback(
                                        &include_resolve_result.file_path,
                                        &self
                                            .context
                                            .current_file_item
                                            .location
                                            .canonical_full_path,
                                        self.context.resolve_relative_path_within_current_file,
                                    )
                                    .is_some()
                                {
                                    1
                                } else {
                                    0
                                }
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    result.to_string(),
                                    false,
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__has_embed"
                            if code_parser.peek_token_and_equals(
                                0,
                                &Token::Punctuator(Punctuator::ParenthesisOpen),
                            ) =>
                        {
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

                            // Collect the tokens for `__has_embed(...)` and resolve it later.
                            let mut components = vec![];

                            code_parser.consume_opening_paren()?; // Consumes '('

                            let mut parenthesis_level = 1;

                            loop {
                                match code_parser.next_token_with_location() {
                                    Some(
                                        token_with_location @ TokenWithLocation {
                                            token: Token::Punctuator(Punctuator::ParenthesisOpen),
                                            ..
                                        },
                                    ) => {
                                        components.push(token_with_location);
                                        parenthesis_level += 1;
                                    }
                                    Some(
                                        token_with_location @ TokenWithLocation {
                                            token: Token::Punctuator(Punctuator::ParenthesisClose),
                                            ..
                                        },
                                    ) => {
                                        if parenthesis_level > 1 {
                                            components.push(token_with_location);
                                            parenthesis_level -= 1;
                                        } else {
                                            // Reached the closing parenthesis
                                            break;
                                        }
                                    }
                                    Some(token_with_location) => {
                                        components.push(token_with_location);
                                    }
                                    None => {
                                        // Unexpected end of tokens
                                        return Err(PreprocessFileError {
                                            file_number: current_token_with_location
                                                .location
                                                .file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "Unclosed parenthesis in `__has_include`."
                                                    .to_owned(),
                                                current_token_with_location.location.range,
                                            ),
                                        });
                                    }
                                }
                            }

                            if components.is_empty() {
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "`__has_embed` requires a file path argument.".to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }

                            let embed_resolve_result = self.resolve_embed(components)?;

                            // The possible return values are:
                            //
                            // - `__STDC_EMBED_FOUND__`
                            // - `__STDC_EMBED_EMPTY__`
                            // - `__STDC_EMBED_NOT_FOUND__`
                            //
                            // see:
                            // https://en.cppreference.com/w/c/preprocessor/embed.html

                            // Currently, we do not support checking for parameters like `prefix`, `suffix`,
                            // `limit`, and `if_empty`.
                            let result = if embed_resolve_result.is_system_file {
                                self.context
                                    .file_provider
                                    .resolve_system_file(&embed_resolve_result.file_path)
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
                                        &embed_resolve_result.file_path,
                                        &self
                                            .context
                                            .current_file_item
                                            .location
                                            .canonical_full_path,
                                        self.context.resolve_relative_path_within_current_file,
                                    )
                                    .map_or(
                                        0,
                                        |FilePathResolveResult {
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
                                    IntegerNumberWidth::Default,
                                ))),
                                Location::default(),
                            ));
                        }
                        "__has_c_attribute"
                            if code_parser.peek_token_and_equals(
                                0,
                                &Token::Punctuator(Punctuator::ParenthesisOpen),
                            ) =>
                        {
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

                            code_parser.consume_opening_paren()?; // Consumes '('

                            let attribute_name = match code_parser.next_token() {
                                Some(Token::Identifier(id)) => id.to_owned(),
                                Some(_) => {
                                    // let location = code_parser.peek_location(0).unwrap();
                                    return Err(PreprocessFileError {
                                        file_number: code_parser.last_location.file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "`__has_c_attribute` must be followed by an attribute."
                                                .to_owned(),
                                            code_parser.last_location.range,
                                        ),
                                    });
                                }
                                None => {
                                    return Err(PreprocessFileError {
                                        file_number: current_token_with_location
                                            .location
                                            .file_number,
                                        error: PreprocessError::MessageWithRange(
                                            "`__has_c_attribute` must be followed by an attribute."
                                                .to_owned(),
                                            current_token_with_location.location.range,
                                        ),
                                    });
                                }
                            };

                            code_parser.consume_closing_paren()?; // Consumes ')'

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
                                    // If the attribute is not recognized, expand to 0.
                                    0
                                }
                            };

                            output.push(TokenWithLocation::new(
                                Token::Number(Number::Integer(IntegerNumber::new(
                                    value.to_string(),
                                    false,
                                    IntegerNumberWidth::Long,
                                ))),
                                Location::default(),
                            ));
                        }
                        _ => {
                            // The identifier is not a special operator,
                            // we simply copy it to the output.
                            output.push(current_token_with_location);
                        }
                    }
                }
                _ => {
                    // The current token is not an identifier,
                    // we simply copy it to the output.
                    output.push(current_token_with_location);
                }
            }
        }

        Ok(output)
    }

    fn resolve_include(
        &mut self,
        components: Vec<TokenWithLocation>,
    ) -> Result<IncludeResolveResult, PreprocessFileError> {
        // The `components` are expanded tokens following `#include` directive or `__has_include` operator.
        //
        // There are three possible forms in the file path:
        // 1. One `Token::FilePath` which is a path enclosed in angle brackets `<...>`
        // 2. One `Token::FilePath` which is a path enclosed in double quotes `"..."`
        // 3. One `Token::String` which is a path enclosed in double quotes `"..."`
        // 4. A sequence of tokens beginning with '<' and ending with '>' and forming a valid file path.
        //
        // `components` must not be empty.

        let first_token_with_location = components.first().unwrap().clone();

        let mut iter = components.into_iter();
        let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CODE_PARSE);
        let mut code_parser = CodeParser::new(
            &mut peekable_iter,
            &first_token_with_location.token,
            &first_token_with_location.location,
        );

        let mut file_path = PathBuf::new();
        let mut is_system_file = false;

        while let Some(current_token_with_location) = code_parser.next_token_with_location() {
            match &current_token_with_location.token {
                Token::FilePath(path_name, angle_bracket) => {
                    // Case 1 and 2: A single `Token::FilePath`
                    is_system_file = *angle_bracket;
                    file_path = PathBuf::from(path_name);
                }
                Token::String(path_name, _) => {
                    // Case 3: A single `Token::String` (double-quoted string literal)
                    is_system_file = false;
                    file_path = PathBuf::from(path_name);
                }
                Token::Punctuator(Punctuator::LessThan) => {
                    // Case 4: A sequence of tokens forming a valid file path enclosed in `<...>`

                    let mut path_components = vec![];

                    loop {
                        match code_parser.next_token_with_location() {
                            Some(TokenWithLocation {
                                token: Token::Punctuator(Punctuator::GreaterThan),
                                ..
                            }) => {
                                // Reached the closing '>'
                                break;
                            }
                            Some(token_with_location) => {
                                path_components.push(token_with_location);
                            }
                            None => {
                                // Unexpected end of tokens
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "Unclosed angle bracket in include file path.".to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }
                        }
                    }

                    let path_string = stringize_tokens_without_spaces(&path_components)?;

                    is_system_file = true;
                    file_path = PathBuf::from(path_string);
                }
                _ => {
                    // Invalid token in file path
                    return Err(PreprocessFileError {
                        file_number: current_token_with_location.location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Invalid token in `#include` directive or `__has_include` operator."
                                .to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }
            }
        }

        Ok(IncludeResolveResult {
            file_path,
            file_path_location: first_token_with_location.location,
            is_system_file,
        })
    }

    fn resolve_embed(
        &mut self,
        components: Vec<TokenWithLocation>,
    ) -> Result<EmbedResolveResult, PreprocessFileError> {
        // The `components` are expanded tokens following `#embed` directive or `__has_embed` operator.
        //
        // There are three possible forms in the file path:
        // 1. One `Token::FilePath` which is a path enclosed in angle brackets `<...>`
        // 2. One `Token::FilePath` which is a path enclosed in double quotes `"..."`
        // 3. One `Token::String` which is a path enclosed in double quotes `"..."`
        // 4. A sequence of tokens beginning with '<' and ending with '>' and forming a valid file path.
        //
        // `components` must not be empty.

        let first_token_with_location = components.first().unwrap().clone();

        // Collect data within `prefix(...), suffix(...), limit(...), if_empty(...)`
        // The data is consisted of decimal or hexadecimal byte values separated by commas.
        // e.g.,
        // `prefix(0xCA, 0xFE, 0x03, 0x05)`

        let collect_data = |parser: &mut CodeParser| -> Result<Vec<u8>, PreprocessFileError> {
            parser.consume_opening_paren()?; // Consumes '('

            // Collect all data to `data` until we hit a closing parenthesis.
            let mut data = vec![];

            while let Some(current_token_with_location) = parser.next_token_with_location() {
                match &current_token_with_location.token {
                    Token::Punctuator(Punctuator::ParenthesisClose) => {
                        break;
                    }
                    Token::Char(ch, char_type) => {
                        if matches!(
                            char_type,
                            CharEncoding::Wide
                                | CharEncoding::UTF16
                                | CharEncoding::UTF32
                                | CharEncoding::UTF8
                        ) {
                            return Err(PreprocessFileError::new(
                                current_token_with_location.location.file_number,
                                PreprocessError::MessageWithRange(
                                    "Wide character literals are not supported in data list of `embed` directive."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            ));
                        }

                        let char_value = *ch as usize;
                        if char_value > 0xFF {
                            return Err(PreprocessFileError::new(
                                current_token_with_location.location.file_number,
                                PreprocessError::MessageWithRange(
                                    "Character literal value exceeds the maximum value of byte (0xFF)."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            ));
                        }

                        data.push(char_value as u8);
                    }
                    Token::Number(Number::Integer(n)) => {
                        let number_value = n.as_u64().map_err(|_| {
                            PreprocessFileError::new(
                                current_token_with_location.location.file_number,
                                PreprocessError::MessageWithRange(
                                    "Expect a integer number or character literal in data list of `embed` directive.".to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            )
                        })?;

                        if number_value > 0xFF {
                            return Err(PreprocessFileError::new(
                                current_token_with_location.location.file_number,
                                PreprocessError::MessageWithRange(
                                    "Integer number exceeds the maximum value of byte (0xFF)."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            ));
                        }

                        data.push(number_value as u8);
                    }
                    _ => {
                        return Err(PreprocessFileError::new(
                            current_token_with_location.location.file_number,
                            PreprocessError::MessageWithRange(
                                "Expect an integer number or character literal in data list of `embed` directive."
                                    .to_owned(),
                                current_token_with_location.location.range,
                            ),
                        ));
                    }
                }

                if !parser.peek_token_and_equals(0, &Token::Punctuator(Punctuator::Comma)) {
                    parser.consume_closing_paren()?; // Consumes ')'
                    break;
                }

                parser.next_token(); // Consumes ','

                // If we have a comma, we expect another character literal or integer number next.
                if parser.peek_token_and_equals(0, &Token::Punctuator(Punctuator::ParenthesisClose))
                {
                    return Err(
                        PreprocessFileError::new(
                            current_token_with_location.location.file_number,
                            PreprocessError::MessageWithPosition(
                                "Expect a character literal or integer number after comma in data list of `embed` directive."
                                    .to_owned(),
                                current_token_with_location.location.range.start
                            ),
                        )
                    );
                }
            }

            Ok(data)
        };

        let mut iter = components.into_iter();
        let mut peekable_iter = PeekableIter::new(&mut iter, PEEK_BUFFER_LENGTH_CODE_PARSE);
        let mut code_parser = CodeParser::new(
            &mut peekable_iter,
            &first_token_with_location.token,
            &first_token_with_location.location,
        );

        let mut file_path = PathBuf::new();
        let mut is_system_file = false;
        let mut limit: Option<usize> = None;
        let mut prefix: Option<Vec<u8>> = None;
        let mut suffix: Option<Vec<u8>> = None;
        let mut if_empty: Option<Vec<u8>> = None;

        while let Some(current_token_with_location) = code_parser.next_token_with_location() {
            match &current_token_with_location.token {
                Token::FilePath(path_name, angle_bracket) => {
                    // Case 1 and 2: A single `Token::FilePath`
                    is_system_file = *angle_bracket;
                    file_path = PathBuf::from(path_name);
                }
                Token::String(path_name, _) => {
                    // Case 3: A single `Token::String` (double-quoted string literal)
                    is_system_file = false;
                    file_path = PathBuf::from(path_name);
                }
                Token::Punctuator(Punctuator::LessThan) => {
                    // Case 4: A sequence of tokens forming a valid file path enclosed in `<...>`

                    let mut path_components = vec![];

                    loop {
                        match code_parser.next_token_with_location() {
                            Some(TokenWithLocation {
                                token: Token::Punctuator(Punctuator::GreaterThan),
                                ..
                            }) => {
                                // Reached the closing '>'
                                break;
                            }
                            Some(token_with_location) => {
                                path_components.push(token_with_location);
                            }
                            None => {
                                // Unexpected end of tokens
                                return Err(PreprocessFileError {
                                    file_number: current_token_with_location.location.file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "Unclosed angle bracket in file path.".to_owned(),
                                        current_token_with_location.location.range,
                                    ),
                                });
                            }
                        }
                    }

                    let path_string = stringize_tokens_without_spaces(&path_components)?;

                    is_system_file = true;
                    file_path = PathBuf::from(path_string);
                }
                Token::Identifier(param_name) => {
                    match param_name.as_str() {
                        "limit" => {
                            code_parser.consume_opening_paren()?; // Consumes '('

                            match code_parser.next_token_with_location() {
                                Some(TokenWithLocation {
                                    token: Token::Number(Number::Integer(number)),
                                    location,
                                }) => {
                                    let number_value = number.as_u64().map_err(|_| {
                                        PreprocessFileError::new(
                                            location.file_number,
                                            PreprocessError::MessageWithRange(
                                                "Invalid integer number.".to_owned(),
                                                location.range,
                                            ),
                                        )
                                    })?;

                                    limit = Some(number_value as usize);
                                }
                                Some(TokenWithLocation { location, .. }) => {
                                    return Err(PreprocessFileError::new(
                                        location.file_number,
                                        PreprocessError::MessageWithRange(
                                            "The value of embed parameter `limit` must be an integer."
                                                .to_owned(),
                                            location.range,
                                        ),
                                    ));
                                }
                                _ => {
                                    return Err(PreprocessFileError::new(
                                        current_token_with_location.location.file_number,
                                        PreprocessError::MessageWithRange(
                                            "Expect an integer number or character literal in `limit` parameter."
                                                .to_owned(),
                                            current_token_with_location.location.range,
                                        ),
                                    ));
                                }
                            }

                            code_parser.consume_closing_paren()?; // Consumes ')'
                        }
                        "prefix" => {
                            let data = collect_data(&mut code_parser)?;
                            prefix = Some(data);
                        }
                        "suffix" => {
                            let data = collect_data(&mut code_parser)?;
                            suffix = Some(data);
                        }
                        "if_empty" => {
                            let data = collect_data(&mut code_parser)?;
                            if_empty = Some(data);
                        }
                        "__limit__" | "__prefix__" | "__suffix__" | "__if_empty__" => {
                            let standard_param_name =
                                param_name.trim_start_matches("__").trim_end_matches("__");
                            return Err(PreprocessFileError::new(
                                current_token_with_location.location.file_number,
                                PreprocessError::MessageWithRange(
                                    format!(
                                        "Embed parameter `{}` is not supported, use `{}` instead.",
                                        param_name, standard_param_name
                                    ),
                                    current_token_with_location.location.range,
                                ),
                            ));
                        }
                        _ => {
                            // Invalid identifier/parameter in file path
                            return Err(PreprocessFileError {
                                file_number: current_token_with_location.location.file_number,
                                error: PreprocessError::MessageWithRange(
                                    "Invalid parameter in `#embed` directive or `__has_embed` operator."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            });
                        }
                    }
                }
                _ => {
                    // Invalid token in file path
                    return Err(PreprocessFileError {
                        file_number: current_token_with_location.location.file_number,
                        error: PreprocessError::MessageWithRange(
                            "Invalid token in `#embed` directive or `__has_embed` operator."
                                .to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }
            }
        }

        Ok(EmbedResolveResult {
            file_path,
            file_path_location: first_token_with_location.location,
            is_system_file,
            limit,
            prefix,
            suffix,
            if_empty,
        })
    }

    /// Check if the file has `#pragma once` or include guard.
    ///
    /// - If `#pragma once` is present, do nothing.
    /// - If neither is present, suggest adding `#pragma once`.
    /// - If `#pragma once` is not present but an include guard is found:
    ///   + check if the macro follows the naming convention.
    ///   + suggest adding additional `#pragma once`.
    ///
    fn check_pragma_once_and_include_guard(
        &mut self,
        file_number_of_header_file: usize,
        relative_path: &Path,
        program: &Program,
    ) {
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
            Statement::Pragma(Pragma { components}, _)
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
                        consequence,
                        ..
                    } if matches!(
                        consequence.first(),
                        Some(Statement::Define(Define::ObjectLike {
                            identifier: (name1, _),
                            definition,
                        }, _)) if name0 == name1 && definition.is_empty()
                    )
                )
            )
        {
            let Statement::If(If { branches, .. }) = first_statement else {
                unreachable!()
            };

            let branch = branches.first().unwrap();

            let Condition::NotDefined(name, range) = &branch.condition else {
                unreachable!()
            };

            // The file has an include guard.
            if name.ends_with(&suggested_include_guard_macro_name) {
                // The include guard macro name matches the suggested name.

                // Suggest using `#pragma once`:
                self.context.prompts.push(Prompt::Message(
                    PromptLevel::Info,
                    file_number_of_header_file,
                    "Consider adding additional `#pragma once` for better practice.".to_owned(),
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

    // Preprocess the program in the context of the included file.
    // This will handle all macros, directives, and other preprocessing tasks.
    fn process_included_header_file(
        &mut self,
        file_number: usize,

        is_system_file: bool,

        // The canonical full path of the source file.
        // This is used to load the source file content actually.
        header_file_canonical_full_path: &Path,

        // The path name of the source file.
        // It is used to generate the value of the `__FILE__` macro and the parser wouldn't use it to load the file.
        // Usually, it's value is the relative path to the project root directory.
        header_file_path_name: &Path,

        program: &Program,
    ) -> Result<(), PreprocessFileError> {
        // store the current context file as `last_context_file`
        let last_context_file_item = self.context.current_file_item.clone();

        // update the current context file to the included file
        let current_context_file_item = FileItem::new(
            file_number,
            FileLocation::new(
                if is_system_file {
                    FilePathSource::from_system_header_file(header_file_path_name)
                } else {
                    FilePathSource::from_user_header_file(header_file_path_name)
                },
                header_file_canonical_full_path,
            ),
        );
        self.context.current_file_item = current_context_file_item;

        // process the program
        self.process_program(program)?;

        // restore the `last_context_file`
        self.context.current_file_item = last_context_file_item;

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum ExpansionType {
    /// Normal code statements, inclusions
    Normal = 0,

    /// Conditional expression, i.e., `#if` and `#elif`.
    ConditionalExpression = 1,

    /// Object-like macro definition, where token concatenation is allowed.
    ObjectLikeDefinition = 2,

    /// Function-like macro definition, where stringizing is allowed.
    FunctionLikeDefinition = 3,
}

fn is_valid_identifier(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {
            for c in chars {
                if !(c.is_ascii_alphanumeric() || c == '_') {
                    return false;
                }
            }
            true
        }
        _ => false,
    }
}

/// Convert a token to a string.
fn stringize_token(token_with_location: &TokenWithLocation) -> Result<String, PreprocessFileError> {
    let s = match &token_with_location.token {
        Token::Identifier(_)
        | Token::Number(_)
        | Token::String(_, _)
        | Token::Char(_, _)
        | Token::Punctuator(_) => token_with_location.token.to_string(),
        _ => {
            return Err(PreprocessFileError {
                file_number: token_with_location.location.file_number,
                error: PreprocessError::MessageWithRange(
                    "Stringizing (#) can only be applied to macros, identifiers, numbers, chars, string and punctuators."
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
fn stringize_tokens(
    token_with_locations: &[TokenWithLocation],
) -> Result<String, PreprocessFileError> {
    let mut items = vec![];
    for token_with_location in token_with_locations {
        let token_string = stringize_token(token_with_location)?;
        items.push(token_string);
    }
    Ok(items.join(" "))
}

fn stringize_tokens_without_spaces(
    token_with_locations: &[TokenWithLocation],
) -> Result<String, PreprocessFileError> {
    let mut items = vec![];
    for token_with_location in token_with_locations {
        let token_string = stringize_token(token_with_location)?;
        items.push(token_string);
    }
    Ok(items.join(""))
}

/// Stringify a list of arguments.
/// Arguments are separated by commas.
fn stringize_arguments(args: &[ArgumentValue]) -> Result<String, PreprocessFileError> {
    let mut outputs = vec![];
    for arg in args {
        let token_string = stringize_tokens(&arg.origin)?;
        outputs.push(token_string);
    }
    Ok(outputs.join(", "))
}

#[derive(Debug, PartialEq, Clone)]
enum MacroExpansionStackItem {
    Object(String),
    Function(String),
}

fn contains_self_reference(
    macros_expansion_stack: &[MacroExpansionStackItem],
    macro_name: &str,
    is_invocation: bool,
) -> bool {
    for item in macros_expansion_stack {
        match item {
            MacroExpansionStackItem::Object(name) if name == macro_name && !is_invocation => {
                return true;
            }
            MacroExpansionStackItem::Function(name) if name == macro_name && is_invocation => {
                return true;
            }
            _ => {
                // Continue checking other items.
            }
        }
    }
    false
}

#[derive(Debug, PartialEq)]
struct ArgumentValue {
    // The original token value before expansion.
    // ANCPP only supports single token as argument value.
    origin: Vec<TokenWithLocation>,

    // The fully expanded tokens.
    // Only exists if the argument is a macro.
    expanded: Option<Vec<TokenWithLocation>>,
}

#[derive(Debug, PartialEq)]
enum Argument {
    // Poisition argument
    Position(ArgumentValue),

    // The value list of variadic arguments.
    Rest(Vec<ArgumentValue>),
}

struct IncludeResolveResult {
    file_path: PathBuf,
    file_path_location: Location,
    is_system_file: bool,
}

struct EmbedResolveResult {
    file_path: PathBuf,
    file_path_location: Location,

    // If true, the file path is enclosed in angle brackets `<...>`,
    // otherwise in double quotes `"..."`.
    is_system_file: bool,

    // Specifies the maximum number of bytes to output from the resource.
    // This limit does not include the prefix or suffix, and does not limit the numbers of bytes of `if_empty`.
    limit: Option<usize>,

    // Sequence to append to the output if the resource is not empty.
    suffix: Option<Vec<u8>>,

    // Sequence to prepend to the output if the resource is not empty.
    prefix: Option<Vec<u8>>,

    // Sequence to output if the resource is empty.
    if_empty: Option<Vec<u8>>,
}

struct CodeParser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
    // last_token_with_location: TokenWithLocation,
    last_token: Token,
    last_location: Location,
}

impl<'a> CodeParser<'a> {
    fn new(
        upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
        // first_token_with_location: &TokenWithLocation,
        last_token: &Token,
        last_location: &Location,
    ) -> Self {
        Self {
            upstream,
            last_token: last_token.clone(),
            last_location: *last_location,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithLocation { token, location }) => {
                self.last_token = token.clone();
                self.last_location = location;
                Some(token)
            }
            None => None,
        }
    }

    fn next_token_with_location(&mut self) -> Option<TokenWithLocation> {
        match self.upstream.next() {
            Some(token_with_location) => {
                self.last_token = token_with_location.token.clone();
                self.last_location = token_with_location.location;
                Some(token_with_location)
            }
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { token, .. }) => Some(token),
            None => None,
        }
    }

    // fn peek_token_with_location(&self, offset: usize) -> Option<&TokenWithLocation> {
    //     self.upstream.peek(offset)
    // }

    fn peek_location(&self, offset: usize) -> Option<&Location> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { location, .. }) => Some(location),
            None => None,
        }
    }

    fn peek_token_and_equals(&self, offset: usize, expected_token: &Token) -> bool {
        matches!(
            self.peek_token(offset),
            Some(token) if token == expected_token)
    }

    fn consume_token_and_expect(
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
                self.last_location.file_number,
                PreprocessError::MessageWithPosition(
                    format!("Expect token \"{}\" following here.", token_description),
                    self.last_location.range.end_included,
                ),
            )),
        }
    }

    // expects open parenthesis '(' and consumes it.
    fn consume_opening_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.consume_token_and_expect(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    // expects close parenthesis ')' and consumes it.
    fn consume_closing_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.consume_token_and_expect(
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
        FILE_NUMBER_SOURCE_FILE_BEGIN,
        context::{HeaderFileCache, Prompt, PromptLevel},
        error::{PreprocessError, PreprocessFileError},
        location::Location,
        memory_file_provider::MemoryFileProvider,
        position::Position,
        processor::{PreprocessResult, process_source_file},
        range::Range,
        token::{
            C23_KEYWORDS, IntegerNumber, IntegerNumberWidth, Number, Punctuator, Token,
            TokenWithLocation,
        },
    };

    /// Help function to process a single source file with given predefinitions.
    fn process_single_source_file(
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
            false,
            FILE_NUMBER_SOURCE_FILE_BEGIN,
            Path::new("src/main.c"),
            Path::new("/projects/hello/src/main.c"),
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

    /// Help function to process a single source file with given predefinitions.
    fn process_multiple_tokens_argument(
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
            true,
            FILE_NUMBER_SOURCE_FILE_BEGIN,
            Path::new("src/main.c"),
            Path::new("/projects/hello/src/main.c"),
        )
    }

    /// Help function to process a single source file and get the output tokens.
    fn process_multiple_tokens_argument_and_get_tokens(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Vec<TokenWithLocation> {
        process_multiple_tokens_argument(src, predefinitions)
            .unwrap()
            .output
    }

    /// Help function to process multiple source files with given header files.
    fn process_multiple_source_files(
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
            false,
            FILE_NUMBER_SOURCE_FILE_BEGIN,
            Path::new("src/main.c"),
            Path::new("/projects/hello/src/main.c"),
        )
    }

    /// Help function to process multiple source files and get the output tokens.
    fn process_multiple_source_files_and_get_tokens(
        main_src: &str,
        user_header_files: &[(&str, &str)],
        user_binary_files: &[(&str, &[u8])],
        system_header_files: &[(&str, &str)],
    ) -> Vec<TokenWithLocation> {
        process_multiple_source_files(
            main_src,
            user_header_files,
            user_binary_files,
            system_header_files,
        )
        .unwrap()
        .output
    }

    /// Help function to print tokens as a string.
    fn print_tokens(token_with_location: &[TokenWithLocation]) -> String {
        token_with_location
            .iter()
            .map(|TokenWithLocation { token, .. }| token.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn test_process_code_without_directive() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_file_and_get_tokens(
            "\
int main() {
    return 0;
}",
            &predefinitions,
        );

        let filenum = FILE_NUMBER_SOURCE_FILE_BEGIN;
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
                        IntegerNumberWidth::Default
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
    fn test_process_predefinitions() {
        let mut predefinitions = HashMap::new();
        predefinitions.insert("A".to_string(), "123".to_string());
        predefinitions.insert("B".to_string(), "3.14".to_string());
        predefinitions.insert("C".to_string(), "'🦛'".to_string());
        predefinitions.insert("D".to_string(), "\"✨ abc\"".to_string());
        predefinitions.insert("E".to_string(), "foo".to_string());
        predefinitions.insert("F".to_string(), "+".to_string());
        predefinitions.insert("G".to_string(), "".to_string()); // Empty

        let tokens = process_single_source_file_and_get_tokens(
            "\
A B C D E F G",
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "123 3.14 '🦛' \"✨ abc\" foo +");

        let mut predefinitions = HashMap::new();
        predefinitions.insert("A".to_string(), "'😆'".to_string());
        predefinitions.insert("B".to_string(), "A".to_string());
        predefinitions.insert("C".to_string(), "B".to_string());
        predefinitions.insert("M".to_string(), "M".to_string()); // Self-reference
        predefinitions.insert("X".to_string(), "Z".to_string()); // Indirect self-reference
        predefinitions.insert("Y".to_string(), "X".to_string());
        predefinitions.insert("Z".to_string(), "Y".to_string());

        let tokens = process_single_source_file_and_get_tokens(
            "\
A B C M X Y Z",
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "'😆' '😆' '😆' M X Y Z");
    }

    #[test]
    fn test_process_builtin_macro() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_file_and_get_tokens(
            "\
__FILE__;
__LINE__;
__DATE__;
__TIME__;
__STDC_EMBED_NOT_FOUND__;
__STDC_EMBED_FOUND__;
__STDC_EMBED_EMPTY__;
",
            &predefinitions,
        );

        let now = Local::now();
        let date_string = now.format("%b %e %Y").to_string();
        let time_string = now.format("%H:%M:%S").to_string();

        assert_eq!(
            print_tokens(&tokens),
            format!(
                r#""src/main.c" ; 2 ; "{}" ; "{}" ; 0 ; 1 ; 2 ;"#,
                date_string, time_string
            )
        );
    }

    #[test]
    fn test_process_define() {
        let mut predefinitions = HashMap::new();

        let tokens = process_single_source_file_and_get_tokens(
            "\
#define A 123
#define B 3.14
#define C '🦛'
#define D \"✨ abc\"
#define E foo
#define F +
#define G
A B C D E F G",
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "123 3.14 '🦛' \"✨ abc\" foo +");

        // Reference
        let tokens = process_single_source_file_and_get_tokens(
            "\
#define A '😆'
#define B A
#define C B
#define M M
#define X Z
#define Y X
#define Z Y
A B C M X Y Z",
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "'😆' '😆' '😆' M X Y Z");

        // Work with predefinitions
        predefinitions.insert("FOO".to_string(), "\"巴拉巴拉呣呣\"".to_string());
        let tokens = process_single_source_file_and_get_tokens(
            "\
#define BAR FOO
FOO 11 BAR",
            &predefinitions,
        );
        assert_eq!(
            print_tokens(&tokens),
            "\"巴拉巴拉呣呣\" 11 \"巴拉巴拉呣呣\""
        );

        // err: Redefine without undefine
        assert!(matches!(
            process_single_source_file(
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

        // err: Redefine with same definition
        assert!(matches!(
            process_single_source_file(
                "\
#define FOO 'a'
#define FOO 'a'",
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

        // err: Define reserved identifier: 'defined'
        assert!(matches!(
            process_single_source_file(
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

        // err: Define reserved identifier: the C keyword `return`
        assert!(matches!(
            process_single_source_file(
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
        let tokens = process_single_source_file_and_get_tokens(
            "\
#define A 123
#undef A
#define A 456
A",
            &HashMap::new(),
        );
        assert_eq!(print_tokens(&tokens), "456");

        // err: undefine non-existing macro
        assert!(matches!(
            process_single_source_file(
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
    }

    #[test]
    fn test_process_define_function() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_file_and_get_tokens(
            r#"
#define FOO(x) x
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

        // Macro invocation inside macro body
        let tokens = process_single_source_file_and_get_tokens(
            r#"
#define FOO(a) 1 a
#define BAR(x, y) 2 FOO(x) y
#define BUZZ(z) 3 BAR(z, spark)

BUZZ(hippo)"#,
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "3 2 1 hippo spark");

        // Self-reference macro invocation
        let tokens = process_single_source_file_and_get_tokens(
            r#"
#define FOO(x) BAR(x)
#define BAR(y) FOO(y)
#define BUZZ(z) BUZZ(z)

FOO(a) BAR(b) BUZZ(c)"#,
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "FOO ( a ) BAR ( b ) BUZZ ( c )");

        // err: redefine function-like macro
        assert!(matches!(
            process_single_source_file(
                "\
#define FOO(x) x
#define FOO(y) y",
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
            process_single_source_file(
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
            process_single_source_file(
                "\
#define FOO(x,y) x y
FOO(1, 2, 3)",
                &predefinitions,
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

        // err: Missing closing parenthesis
        assert!(matches!(
            process_single_source_file(
                "\
#define FOO(x) x
FOO(abc",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 17,
                            line: 1,
                            column: 0
                        },
                        end_included: Position {
                            index: 19,
                            line: 1,
                            column: 2
                        }
                    }
                )
            })
        ));

        // err: multiple tokens as argument
        assert!(matches!(
            process_single_source_file(
                "\
#define FOO(x) x
FOO(a b c)",
                &predefinitions
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 17,
                            line: 1,
                            column: 0
                        },
                        end_included: Position {
                            index: 19,
                            line: 1,
                            column: 2
                        }
                    }
                )
            })
        ));

        // err: define reserved identifier: 'defined'
        assert!(matches!(
            process_single_source_file(
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
    fn test_process_define_function_multiple_tokens_argument() {
        let predefinitions = HashMap::new();

        // Multiple tokens as argument
        let tokens = process_multiple_tokens_argument_and_get_tokens(
            r#"
#define FOO(x,y) x; y
FOO(1+2,abc)"#,
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "1 + 2 ; abc");

        // Commas inside argument
        let tokens = process_multiple_tokens_argument_and_get_tokens(
            r#"
#define FOO(x,y) x; y
FOO(2*(a+b,c-d), (x,y,z))"#,
            &predefinitions,
        );
        assert_eq!(
            print_tokens(&tokens),
            "2 * ( a + b , c - d ) ; ( x , y , z )"
        );

        // Macro invocation inside argument
        let tokens = process_multiple_tokens_argument_and_get_tokens(
            r#"
#define FOO(x) x
#define BAR(y) y
FOO(BAR(hello))"#,
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "hello");

        // Nested macro invocation inside argument
        let tokens = process_multiple_tokens_argument_and_get_tokens(
            r#"
#define FOO(x) (x+1)
FOO(FOO(hello))"#,
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "( ( hello + 1 ) + 1 )");

        // Self-reference macro invocation inside argument
        let tokens = process_multiple_tokens_argument_and_get_tokens(
            r#"
#define FOO(x) FOO(x+1)
FOO(hello)"#,
            &predefinitions,
        );
        assert_eq!(print_tokens(&tokens), "FOO ( hello + 1 )");
    }

    #[test]
    fn test_process_define_function_variadic() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_file_and_get_tokens(
            "\
// Outputs argument list as is
#define FOO(...) __VA_ARGS__

// Consumes the first two arguments and outputs the rest
#define BAR(x,y,...) x y __VA_ARGS__

// Outputs `int name[] = { values }`
#define BUZZ(x,...) int x[] __VA_OPT__(= { __VA_ARGS__ })

FOO();
FOO(1, 2, 3);
BAR(a, b);
BAR(c, d, e, f);
BUZZ(first);
BUZZ(last,11,13);",
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            "; 1 , 2 , 3 ; a b ; c d e , f ; int first [ ] ; int last [ ] = { 11 , 13 } ;"
        );

        // err: not enough arguments for variadic macro
        assert!(matches!(
            process_single_source_file(
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
        let tokens = process_single_source_file_and_get_tokens(
            r#"
#define FOO '🐘'
#define STR(x) #x
#define STR2(x) STR(x)
#define STR3(...) #__VA_ARGS__
STR(hello);
STR("world");
STR(FOO);
STR2(FOO);
STR3(42);
STR3(11, '🦛', "abc", id, FOO);"#,
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            r#""hello" ; "\"world\"" ; "FOO" ; "\'🐘\'" ; "42" ; "11, \'🦛\', \"abc\", id, FOO" ;"#
        );

        // Multiple tokens as argument for stringizing

        let tokens = process_multiple_tokens_argument_and_get_tokens(
            r#"
#define STR(...) #__VA_ARGS__
STR(a, b, c, d);
STR(a b, c d);"#,
            &predefinitions,
        );

        assert_eq!(print_tokens(&tokens), r#""a, b, c, d" ; "a b, c d" ;"#);

        // err: stringizing on non-function-like macro
        assert!(matches!(
            process_single_source_file(
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

        // err: stringizing on non-parameter
        assert!(matches!(
            process_single_source_file(
                "\
#define STR(x) #123
STR(foo)",
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
                            index: 18,
                            line: 0,
                            column: 18
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_process_token_concatenation() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_file_and_get_tokens(
            "\
#define FOO() foo##1
#define BAR() sprite ## 2 ## bar
#define ABC abc
#define BUZZ() ABC ## 3
#define CONCAT(a, b) a##b

FOO()
BAR()
BUZZ()
CONCAT(hello, world)
CONCAT(ABC, 42)
            ",
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            "foo1 sprite2bar ABC3 helloworld ABC42"
        );

        // err: invalid identifier after concatenation
        assert!(matches!(
            process_single_source_file(
                "\
#define CONCAT() 9 ## s
CONCAT()",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 17,
                            line: 0,
                            column: 17
                        },
                        end_included: Position {
                            index: 22,
                            line: 0,
                            column: 22
                        }
                    }
                )
            })
        ));

        // err: invalid identifier, through parameter concatenation
        assert!(matches!(
            process_single_source_file(
                "\
#define CONCAT(a, b) a##b
CONCAT(9, s)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 21,
                            line: 0,
                            column: 21
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

        // err: invalid identifier: concatenate an identifier and a string
        assert!(matches!(
            process_single_source_file(
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
                            index: 21,
                            line: 0,
                            column: 21
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

        // err: concatenate a multiple tokens argument
        assert!(matches!(
            process_multiple_tokens_argument(
                "\
#define CONCAT(a, b) a##b
CONCAT(hello world, 2)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 21,
                            line: 0,
                            column: 21
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

        // err: `##` is followed by `__VA_ARGS__`
        assert!(matches!(
            process_single_source_file(
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

        // err: `##` is followed by `__VA_OPT__`
        assert!(matches!(
            process_single_source_file(
                "\
#define CONCAT(...) ##__VA_OPT__
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

        // err: `##` is preceded by nothing
        assert!(matches!(
            process_single_source_file(
                "\
#define CONCAT(a, b) ##a
CONCAT(hello, world)",
                &predefinitions,
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 21,
                            line: 0,
                            column: 21
                        },
                        end_included: Position {
                            index: 22,
                            line: 0,
                            column: 22
                        }
                    }
                )
            })
        ));

        // err: `##` is followed by nothing
        assert!(matches!(
            process_single_source_file(
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
    fn test_process_if() {
        let predefinitions = HashMap::new();

        // Test `ifdef`
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#define FOO 'a'
#define BAR

#ifdef FOO
    foo_def
#else
    foo_undef
#endif

#ifdef BAR
    bar_def
#else
    bar_undef
#endif

#ifdef BAZ
    baz_def
#else
    baz_undef
#endif
"#,
                &predefinitions,
            )),
            "foo_def bar_def baz_undef"
        );

        // Test `ifndef`
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#define FOO 'a'
#define BAR

#ifndef FOO
    foo_undef
#else
    foo_def
#endif

#ifndef BAR
    bar_undef
#else
    bar_def
#endif

#ifndef BAZ
    baz_undef
#else
    baz_def
#endif
"#,
                &predefinitions,
            )),
            "foo_def bar_def baz_undef"
        );

        // Test `elifdef` and `elifndef`

        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#define FOO
#define BAR

#ifdef QUUX
    a
#elifdef BAZ
    b
#elifdef FOO
    c   // 👈
#elifdef BAR
    d
#else
    e
#endif

#ifndef FOO
    f
#elifndef BAR
    g
#elifndef QUUX
    h   // 👈
#elifndef BAZ
    i
#else
    j
#endif
"#,
                &predefinitions,
            )),
            "c h"
        );

        // Test `if` and operator `defined`
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#define FOO 'a'

#if defined(FOO)
    def_foo
#else
    undef_foo
#endif

#if !defined(FOO)
    undef_foo
#else
    def_foo
#endif

#if defined BAR
    def_bar
#else
    undef_bar
#endif

#if !defined BAR
    undef_bar
#else
    def_bar
#endif
"#,
                &predefinitions,
            )),
            "def_foo def_foo undef_bar undef_bar"
        );

        // Test 0, 1, -1, `true` and `false`
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#if 0
    a
#else
    b   // 👈
#endif

#if 1
    c   // 👈
#else
    d
#endif

#if -1
    e   // 👈
#else
    f
#endif

#if true
    g   // 👈
#else
    h
#endif

#if false
    i
#else
    j   // 👈
#endif
"#,
                &predefinitions,
            )),
            "b c e g j"
        );

        // Test `elif`
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#if false
    a
#elif false
    b
#elif true
    c   // 👈
#elif true
    d
#else
    e
#endif
"#,
                &predefinitions,
            )),
            "c"
        );

        // Test `if` and (integer) binary and unary operators
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#define FOO 3
#define BAR 5
#define BUZ 0

#if FOO
    a   // 👈
#else
    b
#endif

#if FOO == BAR
    c
#else
    d   // 👈
#endif

#if FOO != BAR
    e   // 👈
#else
    f
#endif

#if FOO < BAR
    g   // 👈
#else
    h
#endif

#if FOO > BAR
    i
#else
    j   // 👈
#endif

#if FOO + BAR == 8
    k   // 👈
#else
    l
#endif

#if FOO || BUZ
    m   // 👈
#else
    n
#endif

#if FOO && BUZ
    o
#else
    p   // 👈
#endif

// bitwise OR 0b0011 | 0b0101 = 0b0111
// note that the C bitwise operators have lower precedence than `==` operator
#if (FOO | BAR) == 7
    q   // 👈
#else
    r
#endif

// bitwise AND 0b0011 & 0b0101 = 0b0001
// note that the C bitwise operators have lower precedence than `==` operator
#if (FOO & BAR) == 1
    s   // 👈
#else
    t
#endif

#if !FOO
    u
#else
    v   // 👈
#endif
"#,
                &predefinitions,
            )),
            "a d e g j k m p q s v"
        );

        // Test operator precedence
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#if 1 + 2 * 3 == 7
    a   // 👈
#else
    b
#endif

#if (1 + 2) * 3 == 9
    c   // 👈
#else
    d
#endif

// logical OR has lower precedence than logical AND
// `false && true || true` -> `(false && true) || true`

#if false && true || true
    e   // 👈
#else
    f
#endif

#if false && (true || true)
    g
#else
    h   // 👈
#endif
"#,
                &predefinitions,
            )),
            "a c e h"
        );
    }

    #[test]
    fn test_process_include() {
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#include "foo.h"
FOO
"#,
            &[("src/header/foo.h", "#define FOO 42")],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens), "42");

        // include the same header file multiple times
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#include "foo.h"
#include "foo.h"
FOO
"#,
            &[("src/header/foo.h", "#define FOO 42")],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens), "42");

        // nested include
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#include "foo.h"
FOO BAR
"#,
            &[
                (
                    "src/header/foo.h",
                    r#"
#include "bar.h"
#define FOO 11
"#,
                ),
                ("src/header/bar.h", "#define BAR 13"),
            ],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens), "11 13");

        // include system header file
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#include <std/io.h>
FOO
"#,
            &[],
            &[],
            &[("std/io.h", "#define FOO 42")],
        );

        assert_eq!(print_tokens(&tokens), "42");

        // include system header file through quoted include
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#include "std/io.h"
FOO
"#,
            &[],
            &[],
            &[("std/io.h", "#define FOO 42")],
        );

        assert_eq!(print_tokens(&tokens), "42");

        // include header file with macro (with quoted string literal)
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#define HEADER_FILE "foo.h"
#include HEADER_FILE
FOO
"#,
            &[(
                "src/header/foo.h",
                r#"
#define FOO 42
"#,
            )],
            &[],
            &[],
        );

        assert_eq!(print_tokens(&tokens), "42");

        // include header file with macro (with angle bracket)
        let tokens = process_multiple_source_files_and_get_tokens(
            r#"
#define HEADER_FILE <std/io.h>
#include HEADER_FILE
FOO
"#,
            &[],
            &[],
            &[(
                "std/io.h",
                r#"
#define FOO 42
"#,
            )],
        );

        assert_eq!(print_tokens(&tokens), "42");

        // err: include non-existing file
        assert!(matches!(
            process_multiple_source_files(
                r#"
#include "non_existing.h"
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
                            index: 10,
                            line: 1,
                            column: 9
                        },
                        end_included: Position {
                            index: 25,
                            line: 1,
                            column: 24
                        }
                    }
                )
            })
        ));

        // err: include with identifier which expands to non-existing file
        assert!(matches!(
            process_multiple_source_files(
                r#"
#define HEADER_FILE "non_existing.h"
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
                            index: 21,
                            line: 1,
                            column: 20
                        },
                        end_included: Position {
                            index: 36,
                            line: 1,
                            column: 35
                        }
                    }
                )
            })
        ));

        // err: include with identifier which expands to empty
        assert!(matches!(
            process_multiple_source_files(
                r#"
#define HEADER_FILE
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
                            index: 22,
                            line: 2,
                            column: 1
                        },
                        end_included: Position {
                            index: 28,
                            line: 2,
                            column: 7
                        }
                    }
                )
            })
        ));

        // err: include with a number literal
        assert!(matches!(
            process_multiple_source_files(
                r#"#include 123"#,
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
            if range == Range::from_detail(9, 0, 9, 3)
        ));

        let a = process_multiple_source_files(r#"#include "foo.h" bar"#, &[], &[], &[]);
        println!("{:?}", a);

        // err: include with identifier which extraneous tokens
        assert!(matches!(
            process_multiple_source_files(r#"#include "foo.h" bar"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 17,
                            line: 0,
                            column: 17
                        },
                        end_included: Position {
                            index: 19,
                            line: 0,
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
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#embed "foo.bin"
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 , 0x04 , 0x05"
        );

        // Test `limit` option
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#embed "foo.bin" limit(100)
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 , 0x04 , 0x05"
        );

        // Test `prefix` and `suffix` options
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#embed "foo.bin" limit(3) prefix(0xaa, 11, 'a') suffix(0)
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0xaa , 0x0b , 0x61 , 0x01 , 0x02 , 0x03 , 0x00"
        );

        // Test `if_empty` option
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#embed "foo.bin" limit(3) if_empty(0x11,0x13,0x17,0x19) prefix(0x3,0x5,0x7) suffix(0x23, 0x29)
"#,
                &[],
                &[("src/resources/foo.bin", &[])],
                &[],
            )),
            "0x11 , 0x13 , 0x17 , 0x19"
        );

        // load binary multiple times
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#embed "foo.bin"
;
#embed "foo.bin"
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 ; 0x01 , 0x02 , 0x03"
        );

        // Load binary with macro file name
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#define FOO "foo.bin"
#embed FOO
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03 , 0x04 , 0x05"
        );

        // Load binary with macro file name and options
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#define FOO "foo.bin"
#embed FOO limit(3)
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03"
        );

        // Load binary with macro that expand to file name and options
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#define FOO "foo.bin" limit(3)
#embed FOO
"#,
                &[],
                &[("src/resources/foo.bin", &[1, 2, 3, 4, 5])],
                &[],
            )),
            "0x01 , 0x02 , 0x03"
        );

        // err: include non-existing file
        assert!(matches!(
            process_multiple_source_files(r#"#embed "non_existing.bin""#, &[], &[], &[],),
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

        // err: invalid parameter value
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" limit(abc)"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _, // "The value of embed parameter `limit` must be an integer."
                    Range {
                        start: Position {
                            index: 23,
                            line: 0,
                            column: 23,
                        },
                        end_included: Position {
                            index: 25,
                            line: 0,
                            column: 25,
                        },
                    },
                ),
            },)
        ));

        // err: missing opening parenthesis in parameter value
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" limit 100"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithPosition(
                    _, // "Expect token: opening parenthesis.",
                    Position {
                        index: 23,
                        line: 0,
                        column: 23,
                    },
                ),
            },)
        ));

        // err: missing closing parenthesis in parameter value
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" limit(100"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithPosition(
                    _, // "Expect token \"closing parenthesis\" following here."
                    Position {
                        index: 25,
                        line: 0,
                        column: 25,
                    },
                ),
            },)
        ));

        // err: unsupported data type in data list
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" prefix(11, 'a', c)"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _, // "Expect an integer number or character literal in data list of `embed` directive."
                    Range {
                        start: Position {
                            index: 33,
                            line: 0,
                            column: 33,
                        },
                        end_included: Position {
                            index: 33,
                            line: 0,
                            column: 33,
                        },
                    },
                ),
            },)
        ));

        // err: number value exceeds byte range
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" prefix(42, 256)"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _, // "Integer number exceeds the maximum value of byte (0xFF)."
                    Range {
                        start: Position {
                            index: 28,
                            line: 0,
                            column: 28,
                        },
                        end_included: Position {
                            index: 30,
                            line: 0,
                            column: 30,
                        },
                    },
                ),
            },)
        ));

        // err: char value exceeds byte range
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" prefix('光', 'a')"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _, // "Character literal value exceeds the maximum value of byte (0xFF)."
                    Range {
                        start: Position {
                            index: 24,
                            line: 0,
                            column: 24,
                        },
                        end_included: Position {
                            index: 26,
                            line: 0,
                            column: 26,
                        },
                    },
                ),
            },)
        ));

        // err: missing commas in the data list
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" prefix(11 13)"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithPosition(
                    _, // "Expect token: closing parenthesis."
                    Position {
                        index: 27,
                        line: 0,
                        column: 27,
                    },
                ),
            },)
        ));

        // err: unsupported parameter name
        assert!(matches!(
            process_multiple_source_files(r#"#embed "foo.bin" offset(10)"#, &[], &[], &[],),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _, // "Invalid parameter in `#embed` directive or `__has_embed` operator."
                    Range {
                        start: Position {
                            index: 17,
                            line: 0,
                            column: 17,
                        },
                        end_included: Position {
                            index: 22,
                            line: 0,
                            column: 22,
                        },
                    },
                ),
            },)
        ));
    }

    #[test]
    fn test_process_include_guard_check() {
        // Empty header file
        assert!(matches!(
            process_multiple_source_files(
                r#"
#include "foo.h"
"#,
                &[("src/header/foo.h", "",)],
                &[],
                &[],
            )
            .unwrap()
            .prompts
            .first(),
            Some(Prompt::Message(PromptLevel::Warning, 1, _))
        ));

        // neither include guard nor `#pragma once` is present
        assert!(matches!(
            process_multiple_source_files(
                r#"
#include "foo.h"
"#,
                &[(
                    "src/header/foo.h",
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

        // `#pragma once` is present, no info or warning
        assert!(
            process_multiple_source_files(
                r#"
#include "foo.h"
"#,
                &[(
                    "src/header/foo.h",
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

        // include guard is present, suggest adding additional `#pragma once`
        assert!(matches!(
            process_multiple_source_files(
                r#"
#include "foo.h"
"#,
                &[(
                    "src/header/foo.h",
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

        // include guard is present, the macro name follows the convention,
        // though it is not the suggested one.
        assert!(matches!(
            process_multiple_source_files(
                r#"
#include "foo.h"
"#,
                &[(
                    "src/header/foo.h",
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

        // include guard is present, but the macro name does not follow the suggested convention
        assert!(matches!(
            process_multiple_source_files(
                r#"
#include "foo.h"
"#,
                &[(
                    "src/header/foo.h",
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
    fn test_process_operator_has_include() {
        // Test `#has_include`
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#define HEADER_FILE "foo.h"

#if __has_include("foo.h")
    a   // 👈
#else
    b
#endif

#if __has_include("bar.h")
    c
#else
    d   // 👈
#endif

#if __has_include(HEADER_FILE)
    e   // 👈
#else
    f
#endif
"#,
                &[("src/header/foo.h", "123")],
                &[],
                &[],
            )),
            "a d e"
        );
    }

    #[test]
    fn test_process_operator_has_embed() {
        // Test `#has_embed`
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
                r#"
#define FOO_BIN "foo.bin"
#define BAR_BIN "bar.bin" limit(10)
#define BAZ_BIN "baz.bin"

#if __has_embed("foo.bin")
    a   // 👈
#else
    b
#endif

#if __has_embed("foo.bin" limit(10) prefix(1,2,3))
    c   // 👈
#else
    d
#endif

#if __has_embed("bar.bin")
    e   // 👈
#else
    f
#endif

#if __has_embed("baz.bin")
    g
#else
    h   // 👈
#endif

#if __has_embed(FOO_BIN)
    i   // 👈
#else
    j
#endif

#if __has_embed(FOO_BIN limit(10) suffix(0x0))
    k   // 👈
#else
    l
#endif

#if __has_embed(BAR_BIN)
    m   // 👈
#else
    n
#endif

#if __has_embed(BAZ_BIN)
    o
#else
    p   // 👈
#endif

#if __has_embed("foo.bin") == __STDC_EMBED_FOUND__
    foo_found
#elif __has_embed("foo.bin") == __STDC_EMBED_EMPTY__
    foo_empty
#elif __has_embed("foo.bin") == __STDC_EMBED_NOT_FOUND__
    foo_not_found
#else
    foo_unreachable
#endif

#if __has_embed("bar.bin") == __STDC_EMBED_FOUND__
    bar_found
#elif __has_embed("bar.bin") == __STDC_EMBED_EMPTY__
    bar_empty
#elif __has_embed("bar.bin") == __STDC_EMBED_NOT_FOUND__
    bar_not_found
#else
    bar_unreachable
#endif

#if __has_embed("baz.bin") == __STDC_EMBED_FOUND__
    baz_found
#elif __has_embed("baz.bin") == __STDC_EMBED_EMPTY__
    baz_empty
#elif __has_embed("baz.bin") == __STDC_EMBED_NOT_FOUND__
    baz_not_found
#else
    baz_unreachable
#endif
"#,
                &[],
                &[
                    ("src/resources/foo.bin", &[1, 2, 3]),
                    ("src/resources/bar.bin", &[])
                ],
                &[],
            )),
            "a c e h i k m p foo_found bar_empty baz_not_found"
        );
    }

    #[test]
    fn test_process_operator_has_c_attribute() {
        // Test `#has_c_attribute`
        assert_eq!(
            print_tokens(&process_multiple_source_files_and_get_tokens(
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
            print_tokens(&process_multiple_source_files_and_get_tokens(
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

        assert!(matches!(
            process_multiple_source_files(
                r#"
#if __has_c_attribute deprecated
    // error: missing parentheses
#endif
                "#,
                &[],
                &[],
                &[],
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _, // "Expect a macro, an integer number, or an operator."
                    Range {
                        start: Position {
                            index: 5,
                            line: 1,
                            column: 4,
                        },
                        end_included: Position {
                            index: 21,
                            line: 1,
                            column: 20,
                        },
                    },
                ),
            },)
        ));
    }

    #[test]
    fn test_process_error_directive() {
        let predefinitions = HashMap::new();

        assert!(matches!(
            process_single_source_file(r#"#error "foobar""#, &predefinitions,),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 1,
                            line: 0,
                            column: 1
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
            process_single_source_file(r#"#warning "foobar""#, &predefinitions,)
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
    fn test_process_pragma() {
        let predefinitions = HashMap::new();

        // pragmas are ignored currently, so the output should be empty.
        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                "\
#pragma STDC FENV_ACCESS ON
#pragma STDC FP_CONTRACT OFF
#pragma STDC CX_LIMITED_RANGE DEFAULT
#define WORLD __Pragma(\"GCC dependency \\\"foo.h\\\"\") world
hello
WORLD
",
                &predefinitions,
            )),
            "hello world"
        );
    }

    #[test]
    fn test_process_adjacent_string_literals() {
        let predefinitions = HashMap::new();

        assert_eq!(
            print_tokens(&process_single_source_file_and_get_tokens(
                r#"
#define STR1 "Hello"
#define STR2 "World"
#define STR3 STR1 ", " STR2 "!"

"foo" "bar"
"buz";

STR1
STR2
2026
STR3
"#,
                &predefinitions,
            )),
            r#""foobarbuz" ; "HelloWorld" 2026 "Hello, World!""#
        );

        assert_eq!(
            print_tokens(&process_multiple_tokens_argument_and_get_tokens(
                r#"
#define FOO(x) x

// adjacent strings in argument

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
            process_single_source_file(r#""abc" u8"xyz""#, &predefinitions),
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
