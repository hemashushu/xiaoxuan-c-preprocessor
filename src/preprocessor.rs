// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{collections::HashMap, path::Path};

use chrono::Local;

use crate::{
    PreprocessError, PreprocessFileError, TokenWithLocation, TokenWithRange,
    ast::{Define, Pragma, Program, Statement},
    context::Context,
    definition,
    file_cache::FileCache,
    file_provider::FileProvider,
    location::Location,
    parser::parse_from_str,
    peekable_iter::PeekableIter,
    preprocessor_parser::PreprocessorParser,
    prompt::{Prompt, PromptLevel},
    range::Range,
    token::{IntegerNumber, IntegerNumberType, Number, Punctuator, StringType, Token},
};

pub const PEEK_BUFFER_LENGTH_PREPROCESS: usize = 4;
pub const PEEK_BUFFER_LENGTH_MERGE_STRINGS: usize = 2;

/// Preprocesses C source files.
///
/// Arguments:
/// - `file_number`: The file number for the source file, used for error reporting.
/// - `file_canonical_path`: The C source files to preprocess.
/// - `resolve_relative_file`: If true, also search for headers in the directory where the source file resides.
///   For example, if the source file is `src/foo/bar.c` and this flag is set,
///   then `#include "header.h"` will look in `src/foo/`,
///   and `#include "../hello/world.h"` will look in `src/hello`.
///   otherwise, it will only search in the specified `user_headers_directories`.
/// - `predefinitions`: Predefined macros to be used during preprocessing.
///   There are many predefined macros in C, such as `__STDC_VERSION__` etc.
///   This map should only contain static predefinitions, dynamic macros (such as `__FILE__`)
///   will be handled by the preprocessor.
///   see:
///   - https://gcc.gnu.org/onlinedocs/cpp/Standard-Predefined-Macros.html
///   - https://en.cppreference.com/w/c/preprocessor/replace.html
/// - `file_provider`: A trait object that provides access to files.
/// - `file_cache`: A mutable reference to the file cache, used to avoid redundant parsing of files.
///
/// see also:
/// - https://en.cppreference.com/w/c/language.html
/// - https://en.cppreference.com/w/c/preprocessor.html
pub fn preprocess_source_file<T>(
    file_number: usize,
    file_canonical_path: &Path,
    resolve_relative_file: bool,
    predefinitions: &HashMap<String, String>,
    file_provider: &T,
    file_cache: &mut FileCache,
) -> Result<PreprocessResult, PreprocessFileError>
where
    T: FileProvider,
{
    let src = file_provider
        .load_file(file_canonical_path)
        .map_err(|error| {
            PreprocessFileError::new(
                file_number,
                PreprocessError::Message(format!(
                    "Failed to load file '{}': {}",
                    file_canonical_path.to_string_lossy(),
                    error
                )),
            )
        })?;

    let program =
        parse_from_str(&src).map_err(|error| PreprocessFileError::new(file_number, error))?;

    let mut context = Context::from_keyvalues(
        file_number,
        file_canonical_path,
        file_provider,
        file_cache,
        predefinitions,
    )
    .map_err(|error| PreprocessFileError { file_number, error })?;

    preprocess_program(&mut context, &program)?;

    let Context {
        prompts, output, ..
    } = context;

    let result = PreprocessResult { output, prompts };

    Ok(result)
}

#[derive(Debug, PartialEq)]
pub struct PreprocessResult {
    pub output: Vec<TokenWithLocation>,
    pub prompts: Vec<Prompt>,
}

fn preprocess_program<T>(
    context: &mut Context<T>,
    program: &Program,
) -> Result<(), PreprocessFileError>
where
    T: FileProvider,
{
    for statement in &program.statements {
        match statement {
            Statement::Pragma(pragma) => preprocess_pragma(context, pragma)?,
            Statement::Define(define) => preprocess_define(context, define)?,
            Statement::Undef(identifier, range) => preprocess_undefine(context, identifier, range)?,
            Statement::Include(include) => todo!(),
            Statement::Embed(embed) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Error(message, range) => preprocess_error(context, message, range)?,
            Statement::Warning(message, range) => preprocess_warnning(context, message, range)?,
            Statement::Code(token_with_ranges) => preprocess_code(context, token_with_ranges)?,
        }
    }

    Ok(())
}

fn preprocess_pragma<T>(
    context: &mut Context<T>,
    pragma: &Pragma,
) -> Result<(), PreprocessFileError>
where
    T: FileProvider,
{
    // Supported pragmas are:
    // - `#pragma once`: Include guard to prevent multiple inclusions of the same file.
    // - `#pragma STDC FENV_ACCESS arg`: Where arg is either ON or OFF or DEFAULT
    // - `#pragma STDC FP_CONTRACT arg`
    // - `#pragma STDC CX_LIMITED_RANGE arg`

    let parse_pragma_argument_value = |s: &str| -> StandardPragmaValue {
        match s.to_uppercase().as_str() {
            "DEFAULT" => StandardPragmaValue::Default,
            "ON" => StandardPragmaValue::On,
            "OFF" => StandardPragmaValue::Off,
            _ => unreachable!(),
        }
    };

    let mut iter = pragma.parts.iter();

    match iter.next().unwrap() {
        TokenWithRange {
            token: Token::Identifier(name),
            range: name_range,
        } => {
            // Handle pragmas based on their identifier.
            match name.as_str() {
                "once" => {
                    // Handle `#pragma once` directive.
                    // do nothing since ANCC always includes files only once.
                    Ok(())
                }
                "STDC" => {
                    // Handle standard pragmas.
                    // Only `FENV_ACCESS`, `FP_CONTRACT`, and `CX_LIMITED_RANGE` are supported.
                    match iter.next() {
                        Some(TokenWithRange {
                            token: Token::Identifier(arg),
                            range: arg_range,
                        }) if arg == "FENV_ACCESS"
                            || arg == "FP_CONTRACT"
                            || arg == "CX_LIMITED_RANGE" =>
                        {
                            // Handle arguments for the pragma.
                            // The arg can be ON, OFF, or DEFAULT.
                            match iter.next() {
                                Some(TokenWithRange {
                                    token: Token::Identifier(value),
                                    range: _,
                                }) if value == "ON" || value == "OFF" || value == "DEFAULT" => {
                                    match iter.next() {
                                        Some(TokenWithRange {
                                            token: _,
                                            range: extraneous_range,
                                        }) => Err(PreprocessFileError {
                                            file_number: context.current_file_number,
                                            error: PreprocessError::MessageWithRange(
                                                "Extraneous parameter.".to_owned(),
                                                *extraneous_range,
                                            ),
                                        }),
                                        None => {
                                            let value = parse_pragma_argument_value(value);
                                            let _standard_pragma = match arg.to_uppercase().as_str()
                                            {
                                                "FENV_ACCESS" => StandardPragma::FenvAccess(value),
                                                "FP_CONTRACT" => StandardPragma::FPContract(value),
                                                "CX_LIMITED_RANGE" => {
                                                    StandardPragma::CxLimitedRange(value)
                                                }
                                                _ => unreachable!(),
                                            };

                                            // todo::
                                            // Standard pragmas should be passed to the compiler for processing.
                                            // For example, standard pragmas could be inserted into the output token stream
                                            // as a new token type or as C attributes.
                                            // Currently, they are simply ignored.
                                            Ok(())
                                        }
                                    }
                                }
                                _ => Err(PreprocessFileError {
                                    file_number: context.current_file_number,
                                    error: PreprocessError::MessageWithRange(
                                        format!("Expected ON, OFF, or DEFAULT after {}", arg),
                                        *arg_range,
                                    ),
                                }),
                            }
                        }
                        _ => Err(PreprocessFileError {
                            file_number: context.current_file_number,
                            error: PreprocessError::MessageWithRange(
                                format!(
                                    "Expected FENV_ACCESS, FP_CONTRACT, or CX_LIMITED_RANGE after STDC"
                                ),
                                *name_range,
                            ),
                        }),
                    }
                }
                _ => Err(PreprocessFileError {
                    file_number: context.current_file_number,
                    error: PreprocessError::MessageWithRange(
                        format!("Unsupported pragma: {}", name),
                        *name_range,
                    ),
                }),
            }
        }
        TokenWithRange {
            token: _,
            range: invalid_range,
        } => Err(PreprocessFileError {
            file_number: context.current_file_number,
            error: PreprocessError::MessageWithRange(
                "Directive `#pragma` should be followed by an identifier".to_string(),
                *invalid_range,
            ),
        }),
    }
}

fn preprocess_code<T>(
    context: &mut Context<T>,
    token_with_ranges: &[TokenWithRange],
) -> Result<(), PreprocessFileError>
where
    T: FileProvider,
{
    let tokens = token_with_ranges
        .iter()
        .map(|token_with_range| {
            TokenWithLocation::new(
                token_with_range.token.clone(),
                Location::new(context.current_file_number, &token_with_range.range),
            )
        })
        .collect::<Vec<_>>();

    // No arguments for code block because it is not a (function-like) macro.
    let argument_map = HashMap::<String, ArgumentValue>::new();
    let expanded_tokens = expand_marco(context, tokens, &argument_map, ExpandContextType::Normal)?;
    context.output.extend(expanded_tokens);
    Ok(())
}

fn preprocess_define<T>(
    context: &mut Context<T>,
    define: &Define,
) -> Result<(), PreprocessFileError>
where
    T: FileProvider,
{
    match define {
        Define::ObjectLike {
            identifier: (name, range),
            definition,
        } => {
            if name == "defined" {
                return Err(PreprocessFileError {
                    file_number: context.current_file_number,
                    error: PreprocessError::MessageWithRange(
                        "The identifier 'defined' cannot be used as a macro name.".to_string(),
                        *range,
                    ),
                });
            }

            let result = context
                .definition
                .add_object_like(context.current_file_number, name, definition)
                .map_err(|error| PreprocessFileError {
                    file_number: context.current_file_number,
                    error,
                })?;

            if !result {
                return Err(PreprocessFileError {
                    file_number: context.current_file_number,
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
                    file_number: context.current_file_number,
                    error: PreprocessError::MessageWithRange(
                        "The identifier 'defined' cannot be used as a macro name.".to_string(),
                        *range,
                    ),
                });
            }

            let result = context
                .definition
                .add_function_like(context.current_file_number, name, parameters, definition)
                .map_err(|error| PreprocessFileError {
                    file_number: context.current_file_number,
                    error,
                })?;

            if !result {
                return Err(PreprocessFileError {
                    file_number: context.current_file_number,
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

fn preprocess_undefine<T>(
    context: &mut Context<T>,
    identifier: &str,
    range: &Range,
) -> Result<(), PreprocessFileError>
where
    T: FileProvider,
{
    if identifier == "defined" {
        return Err(PreprocessFileError {
            file_number: context.current_file_number,
            error: PreprocessError::MessageWithRange(
                "The identifier 'defined' cannot be used as a macro name.".to_string(),
                *range,
            ),
        });
    }

    let result = context.definition.remove(identifier);
    if !result {
        return Err(PreprocessFileError {
            file_number: context.current_file_number,
            error: PreprocessError::MessageWithRange(
                format!("Macro '{}' is not defined.", identifier),
                *range,
            ),
        });
    }

    Ok(())
}

fn preprocess_error(
    context: &mut Context<impl FileProvider>,
    message: &str,
    range: &Range,
) -> Result<(), PreprocessFileError> {
    Err(PreprocessFileError {
        file_number: context.current_file_number,
        error: PreprocessError::MessageWithRange(
            format!("User defined error: {}", message),
            *range,
        ),
    })
}

fn preprocess_warnning(
    context: &mut Context<impl FileProvider>,
    message: &str,
    range: &Range,
) -> Result<(), PreprocessFileError> {
    let prompt = Prompt::MessageWithRange(
        PromptLevel::Warning,
        context.current_file_number,
        format!("User defined warning: {}", message),
        *range,
    );

    context.prompts.push(prompt);
    Ok(())
}

fn expand_marco<T>(
    context: &mut Context<T>,

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
) -> Result<Vec<TokenWithLocation>, PreprocessFileError>
where
    T: FileProvider,
{
    let mut output = Vec::new();

    let mut iter = token_with_locations.into_iter();
    let mut peekable_iter = PeekableIter::new(&mut iter, 2);
    let mut parser = PreprocessorParser::new(&mut peekable_iter, context.current_file_number);

    while let Some(current_token_with_location) = parser.next_token_with_location() {
        match &current_token_with_location.token {
            Token::Identifier(name) => {
                match name.as_str() {
                    "__VA_ARGS__" => {
                        // `__VA_ARGS__` is a special macro that represents the variable arguments
                        // in function-like macros.
                        // see:
                        // https://en.cppreference.com/w/c/preprocessor/replace.html

                        if expand_context_type != ExpandContextType::VariadicFunctionLikeDefinition
                        {
                            return Err(PreprocessFileError {
                                file_number: context.current_file_number,
                                error: PreprocessError::MessageWithRange(
                                    "__VA_ARGS__ can only be used in variadic function-like macros."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            });
                        }

                        if let Some(ArgumentValue::Concatenated(tokens)) = argument_map.get("...") {
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

                        if expand_context_type != ExpandContextType::VariadicFunctionLikeDefinition
                        {
                            return Err(PreprocessFileError {
                                file_number: context.current_file_number,
                                error: PreprocessError::MessageWithRange(
                                    "__VA_OPT__ can only be used in variadic function-like macros."
                                        .to_owned(),
                                    current_token_with_location.location.range,
                                ),
                            });
                        }

                        // collect content
                        let mut content = Vec::new();
                        parser.expect_and_consume_opening_paren()?; // Consumes '('

                        while let Some(next_token_with_location) = parser.next_token_with_location()
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
                                expand_marco(context, content, argument_map, expand_context_type)?;
                            output.extend(expanded_content);
                        }
                    }
                    "__FILE__" => {
                        // expands to the name of the current file, as a character string literal
                        let value = context
                            .current_file_path
                            .file_name()
                            .unwrap()
                            .to_string_lossy()
                            .to_string();
                        let token = TokenWithLocation::new(
                            Token::String(value, StringType::Default),
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
                                IntegerNumberType::Default,
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
                            Token::String(date_string, StringType::Default),
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
                            Token::String(time_string, StringType::Default),
                            Location::default(),
                        );
                        output.push(token);
                    }
                    _ if parser.peek_token_and_equals(0, &Token::PoundPound) => {
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
                            return Err(PreprocessFileError {
                                file_number: context.current_file_number,
                                error: PreprocessError::MessageWithRange(
                                    "Token concatenation (##) can only be used in macro definitions."
                                        .to_owned(),
                                    parser.peek_location(0).unwrap().range,
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
                                                        location: _,
                                                    } => {
                                                        // The argument value is an identifier
                                                        value.to_owned()
                                                    }
                                                    TokenWithLocation {
                                                        token:
                                                            Token::Number(Number::Integer(
                                                                IntegerNumber { value, .. },
                                                            )),
                                                        location: _,
                                                    } => {
                                                        // The argument value is an integer number
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

                        parser.next_token(); // consumes the `##` token

                        loop {
                            if let Some(next_token_with_location) =
                                parser.next_token_with_location()
                            {
                                concatenated_name.push(get_valid_identifier_part_from_token(
                                    &next_token_with_location,
                                    false,
                                )?);
                                concatenated_location.range.end_included =
                                    next_token_with_location.location.range.end_included;

                                if parser.peek_token_and_equals(0, &Token::PoundPound) {
                                    // If the next token is another `##`, we continue concatenating.
                                    parser.next_token(); // consumes the `##` token
                                } else {
                                    // Otherwise, we have reached the end of the concatenation.
                                    break;
                                }
                            } else {
                                return Err(PreprocessFileError {
                                    file_number: context.current_file_number,
                                    error: PreprocessError::MessageWithRange(
                                        "Token concatenation (##) must be followed by an identifier or number."
                                            .to_owned(),
                                        parser.last_location.range,
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
                    _ if context.definition.contains(name) => {
                        // The identifier is a macro.
                        let definition = context.definition.get(name).unwrap();
                        match definition {
                            definition::DefinitionItem::ObjectLike(tokens) => {
                                // The identifier is an object-like macro.
                                // Replace macro with its corresponding tokens.
                                let tokens_owned = tokens.to_owned();
                                let expanded_tokens = expand_marco(
                                    context,
                                    tokens_owned,
                                    argument_map,
                                    ExpandContextType::ObjectLikeDefinition,
                                )?;
                                output.extend(expanded_tokens);
                            }
                            definition::DefinitionItem::FunctionLike(params, tokens) => {
                                // The identifier is a function-like macro.
                                let params_owned = params.to_owned();
                                let tokens_owned = tokens.to_owned();

                                // collect the arguments for the macro invocation.
                                parser.expect_and_consume_opening_paren()?; // Consumes '('

                                let mut args = Vec::new();

                                while let Some(arg_token) = parser.peek_token(0) {
                                    match &arg_token {
                                        Token::Punctuator(Punctuator::ParenthesisClose) => {
                                            // If the next token is a closing parenthesis, we have reached the end of the arguments.
                                            break;
                                        }
                                        Token::String(first_string, first_string_type) => {
                                            // If the argument is a string, we need to handle it specially.

                                            // concatenates adjacent strings
                                            let arg_location = parser.peek_location(0).unwrap();
                                            let mut merged_string = vec![first_string.to_owned()];
                                            let mut merged_range = arg_location.range;

                                            let merged_string_type = *first_string_type;
                                            let merged_file_number = arg_location.file_number;

                                            parser.next_token(); // consumes the first string literal token

                                            // concatenate the next token if it is also a string literal.
                                            while let Some(next_arg_token) = parser.peek_token(0) {
                                                if let Token::String(
                                                    next_string,
                                                    next_string_type,
                                                ) = next_arg_token
                                                {
                                                    let next_arg_location =
                                                        parser.peek_location(0).unwrap();
                                                    if merged_string_type != *next_string_type {
                                                        // If the string types are different, we cannot concatenate them.
                                                        return Err(
                                                        PreprocessFileError::new(
                                                            merged_file_number,
                                                        PreprocessError::MessageWithRange(
                                                        "Cannot concatenate string literals with different encoding types."
                                                            .to_owned(),
                                                        next_arg_location.range,
                                                        )
                                                    ));
                                                    }

                                                    // merge the next string literal
                                                    merged_string.push(next_string.to_owned());
                                                    merged_range.end_included =
                                                        next_arg_location.range.end_included;

                                                    parser.next_token(); // consumes the next string literal token
                                                } else {
                                                    break;
                                                }
                                            }

                                            let merged_arg = TokenWithLocation::new(
                                                Token::String(
                                                    merged_string.join(""),
                                                    merged_string_type,
                                                ),
                                                Location::new(merged_file_number, &merged_range),
                                            );

                                            // Push the merged string literal token to the arguments.
                                            args.push(merged_arg);
                                        }
                                        Token::Identifier(_)
                                        | Token::Number(_)
                                        | Token::Char(_, _) => {
                                            // Valid argument type for macro invocation.
                                            let arg_location = parser.peek_location(0).unwrap();
                                            args.push(TokenWithLocation::new(
                                                arg_token.clone(),
                                                *arg_location,
                                            ));

                                            parser.next_token(); // consumes the token
                                        }
                                        _ => {
                                            // Invalid argument type for macro invocation.
                                            let arg_location = parser.peek_location(0).unwrap();
                                            return Err(PreprocessFileError {
                                                file_number: arg_location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "Invalid argument type for macro invocation, only single identifier, number, string (or adjacent strings), or char are allowed.".to_string(),
                                                    arg_location.range,
                                                ),
                                            });
                                        }
                                    }

                                    if parser.peek_token_and_equals(
                                        0,
                                        &Token::Punctuator(Punctuator::Comma),
                                    ) {
                                        // If the next token is a comma, consume it.
                                        parser.next_token(); // consumes the comma
                                    }
                                }

                                parser.expect_and_consume_closing_paren()?; // Consumes ')'

                                let is_variadic =
                                    matches!(params.last(), Some(name) if name =="...");
                                if (is_variadic && args.len() < params.len() - 1)
                                    || (!is_variadic && args.len() != params.len())
                                {
                                    return Err(PreprocessFileError {
                                        file_number: context.current_file_number,
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
                                    let expanded_token = expand_marco(
                                        context,
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
                                    expand_marco(context, tokens_owned, &map, function_type)?;

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
                        file_number: context.current_file_number,
                        error: PreprocessError::MessageWithRange(
                            "Stringizing (#) can only be used in function-like macro definitions."
                                .to_owned(),
                            current_token_with_location.location.range,
                        ),
                    });
                }

                // Consume the next token, which should be a macro parameter.
                if let Some(next_token_with_location) = parser.next_token_with_location() {
                    match &next_token_with_location.token {
                        Token::Identifier(name) => {
                            if name == "__VA_ARGS__" {
                                if expand_context_type
                                    != ExpandContextType::VariadicFunctionLikeDefinition
                                {
                                    return Err(PreprocessFileError {
                                        file_number: context.current_file_number,
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
                                        Token::String(generated_string, StringType::Default),
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
                                            Token::String(generated_string, StringType::Default),
                                            Location::default(),
                                        );
                                        output.push(generated_token);
                                    }
                                    _ => {
                                        return Err(PreprocessFileError {
                                            file_number: context.current_file_number,
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
                                file_number: context.current_file_number,
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
                        file_number: context.current_file_number,
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
                    file_number: context.current_file_number,
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

// Represents a pragma directive, e.g., `#pragma STDC FENV_ACCESS ON`.
// Pragmas are used to control compiler behavior and
// should be passed to the compiler for further processing.
#[derive(Debug, PartialEq, Clone)]
pub enum StandardPragma {
    FenvAccess(StandardPragmaValue), // Floating-point environment access
    FPContract(StandardPragmaValue), // Floating-point contractions
    CxLimitedRange(StandardPragmaValue), // Complex limited range
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StandardPragmaValue {
    Default,
    On,
    Off,
}

#[cfg(test)]
mod tests {
    use chrono::Local;
    use pretty_assertions::assert_eq;
    use std::{collections::HashMap, path::Path};

    use crate::{
        FILE_NUMBER_SOURCE_FILE_BEGIN, PreprocessError, PreprocessFileError, TokenWithLocation,
        file_cache::FileCache,
        location::Location,
        memory_file_provider::MemoryFileProvider,
        position::Position,
        preprocessor::{PreprocessResult, preprocess_source_file},
        prompt::{Prompt, PromptLevel},
        range::Range,
        token::{IntegerNumber, IntegerNumberType, Number, Punctuator, Token},
    };

    fn process_single_source(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Result<PreprocessResult, PreprocessFileError> {
        let mut file_cache = FileCache::new();
        let mut file_provider = MemoryFileProvider::new();
        file_provider.add_user_file(Path::new("src/main.c"), src);

        preprocess_source_file(
            FILE_NUMBER_SOURCE_FILE_BEGIN,
            Path::new("/projects/test/src/main.c"),
            false,
            predefinitions,
            &file_provider,
            &mut file_cache,
        )
    }

    fn process_single_source_result(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> PreprocessResult {
        process_single_source(src, predefinitions).unwrap()
    }

    fn process_single_source_tokens(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Vec<TokenWithLocation> {
        process_single_source_result(src, predefinitions).output
    }

    fn print_tokens(token_with_location: &[TokenWithLocation]) -> String {
        token_with_location
            .iter()
            .map(|TokenWithLocation { token, .. }| token.to_string())
            .collect::<Vec<_>>()
            .join(" ")
    }

    #[test]
    fn test_preprocess_code_without_directive() {
        let filenum = FILE_NUMBER_SOURCE_FILE_BEGIN;
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
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
                        IntegerNumberType::Default
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
    fn test_preprocess_pragma() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
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
    fn test_preprocess_error_directive() {
        let predefinitions = HashMap::new();
        let result = process_single_source(
            "\
#error \"foobar\"",
            &predefinitions,
        );

        assert!(matches!(
            result,
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
    fn test_preprocess_warning_directive() {
        let predefinitions = HashMap::new();
        let result = process_single_source_result(
            "\
#warning \"foobar\"",
            &predefinitions,
        );

        assert!(matches!(
            result.prompts.first().unwrap(),
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
    fn test_preprocess_predefinitions() {
        let mut predefinitions = HashMap::new();
        predefinitions.insert("FOO".to_string(), "'🦛'".to_string());
        predefinitions.insert("BAR".to_string(), "\"✨ abc\"".to_string());
        predefinitions.insert("A".to_string(), "123".to_string());
        predefinitions.insert("B".to_string(), "".to_string());
        predefinitions.insert("C".to_string(), "FOO".to_string()); // Reference to FOO

        let tokens = process_single_source_tokens(
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
    fn test_preprocess_dynamic_macro() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
            "\
__FILE__
__LINE__
__DATE__
__TIME__",
            &predefinitions,
        );

        let now = Local::now();
        let date_string = now.format("%b %e %Y").to_string();
        let time_string = now.format("%H:%M:%S").to_string();

        assert_eq!(
            print_tokens(&tokens),
            format!(r#""main.c" 2 "{}" "{}""#, date_string, time_string)
        );
    }

    #[test]
    fn test_preprocess_define() {
        let mut predefinitions = HashMap::new();
        predefinitions.insert("FOO".to_string(), "'🦛'".to_string());
        predefinitions.insert("BAR".to_string(), "\"✨ abc\"".to_string());

        let tokens = process_single_source_tokens(
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
            process_single_source(
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
            process_single_source(
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
    }

    #[test]
    fn test_preprocess_undefine() {
        let tokens = process_single_source_tokens(
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
            process_single_source(
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
            process_single_source(
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
    }

    #[test]
    fn test_preprocess_define_function() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
            r#"#define FOO(x) x
#define BAR(x, y) x y
#define A 'a'
#define B A

// string as argument
FOO("foo")

// adjacent strings as argument
FOO(
    "Hello"
    ","
    " "
    "World!"
)

// identifier as argument
FOO(foo)

// number and char as arguments
BAR(123, '✨')

// macros as arguments
BAR(A,B)"#,
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            r#""foo" "Hello, World!" foo 123 '✨' 'a' 'a'"#
        );

        let tokens_nested = process_single_source_tokens(
            r#"#define FOO(a) 1 a
#define BAR(x, y) 2 FOO(x) y
#define BUZ(z) 3 BAR(z, spark)
BUZ(hippo)"#,
            &predefinitions,
        );

        assert_eq!(print_tokens(&tokens_nested), "3 2 1 hippo spark");

        // err: redefine function-like macro
        assert!(matches!(
            process_single_source(
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
            process_single_source(
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
            process_single_source(
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
            process_single_source(
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
            process_single_source(
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
    fn test_preprocess_define_function_variadic() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
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
            process_single_source(
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
    fn test_preprocess_stringizing() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
            "\
#define FOO 3.14 \"world\" '🐘'
#define STR(x) #x
#define STR2(x) STR(x)
#define STR3(...) #__VA_ARGS__
STR(hello)
STR(\"world\")
STR(FOO)
STR('🦛')
STR2('🦛')
STR3(42)
STR3(1, '2', \"3\", id, FOO)",
            &predefinitions,
        );

        assert_eq!(
            print_tokens(&tokens),
            r#""hello" "\"world\"" "3.14 \"world\" \'🐘\'" "\'🦛\'" "\'🦛\'" "42" "1, \'2\', \"3\", id, 3.14 \"world\" \'🐘\'""#
        );

        // err: stringizing in a not function-like macro
        assert!(matches!(
            process_single_source(
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
            process_single_source(
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
    fn test_preprocess_token_concatenation() {
        let predefinitions = HashMap::new();
        let tokens = process_single_source_tokens(
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
            process_single_source(
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
            process_single_source(
                "\
#define CONCAT(a, b) a##b
CONCAT(1, hello)",
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

        // err: concatenate an identifier and a string
        assert!(matches!(
            process_single_source(
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
            process_single_source(
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
            process_single_source(
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
            process_single_source(
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
            process_single_source(
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
}
