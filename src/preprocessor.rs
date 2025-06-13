// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{collections::HashMap, iter::Peekable, path::Path, vec::IntoIter};

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
    token::{StandardPragma, StandardPragmaValue, Token},
};

pub const PEEK_BUFFER_LENGTH_PREPROCESS: usize = 4;
pub const PEEK_BUFFER_LENGTH_MERGE_STRINGS: usize = 2;

pub const FILE_NUMBER_SOURCE_FILE_BEGIN: usize = 2_usize.pow(16);

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
    let argument_name_values: HashMap<String, Vec<TokenWithLocation>> = HashMap::new();
    let expanded_tokens = expand_marco(context, tokens, &argument_name_values)?;
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
    argument_name_values: &HashMap<String, Vec<TokenWithLocation>>,
) -> Result<Vec<TokenWithLocation>, PreprocessFileError>
where
    T: FileProvider,
{
    let mut output = Vec::new();

    let mut iter = token_with_locations.into_iter();
    let mut peekable_iter = PeekableIter::new(&mut iter, 2);
    let mut parser = PreprocessorParser::new(&mut peekable_iter, context.current_file_number);

    while let Some(token_with_location) = parser.upstream.next() {
        match &token_with_location.token {
            Token::Identifier(name) => {
                // Check if the identifier is a macro.
                let definition = context.definition.get(name);
                if let Some(definition) = definition {
                    // The identifier is a macro.
                    match definition {
                        definition::DefinitionItem::ObjectLike(tokens) => {
                            // The identifier is an object-like macro.
                            // Replace macro with its corresponding tokens.
                            let tokens_owned = tokens.to_owned();
                            let expanded_tokens =
                                expand_marco(context, tokens_owned, argument_name_values)?;
                            output.extend(expanded_tokens);
                        }
                        definition::DefinitionItem::FunctionLike(params, tokens) => {
                            // The identifier is a function-like macro.
                            let params_owned = params.to_owned();
                            let tokens_owned = tokens.to_owned();

                            // collect the arguments for the macro invocation.
                            parser.expect_and_consume_opening_paren()?; // Consumes '('

                            let mut args = Vec::new();

                            for idx in 0..params_owned.len() {
                                if let Some(arg) = parser.upstream.next() {
                                    // If the argument is a string, we need to handle it specially.

                                    if let Token::String(first_string, first_string_type) =
                                        &arg.token
                                    {
                                        // concatenates adjacent strings
                                        let mut merged_string = vec![first_string.to_owned()];
                                        let mut merged_range = arg.location.range;

                                        // check if the next token is also a string literal.
                                        while let Some(next_arg) = parser.upstream.peek(0) {
                                            if let TokenWithLocation {
                                                token: Token::String(next_string, next_string_type),
                                                location: next_location,
                                            } = next_arg
                                            {
                                                if first_string_type != next_string_type {
                                                    // If the string types are different, we cannot concatenate them.
                                                    return Err(
                                                        PreprocessFileError::new(
                                                            arg.location.file_number,
                                                        PreprocessError::MessageWithRange(
                                                        "Cannot concatenate string literals with different encoding types."
                                                            .to_owned(),
                                                        next_location.range,
                                                        )
                                                    ));
                                                }

                                                // merge the two string literals.
                                                merged_string.push(next_string.to_owned());
                                                merged_range.end_included =
                                                    next_location.range.end_included;

                                                parser.upstream.next(); // consumes the string literal token
                                            } else {
                                                break;
                                            }
                                        }

                                        let merged_arg = TokenWithLocation::new(
                                            Token::String(
                                                merged_string.join(""),
                                                *first_string_type,
                                            ),
                                            Location::new(arg.location.file_number, &merged_range),
                                        );

                                        args.push(merged_arg);
                                    } else {
                                        // Other types token
                                        if matches!(
                                            arg.token,
                                            Token::Identifier(_)
                                                | Token::Number(_)
                                                | Token::Char(_, _)
                                        ) {
                                            args.push(arg);
                                        } else {
                                            return Err(PreprocessFileError {
                                                file_number: arg.location.file_number,
                                                error: PreprocessError::MessageWithRange(
                                                    "Invalid argument type for macro, only single identifier, number, string (or adjacent strings), or char are allowed.".to_string(),
                                                    arg.location.range,
                                                ),
                                            });
                                        }
                                    }
                                } else {
                                    return Err(PreprocessFileError {
                                        file_number: context.current_file_number,
                                        error: PreprocessError::MessageWithRange(
                                            format!(
                                                "Not enough arguments provided for macro: {}.",
                                                name
                                            ),
                                            token_with_location.location.range,
                                        ),
                                    });
                                }

                                if idx != params.len() - 1 {
                                    // If not the last argument, expect a comma.
                                    parser.expect_and_consume_comma()?;
                                }
                            }

                            parser.expect_and_consume_closing_paren()?; // Consumes ')'

                            // Expand arguments
                            let mut expanded_args = Vec::new();
                            for arg in args {
                                let expanded_token =
                                    expand_marco(context, vec![arg], argument_name_values)?;
                                expanded_args.push(expanded_token);
                            }

                            // Construct the arguments with name-value pairs.
                            let argument_name_values: HashMap<String, Vec<TokenWithLocation>> =
                                params_owned
                                    .iter()
                                    .zip(expanded_args.iter())
                                    .map(|(arg_name, arg_value)| {
                                        (arg_name.to_owned(), arg_value.to_owned())
                                    })
                                    .collect();

                            println!("Expanding macro: {} ---------------", name);
                            argument_name_values
                                .iter()
                                .for_each(|(arg_name, arg_values)| {
                                    // Replace the parameter with the argument value.
                                    println!(
                                        "  arg: {}={}",
                                        arg_name,
                                        arg_values
                                            .iter()
                                            .map(|item| item.token.to_string())
                                            .collect::<Vec<_>>()
                                            .join(", ")
                                    );
                                });

                            // Expand the macro body with the arguments.
                            let expanded_tokens =
                                expand_marco(context, tokens_owned, &argument_name_values)?;
                            output.extend(expanded_tokens);
                        }
                    }
                } else {
                    // Check if the identifier is a parameter.
                    if let Some(arg_values) = argument_name_values.get(name) {
                        // If the identifier is a parameter, replace it with its value.
                        output.extend(arg_values.to_owned());
                    } else {
                        // If the identifier is not a macro or parameter, just push it as is.
                        output.push(token_with_location);
                    }
                }
            }
            _ => {
                output.push(token_with_location);
            }
        }
    }

    Ok(output)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::{collections::HashMap, path::Path};

    use crate::{
        PreprocessError, PreprocessFileError, TokenWithLocation,
        file_cache::FileCache,
        location::Location,
        memory_file_provider::MemoryFileProvider,
        position::Position,
        preprocessor::{FILE_NUMBER_SOURCE_FILE_BEGIN, PreprocessResult, preprocess_source_file},
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
        let result = process_single_source("#error \"foobar\"", &predefinitions);

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
        let result = process_single_source_result("#warning \"foobar\"", &predefinitions);

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

        let tokens = process_single_source_tokens("hello FOO BAR A B C world.", &predefinitions);
        assert_eq!(
            print_tokens(&tokens),
            "hello '🦛' \"✨ abc\" 123 '🦛' world ."
        );
    }

    #[test]
    fn test_preprocess_define_object() {
        let mut predefinitions = HashMap::new();
        predefinitions.insert("FOO".to_string(), "'🦛'".to_string());
        predefinitions.insert("BAR".to_string(), "\"✨ abc\"".to_string());

        let tokens = process_single_source_tokens(
            r#"
        #define A 123
        #define B
        #define C FOO
        hello FOO BAR A B C world.
        "#,
            &predefinitions,
        );
        assert_eq!(
            print_tokens(&tokens),
            "hello '🦛' \"✨ abc\" 123 '🦛' world ."
        );

        // err: redefine FOO
        assert!(matches!(
            process_single_source(
                r#"
        #define FOO 'a'
        #define FOO 'b'
        hello FOO world.
        "#,
                &HashMap::new(),
            ),
            Err(PreprocessFileError {
                file_number: FILE_NUMBER_SOURCE_FILE_BEGIN,
                error: PreprocessError::MessageWithRange(
                    _,
                    Range {
                        start: Position {
                            index: 41,
                            line: 2,
                            column: 16
                        },
                        end_included: Position {
                            index: 43,
                            line: 2,
                            column: 18
                        }
                    }
                )
            })
        ));
    }

    #[test]
    fn test_preprocess_undefine() {
        let tokens = process_single_source_tokens(
            r#"
        #define A 123
        #undef A
        #define A 456
        hello A world.
        "#,
            &HashMap::new(),
        );
        assert_eq!(print_tokens(&tokens), "hello 456 world .");
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
    }
}
