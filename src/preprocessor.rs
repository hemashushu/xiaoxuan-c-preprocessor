// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    thread::LocalKey,
};

use crate::{
    PreprocessError, TokenWithLocation, TokenWithRange,
    ast::{Program, Statement},
    context::{self, Context},
    file_cache::FileCache,
    file_provider::FileProvider,
    lexer::lex_from_str,
    location::Location,
    parser::parse_from_str,
    peekable_iter::PeekableIter,
    prompt::Prompt,
    range::Range,
    token::{self, Token},
};

pub const PEEK_BUFFER_LENGTH_PREPROCESS: usize = 4;
pub const PEEK_BUFFER_LENGTH_MERGE_STRINGS: usize = 2;

pub const FILE_NUMBER_SOURCE_FILE_BEGIN: usize = 2 ^ 16;

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
) -> Result<PreprocessResult, PreprocessError>
where
    T: FileProvider,
{
    let src = file_provider.load_file(file_canonical_path).map_err(|e| {
        PreprocessError::Message(format!(
            "Failed to load file '{}': {}",
            file_canonical_path.to_string_lossy(),
            e
        ))
    })?;

    let program = parse_from_str(&src)?;
    let mut context =
        Context::from_keyvalues(file_number, file_provider, file_cache, predefinitions)?;

    preprocess_program(&mut context, &program)?;

    let Context {
        prompts, tokens, ..
    } = context;

    let result = PreprocessResult { tokens, prompts };

    Ok(result)
}

pub struct PreprocessResult {
    pub tokens: Vec<TokenWithLocation>,
    pub prompts: Vec<Prompt>,
}

fn preprocess_program<T>(context: &mut Context<T>, program: &Program) -> Result<(), PreprocessError>
where
    T: FileProvider,
{
    for statement in &program.statements {
        match statement {
            Statement::Pragma(pragma) => todo!(),
            Statement::Define(define) => todo!(),
            Statement::Undef(_, range) => todo!(),
            Statement::Include(include) => todo!(),
            Statement::Embed(embed) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Error(_) => todo!(),
            Statement::Warning(_) => todo!(),
            Statement::Code(token_with_ranges) => preprocess_code(context, token_with_ranges)?,
        }
    }

    Ok(())
}

fn preprocess_code<T>(
    context: &mut Context<T>,
    token_with_ranges: &[TokenWithRange],
) -> Result<(), PreprocessError>
where
    T: FileProvider,
{
    for token_with_range in token_with_ranges {
        match &token_with_range.token {
            Token::Identifier(_) => {
                // Handle identifiers, possibly as macros.
                context.tokens.push(TokenWithLocation::new(
                    token_with_range.token.clone(),
                    Location::new(context.current_file_number, &token_with_range.range),
                ));
            }
            _ => {
                context.tokens.push(TokenWithLocation::new(
                    token_with_range.token.clone(),
                    Location::new(context.current_file_number, &token_with_range.range),
                ));
            }
        }
    }

    Ok(())
}

// concatenate adjacent string literals
// Note that only two string literals have identical encoding prefixes can be concatenated.
// see:
// - https://en.cppreference.com/w/c/language/string_literal.html
//
// fn concatenate_adjacent_strings(source_code: &str) -> Result<Vec<TokenWithRange>, PreprocessError> {
//     // merges continuous strings.
//     let mut merged_tokens = Vec::new();
//
//     let tokens = lex_from_str(source_code)?;
//     let mut token_iter = tokens.into_iter();
//     let mut peekable_token_iter =
//         PeekableIter::new(&mut token_iter, PEEK_BUFFER_LENGTH_MERGE_STRINGS);
//
//     while let Some(token_with_range) = peekable_token_iter.next() {
//         if let TokenWithRange {
//             token: Token::String(first_string, first_type),
//             range: first_range,
//         } = &token_with_range
//         {
//             let mut merged_string = vec![first_string.to_owned()];
//             let mut merged_range = Range::new(&first_range.start, &first_range.end_included);
//
//             // check if the next token is also a string literal.
//             while let Some(next_token_with_range) = peekable_token_iter.peek(0) {
//                 if let TokenWithRange {
//                     token: Token::String(next_string, _next_type),
//                     range: next_range,
//                 } = next_token_with_range
//                 {
//                     // merge the two string literals.
//                     merged_string.push(next_string.to_owned());
//                     merged_range.end_included = next_range.end_included;
//
//                     peekable_token_iter.next(); // consumes the string literal token
//                 } else {
//                     break;
//                 }
//             }
//
//             let merged_token_with_range = TokenWithRange {
//                 token: Token::String(merged_string.join(""), *first_type),
//                 range: merged_range,
//             };
//
//             merged_tokens.push(merged_token_with_range);
//         } else {
//             merged_tokens.push(token_with_range);
//         }
//     }
//
//     Ok(merged_tokens)
// }

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use std::{collections::HashMap, path::Path};

    use crate::{
        TokenWithLocation,
        file_cache::FileCache,
        location::Location,
        memory_file_provider::MemoryFileProvider,
        preprocessor::{FILE_NUMBER_SOURCE_FILE_BEGIN, PreprocessResult, preprocess_source_file},
        token::{IntegerNumber, IntegerNumberType, Number, Punctuator, Token},
    };

    fn generate_single_source(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> PreprocessResult {
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
        .unwrap()
    }

    fn generate_single_source_tokens(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Vec<TokenWithLocation> {
        generate_single_source(src, predefinitions).tokens
    }

    fn generate_single_source_tokens_with_range_strip(
        src: &str,
        predefinitions: &HashMap<String, String>,
    ) -> Vec<Token> {
        generate_single_source_tokens(src, predefinitions)
            .into_iter()
            .map(|token_with_location| token_with_location.token)
            .collect()
    }

    #[test]
    fn test_preprocess_no_directive() {
        let filenum = FILE_NUMBER_SOURCE_FILE_BEGIN;
        let predefinitions = HashMap::new();
        let tokens = generate_single_source_tokens(
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
    }
}
