// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{collections::HashMap, path::Path};

use crate::{
    context::Context, file_provider::FileProvider, peekableiter::PeekableIter, prompt::Prompt, range::Range, PreprocessError, TokenWithLocation, TokenWithRange
};

pub const PEEK_BUFFER_LENGTH_PREPROCESS: usize = 4;
pub const PEEK_BUFFER_LENGTH_MERGE_STRINGS: usize = 2;

/// Preprocesses C source files.
///
/// Arguments:
///
/// - `source_files`: The C source files to preprocess.
/// - `system_headers_directories`: Directories to search for system and external module headers.
///   These are used when resolving `#include` directives with angle brackets,
///   e.g., `#include <stdio.h>`.
/// - `user_headers_directories`: Directories to search for module-specific headers.
///   These are used when resolving `#include` directives with double quotes,
///   e.g., `#include "relative/path/to/header.h"`.
/// - `find_header_in_source_file_directory`: If true, also search for headers in the directory where the source file resides.
///   For example, if the source file is `src/foo/bar.c` and this flag is set,
///   then `#include "header.h"` will look in `src/foo/`,
///   and `#include "../hello/world.h"` will look in `src/hello`.
///   otherwise, it will only search in the specified `user_headers_directories`.
pub fn preprocess_source_file(
    source_files: &Path,
    file_number: usize,
    system_headers_directories: &[&Path],
    user_headers_directories: &[&Path],
    find_header_in_source_file_directory: bool,
    predefined: HashMap<String, String>,
) -> Result<PreprocessResult, PreprocessError> {
    todo!()

    // source_code: &str) -> Result<ModuleNode, ParserError> {
    // let tokens = lex_from_str(source_code)?;
    // let clean_tokens = clean(tokens);
    // let normalized_tokens = normalize(clean_tokens)?;
    // let mut token_iter = normalized_tokens.into_iter();
    // let mut peekable_token_iter = PeekableIter::new(&mut token_iter, PEEK_BUFFER_LENGTH_PREPROCESS);
    // let mut parser = Preprocessor::new(&mut peekable_token_iter);
    // parser.parse_module_node()
}

pub struct PreprocessResult {
    pub tokens: Vec<TokenWithLocation>,
    pub prompts: Vec<Prompt>,
}

// fn merge_continous_strings(source_code: &str) -> Result<Vec<TokenWithRange>, PreprocessError> {
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

// see:
// - https://en.cppreference.com/w/c/language.html
// - https://en.cppreference.com/w/c/preprocessor.html
pub struct Preprocessor<'a, T>
    where
        T: 'a + FileProvider,
{
    upstream: &'a mut PeekableIter<'a, TokenWithRange>,
    last_range: Range,
    context: Context<'a, T>,
}

// impl<'a> Preprocessor<'a> {
//     fn new(upstream: &'a mut PeekableIter<'a, TokenWithRange>) -> Self {
//         Self {
//             upstream,
//             last_range: Location::new_range(/*0,*/ 0, 0, 0, 0),
//         }
//     }
//
//     fn next_token(&mut self) -> Option<Token> {
//         match self.upstream.next() {
//             Some(TokenWithRange { token, range }) => {
//                 self.last_range = range;
//                 Some(token)
//             }
//             None => None,
//         }
//     }
//
//     fn peek_range(&self, offset: usize) -> Option<&Location> {
//         match self.upstream.peek(offset) {
//             Some(TokenWithRange { range, .. }) => Some(range),
//             None => None,
//         }
//     }
//
//     fn peek_token(&self, offset: usize) -> Option<&Token> {
//         match self.upstream.peek(offset) {
//             Some(TokenWithRange { token, .. }) => Some(token),
//             None => None,
//         }
//     }
//
//     fn expect_token(&self, offset: usize, expected_token: &Token) -> bool {
//         matches!(
//             self.peek_token(offset),
//             Some(token) if token == expected_token)
//     }
//
//     /// Returns:
//     /// - `None` if the specified token is not found.
//     /// - `Some(false)` found the token without new-line.
//     /// - `Some(true)` found the token and new-line.
//     fn expect_token_ignore_newline(&self, offset: usize, expected_token: &Token) -> Option<bool> {
//         if self.expect_token(offset, expected_token) {
//             Some(false)
//         } else if self.expect_token(offset, &Token::NewLine)
//             && self.expect_token(offset + 1, expected_token)
//         {
//             Some(true)
//         } else {
//             None
//         }
//     }
//
//     fn expect_keyword(&self, offset: usize, expected_keyword: &str) -> bool {
//         matches!(self.peek_token(offset), Some(Token::Keyword(keyword)) if keyword == expected_keyword )
//     }
//
//     // /// Returns:
//     // /// - `None` if the specified token is not found,
//     // /// - `Some(false)` if no new-line is found,
//     // /// - `Some(true)` otherwise.
//     // fn expect_keyword_ignore_newline(&self, offset: usize, expected_keyword: &str) -> Option<bool> {
//     //     if self.expect_keyword(offset, expected_keyword) {
//     //         Some(false)
//     //     } else if self.expect_token(offset, &Token::NewLine)
//     //         && self.expect_keyword(offset + 1, expected_keyword)
//     //     {
//     //         Some(true)
//     //     } else {
//     //         None
//     //     }
//     // }
//
//     // consume '\n' if it exists.
//     fn consume_new_line_if_exist(&mut self) -> bool {
//         match self.peek_token(0) {
//             Some(Token::NewLine) => {
//                 self.next_token();
//                 true
//             }
//             _ => false,
//         }
//     }
//
//     // consume '\n' or ',' if they exist.
//     fn consume_new_line_or_comma_if_exist(&mut self) -> bool {
//         match self.peek_token(0) {
//             Some(Token::NewLine | Token::Comma) => {
//                 self.next_token();
//                 true
//             }
//             _ => false,
//         }
//     }
//
//     // fn consume_new_line_or_eof(&mut self) -> Result<(), Error> {
//     //     match self.peek_token(0) {
//     //         Some(Token::NewLine) => {
//     //             self.next_token();
//     //             Ok(())
//     //         }
//     //         Some(_) => Err(PreprocessError::MessageWithPosition(
//     //             "Expect a new-line.".to_owned(),
//     //             self.peek_range(0).unwrap().get_position_by_range_start(),
//     //         )),
//     //         None => Ok(()),
//     //     }
//     // }
//
//     // fn consume_new_line_or_comma(&mut self) -> Result<(), Error> {
//     //     match self.peek_token(0) {
//     //         Some(Token::NewLine | Token::Comma) => {
//     //             self.next_token();
//     //             Ok(())
//     //         }
//     //         Some(_) => Err(PreprocessError::MessageWithPosition(
//     //             "Expect a comma or new-line.".to_owned(),
//     //             self.peek_range(0).unwrap().get_position_by_range_start(),
//     //         )),
//     //         None => Err(Error::UnexpectedEndOfDocument(
//     //             "Expect a comma or new-line.".to_owned(),
//     //         )),
//     //     }
//     // }
//
//     fn consume_token(
//         &mut self,
//         expected_token: &Token,
//         token_description: &str,
//     ) -> Result<(), ParserError> {
//         match self.next_token() {
//             Some(token) => {
//                 if &token == expected_token {
//                     Ok(())
//                 } else {
//                     Err(PreprocessError::MessageWithPosition(
//                         format!("Expect token: {}.", token_description),
//                         self.last_range.get_position_by_range_start(),
//                     ))
//                 }
//             }
//             None => Err(ParserError::UnexpectedEndOfDocument(format!(
//                 "Expect token: {}.",
//                 token_description
//             ))),
//         }
//     }
//
//     fn consume_full_name(&mut self) -> Result<String, ParserError> {
//         match self.next_token() {
//             Some(Token::FullName(s)) => Ok(s),
//             Some(_) => Err(PreprocessError::MessageWithPosition(
//                 "Expect a name path.".to_owned(),
//                 self.last_range.get_position_by_range_start(),
//             )),
//             None => Err(ParserError::UnexpectedEndOfDocument(
//                 "Expect a name path.".to_owned(),
//             )),
//         }
//     }
//
//     fn consume_name(&mut self) -> Result<String, ParserError> {
//         match self.next_token() {
//             Some(Token::Name(s)) => Ok(s),
//             Some(_) => Err(PreprocessError::MessageWithPosition(
//                 "Expect a name.".to_owned(),
//                 self.last_range.get_position_by_range_start(),
//             )),
//             None => Err(ParserError::UnexpectedEndOfDocument(
//                 "Expect a name.".to_owned(),
//             )),
//         }
//     }
//
//     fn consume_keyword(&mut self, keyword: &str) -> Result<String, ParserError> {
//         match self.next_token() {
//             Some(Token::Keyword(s)) if s == keyword => Ok(s),
//             Some(_) => Err(PreprocessError::MessageWithPosition(
//                 format!("Expect keyword \"{}\".", keyword),
//                 self.last_range.get_position_by_range_start(),
//             )),
//             None => Err(ParserError::UnexpectedEndOfDocument(format!(
//                 "Expect keyword \"{}\".",
//                 keyword
//             ))),
//         }
//     }
//
//     fn consume_number_i32(&mut self) -> Result<u32, ParserError> {
//         match self.next_token() {
//             Some(Token::Number(NumberToken::I32(n))) => Ok(n),
//             Some(_) => Err(PreprocessError::MessageWithPosition(
//                 "Expect an i32 number.".to_owned(),
//                 self.last_range.get_position_by_range_start(),
//             )),
//             None => Err(ParserError::UnexpectedEndOfDocument(
//                 "Expect an i32 number.".to_owned(),
//             )),
//         }
//     }
//
//     // '('
//     fn consume_left_paren(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::LeftParen, "left parenthesis")
//     }
//
//     // ')'
//     fn consume_right_paren(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::RightParen, "right parenthesis")
//     }
//
//     // '['
//     fn consume_left_bracket(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::LeftBracket, "left bracket")
//     }
//
//     // ']'
//     fn consume_right_bracket(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::RightBracket, "right bracket")
//     }
//
//     // '}'
//     fn consume_right_brace(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::RightBrace, "right brace")
//     }
//
//     // '='
//     fn consume_equal(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::Equal, "equal sign")
//     }
//
//     // ':'
//     fn consume_colon(&mut self) -> Result<(), ParserError> {
//         self.consume_token(&Token::Colon, "colon sign")
//     }
// }
