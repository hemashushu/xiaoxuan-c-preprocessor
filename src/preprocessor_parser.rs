// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    PreprocessError, PreprocessFileError, TokenWithLocation,
    location::Location,
    peekable_iter::PeekableIter,
    range::Range,
    token::{Punctuator, Token},
};

pub struct PreprocessorParser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
    pub last_location: Location,
    pub current_file_number: usize,
}

impl<'a> PreprocessorParser<'a> {
    pub fn new(
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

    pub fn next_token_with_location(&mut self) -> Option<TokenWithLocation> {
        match self.upstream.next() {
            Some(token_with_location) => {
                self.last_location = token_with_location.location;
                Some(token_with_location)
            }
            None => None,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithLocation { token, location }) => {
                self.last_location = location;
                Some(token)
            }
            None => None,
        }
    }

    pub fn peek_location(&self, offset: usize) -> Option<&Location> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { location, .. }) => Some(location),
            None => None,
        }
    }

    pub fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { token, .. }) => Some(token),
            None => None,
        }
    }

    pub fn peek_token_and_equals(&self, offset: usize, expected_token: &Token) -> bool {
        matches!(
            self.peek_token(offset),
            Some(token) if token == expected_token)
    }

    //     fn expect_and_consume_string(&mut self) -> Result<(String, StringType), PreprocessFileError> {
    //         match self.next_token() {
    //             Some(Token::String(s, t)) => Ok((s, t)),
    //             Some(_) => Err(PreprocessFileError::MessageWithPosition(
    //                 "Expect a string.".to_owned(),
    //                 self.last_location.start,
    //             )),
    //             None => Err(PreprocessFileError::UnexpectedEndOfDocument(
    //                 "Expect a string.".to_owned(),
    //             )),
    //         }
    //     }
    //
    //     fn expect_and_consume_number(&mut self) -> Result<Number, PreprocessFileError> {
    //         match self.next_token() {
    //             Some(Token::Number(n)) => Ok(n),
    //             Some(_) => Err(PreprocessFileError::MessageWithPosition(
    //                 "Expect a number.".to_owned(),
    //                 self.last_location.start,
    //             )),
    //             None => Err(PreprocessFileError::UnexpectedEndOfDocument(
    //                 "Expect a number.".to_owned(),
    //             )),
    //         }
    //     }
    //
    //     fn expect_and_consume_newline_or_eof(&mut self) -> Result<(), PreprocessFileError> {
    //         match self.next_token() {
    //             Some(Token::Newline) => Ok(()),
    //             Some(_) => Err(PreprocessFileError::MessageWithPosition(
    //                 "Expect a newline.".to_owned(),
    //                 self.last_location.start,
    //             )),
    //             None => Ok(()), // EOF is acceptable here
    //         }
    //     }
    //
    //     fn expect_and_consume_newline(&mut self) -> Result<(), PreprocessFileError> {
    //         match self.next_token() {
    //             Some(Token::Newline) => Ok(()),
    //             _ => Err(PreprocessFileError::MessageWithPosition(
    //                 "Expect a newline.".to_owned(),
    //                 self.last_location.start,
    //             )),
    //         }
    //     }

    pub fn expect_and_consume_token(
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
    pub fn expect_and_consume_opening_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    // expects close parenthesis ')' and consumes it.
    pub fn expect_and_consume_closing_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisClose),
            "closing parenthesis",
        )
    }

    // expects comma ',' and consumes it.
    pub fn expect_and_consume_comma(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(&Token::Punctuator(Punctuator::Comma), "comma")
    }

    //     // '['
    //     fn consume_left_bracket(&mut self) -> Result<(), PreprocessFileError> {
    //         self.expect_and_consume_token(&Token::LeftBracket, "left bracket")
    //     }
    //
    //     // ']'
    //     fn consume_right_bracket(&mut self) -> Result<(), PreprocessFileError> {
    //         self.expect_and_consume_token(&Token::RightBracket, "right bracket")
    //     }
    //
    //     // '}'
    //     fn consume_right_brace(&mut self) -> Result<(), PreprocessFileError> {
    //         self.expect_and_consume_token(&Token::RightBrace, "right brace")
    //     }
    //
    //     // '='
    //     fn consume_equal(&mut self) -> Result<(), PreprocessFileError> {
    //         self.expect_and_consume_token(&Token::Equal, "equal sign")
    //     }
    //
    //     // ':'
    //     fn consume_colon(&mut self) -> Result<(), PreprocessFileError> {
    //         self.expect_and_consume_token(&Token::Colon, "colon sign")
    //     }
}
