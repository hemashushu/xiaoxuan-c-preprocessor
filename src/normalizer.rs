// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::ops::Neg;

use crate::{
    location::Location,
    peekableiter::PeekableIter,
    token::{NumberToken, Token, TokenWithRange},
    ParserError,
};

pub fn clean(tokens: Vec<TokenWithRange>) -> Vec<TokenWithRange> {
    // remove all comments.
    let clean_tokens: Vec<TokenWithRange> = tokens
        .into_iter()
        .filter(|item| {
            !matches!(
                item,
                TokenWithRange {
                    token: Token::Comment(_),
                    ..
                }
            )
        })
        .collect();

    clean_tokens
}

pub fn normalize(tokens: Vec<TokenWithRange>) -> Result<Vec<TokenWithRange>, ParserError> {
    // combine multiple continuous newlines into one newline.
    // rules:
    //   + blanks => blank
    //   + comma + blank(s) => comma
    //   + blank(s) + comma => comma
    //   + blank(s) + comma + blank(s) => comma
    //
    // because the comments have been removed, the following conclusions
    // can be inferred:
    //   + comma + comment(s) + comma => comma + comma
    //   + blank(s) + comment(s) + blank(s) => blank
    //
    // - remove the '+' tokens in front of numbers.
    // - apple the '-' tokens to numbers.
    // - checks if the signed number is overflowed.
    //   note that the lexer does not check the valid range of a signed integer
    //   because in the lexing phase the lexer only extracts tokens and does not
    //   check the validity of a combination of tokens.
    //   i.e., the integer does not know if it is preceded by a plus or minus sign.
    //   for example, "128" is an invalid i8, but "-128" is a valid i8.
    //   thus the valid range of an integer can only be checked in the normalization
    //   phase after combining the plus or minus sign and the number of tokens.

    let mut token_iter = tokens.into_iter();
    let mut peekable_token_iter = PeekableIter::new(&mut token_iter, 1);
    let mut normalized_tokens: Vec<TokenWithRange> = vec![];

    while let Some(token_with_range) = peekable_token_iter.next() {
        let TokenWithRange {
            token,
            range: current_range,
        } = &token_with_range;

        let mut start_range = *current_range;
        let mut end_range = start_range;

        let compact_token_with_range = match token {
            Token::NewLine => {
                // consume continuous newlines
                while let Some(TokenWithRange {
                    token: Token::NewLine,
                    range: current_range,
                }) = peekable_token_iter.peek(0)
                {
                    end_range = *current_range;
                    peekable_token_iter.next();
                }

                // found ','
                if let Some(TokenWithRange {
                    token: Token::Comma,
                    range: current_range,
                }) = peekable_token_iter.peek(0)
                {
                    // consume comma
                    start_range = *current_range;
                    end_range = start_range;
                    peekable_token_iter.next();

                    // consume trailing continuous newlines
                    while let Some(TokenWithRange {
                        token: Token::NewLine,
                        range: _,
                    }) = peekable_token_iter.peek(0)
                    {
                        peekable_token_iter.next();
                    }

                    TokenWithRange::new(
                        Token::Comma,
                        Location::from_range_pair(&start_range, &end_range),
                    )
                } else {
                    TokenWithRange::new(
                        Token::NewLine,
                        Location::from_range_pair(&start_range, &end_range),
                    )
                }
            }
            Token::Comma => {
                // consume trailing continuous newlines
                while let Some(TokenWithRange {
                    token: Token::NewLine,
                    range: _,
                }) = peekable_token_iter.peek(0)
                {
                    peekable_token_iter.next();
                }

                TokenWithRange::new(
                    Token::Comma,
                    Location::from_range_pair(&start_range, &end_range),
                )
            }
            Token::Plus => {
                match peekable_token_iter.peek(0) {
                    Some(TokenWithRange {
                        token: Token::Number(_),
                        ..
                    }) => {
                        // consumes the the plus sign (it's already done) and the
                        // number token.
                        let TokenWithRange {
                            token: combined_token,
                            range: end_range,
                        } = peekable_token_iter.next().unwrap();

                        // combines two token ranges and constructs new number token.
                        TokenWithRange {
                            token: combined_token,
                            range: Location::from_range_pair(&start_range, &end_range),
                        }
                    }
                    Some(TokenWithRange {
                        token: _,
                        range: current_range,
                    }) => {
                        // combines two token ranges.
                        return Err(PreprocessError::MessageWithPosition(
                            "The plus sign can only be applied to numbers.".to_owned(),
                            Location::from_range_pair(&start_range, current_range),
                        ));
                    }
                    None => {
                        // "...+EOF"
                        return Err(ParserError::UnexpectedEndOfDocument(
                            "Missing the number that follow the plus sign.".to_owned(),
                        ));
                    }
                }
            }
            Token::Minus => {
                match peekable_token_iter.peek(0) {
                    Some(TokenWithRange {
                        token: Token::Number(num),
                        range: current_range,
                    }) => {
                        match num {
                            NumberToken::F32(v) => {
                                // combines two token ranges and constructs new number token.
                                let ret_val = TokenWithRange {
                                    token: Token::Number(NumberToken::F32(v.neg())),
                                    range: Location::from_range_pair(&start_range, current_range),
                                };

                                // consume the minus sign (it's already done) and the
                                // number token
                                peekable_token_iter.next();

                                ret_val
                            }
                            NumberToken::F64(v) => {
                                // combines two token ranges and constructs new number token.
                                let ret_val = TokenWithRange {
                                    token: Token::Number(NumberToken::F64(v.neg())),
                                    range: Location::from_range_pair(&start_range, current_range),
                                };

                                // consume the minus sign (it's already done) and the
                                // number token
                                peekable_token_iter.next();

                                ret_val
                            }
                            NumberToken::I8(v) => {
                                let combined_range =
                                    Location::from_range_pair(&start_range, current_range);

                                let parse_result =
                                    format!("-{}", v).parse::<i8>().map_err(|_| {
                                        PreprocessError::MessageWithPosition(
                                            format!("Can not convert \"{}\" to negative i8", v),
                                            combined_range,
                                        )
                                    })?;

                                let ret_val = TokenWithRange::new(
                                    Token::Number(NumberToken::I8(parse_result as u8)),
                                    combined_range,
                                );

                                // consume the minus sign (already done) and the number literal token
                                peekable_token_iter.next();

                                ret_val
                            }
                            NumberToken::I16(v) => {
                                let combined_range =
                                    Location::from_range_pair(&start_range, current_range);

                                let parse_result =
                                    format!("-{}", v).parse::<i16>().map_err(|_| {
                                        PreprocessError::MessageWithPosition(
                                            format!("Can not convert \"{}\" to negative i16.", v),
                                            combined_range,
                                        )
                                    })?;

                                let ret_val = TokenWithRange::new(
                                    Token::Number(NumberToken::I16(parse_result as u16)),
                                    combined_range,
                                );

                                // consume the minus sign (already done) and the number literal token
                                peekable_token_iter.next();

                                ret_val
                            }
                            NumberToken::I32(v) => {
                                let combined_range =
                                    Location::from_range_pair(&start_range, current_range);

                                let parse_result =
                                    format!("-{}", v).parse::<i32>().map_err(|_| {
                                        PreprocessError::MessageWithPosition(
                                            format!("Can not convert \"{}\" to negative i32.", v),
                                            combined_range,
                                        )
                                    })?;

                                let ret_val = TokenWithRange::new(
                                    Token::Number(NumberToken::I32(parse_result as u32)),
                                    combined_range,
                                );

                                // consume the minus sign (already done) and the number literal token
                                peekable_token_iter.next();

                                ret_val
                            }
                            NumberToken::I64(v) => {
                                let combined_range =
                                    Location::from_range_pair(&start_range, current_range);

                                let parse_result =
                                    format!("-{}", v).parse::<i64>().map_err(|_| {
                                        PreprocessError::MessageWithPosition(
                                            format!("Can not convert \"{}\" to negative i64.", v),
                                            combined_range,
                                        )
                                    })?;

                                let ret_val = TokenWithRange::new(
                                    Token::Number(NumberToken::I64(parse_result as u64)),
                                    combined_range,
                                );

                                // consume the minus sign (already done) and the number literal token
                                peekable_token_iter.next();

                                ret_val
                            }
                        }
                    }
                    Some(TokenWithRange {
                        token: _,
                        range: current_range,
                    }) => {
                        // combines two token ranges.
                        return Err(PreprocessError::MessageWithPosition(
                            "The minus sign can only be applied to numbers.".to_owned(),
                            Location::from_range_pair(&start_range, current_range),
                        ));
                    }
                    None => {
                        // "...-EOF"
                        return Err(ParserError::UnexpectedEndOfDocument(
                            "Missing the number that follow the minus sign.".to_owned(),
                        ));
                    }
                }
            }
            _ => token_with_range,
        };

        normalized_tokens.push(compact_token_with_range);
    }

    // remove document leading and tailing newlines.
    if let Some(TokenWithRange {
        token: Token::NewLine,
        ..
    }) = normalized_tokens.first()
    {
        normalized_tokens.remove(0);
    }

    if let Some(TokenWithRange {
        token: Token::NewLine,
        ..
    }) = normalized_tokens.last()
    {
        normalized_tokens.pop();
    }

    Ok(normalized_tokens)
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        lexer::lex_from_str,
        location::Location,
        token::{NumberToken, Token, TokenWithRange},
        ParserError,
    };

    use super::{clean, normalize};

    fn clean_and_lex_from_str(s: &str) -> Result<Vec<TokenWithRange>, ParserError> {
        let tokens = lex_from_str(s)?;
        let clean_tokens = clean(tokens);
        Ok(clean_tokens)
    }

    fn clean_and_lex_from_str_without_location(s: &str) -> Result<Vec<Token>, ParserError> {
        let tokens = clean_and_lex_from_str(s)?
            .into_iter()
            .map(|e| e.token)
            .collect::<Vec<Token>>();
        Ok(tokens)
    }

    fn normalize_and_lex_from_str(s: &str) -> Result<Vec<TokenWithRange>, ParserError> {
        let tokens = lex_from_str(s)?;
        let clean_tokens = clean(tokens);
        normalize(clean_tokens)
    }

    fn normalize_and_lex_from_str_without_location(s: &str) -> Result<Vec<Token>, ParserError> {
        let tokens = normalize_and_lex_from_str(s)?
            .into_iter()
            .map(|e| e.token)
            .collect::<Vec<Token>>();
        Ok(tokens)
    }

    #[test]
    fn test_clean_comments() {
        assert_eq!(
            clean_and_lex_from_str_without_location(
                r#"11 // line comment 1
                // line comment 2
                13 /* block comment 1 */
                /*
                block comment 2
                */
                17
                "#
            )
            .unwrap(),
            vec![
                Token::Number(NumberToken::I32(11)),
                Token::NewLine,
                Token::NewLine,
                Token::Number(NumberToken::I32(13)),
                Token::NewLine,
                Token::NewLine,
                Token::Number(NumberToken::I32(17)),
                Token::NewLine,
            ]
        );

        assert_eq!(
            clean_and_lex_from_str(r#"11 /* foo */ 13"#).unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new_position(/*0,*/ 13, 0, 13),
                    2
                ),
            ]
        );
    }

    #[test]
    fn test_normalize_blanks_commas_and_comments() {
        assert_eq!(
            // test items:
            //
            // unchaged:
            // - comma => comma
            //
            // normalized:
            // - comma + blank(s) => comma
            // - blank(s) + comma => comma
            // - blank(s) + comma + blank(s) => comma
            //
            // inferred:
            // - comma + comment(s) + comma => comma + comma
            // - blank(s) + comment(s) + blank(s) => blank
            //
            // normalization:
            // - blanks => blank
            normalize_and_lex_from_str_without_location(
                r#"
                    [1,2,

                    3

                    ,4

                    ,

                    5
                    ,
                    // comment between commas
                    ,
                    6

                    // comment between blank lines

                    7
                    8
                    ]

                    "#
            )
            .unwrap(),
            vec![
                Token::LeftBracket,
                Token::Number(NumberToken::I32(1)),
                Token::Comma,
                Token::Number(NumberToken::I32(2)),
                Token::Comma,
                Token::Number(NumberToken::I32(3)),
                Token::Comma,
                Token::Number(NumberToken::I32(4)),
                Token::Comma,
                Token::Number(NumberToken::I32(5)),
                Token::Comma,
                Token::Comma,
                Token::Number(NumberToken::I32(6)),
                Token::NewLine,
                Token::Number(NumberToken::I32(7)),
                Token::NewLine,
                Token::Number(NumberToken::I32(8)),
                Token::NewLine,
                Token::RightBracket,
            ]
        );

        // location

        // blanks -> blank
        assert_eq!(
            normalize_and_lex_from_str("11\n \n  \n13").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::NewLine,
                    &Location::new_position(/*0,*/ 2, 0, 2),
                    6
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new_position(/*0,*/ 8, 3, 0),
                    2
                ),
            ]
        );

        // comma + blanks -> comma
        assert_eq!(
            normalize_and_lex_from_str(",\n\n\n11").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 4, 3, 0),
                    2
                ),
            ]
        );

        // blanks + comma -> comma
        assert_eq!(
            normalize_and_lex_from_str("11\n\n\n,").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Location::new_position(/*0,*/ 5, 3, 0),
                    1
                ),
            ]
        );

        // blanks + comma + blanks -> comma
        assert_eq!(
            normalize_and_lex_from_str("11\n\n,\n\n13").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Location::new_position(/*0,*/ 4, 2, 0),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new_position(/*0,*/ 7, 4, 0),
                    2
                ),
            ]
        );

        // comma + comment + comma -> comma + comma
        assert_eq!(
            normalize_and_lex_from_str(",//abc\n,").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Location::new_position(/*0,*/ 7, 1, 0),
                    1
                ),
            ]
        );

        // blanks + comment + blanks -> blank
        assert_eq!(
            normalize_and_lex_from_str("11\n\n//abc\n\n13").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    2
                ),
                TokenWithRange::from_position_and_length(
                    Token::NewLine,
                    &Location::new_position(/*0,*/ 2, 0, 2),
                    9
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(13)),
                    &Location::new_position(/*0,*/ 11, 4, 0),
                    2
                ),
            ]
        );
    }

    #[test]
    fn test_normalize_trim_blanks() {
        assert_eq!(
            normalize_and_lex_from_str_without_location(
                r#"

                11

                13

                "#
            )
            .unwrap(),
            vec![
                Token::Number(NumberToken::I32(11)),
                Token::NewLine,
                Token::Number(NumberToken::I32(13)),
            ]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_decimal_numbers() {
        // implicit type, default int
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+11").unwrap(),
                vec![Token::Number(NumberToken::I32(11))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-13").unwrap(),
                vec![Token::Number(NumberToken::I32(-13_i32 as u32))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+2_147_483_648"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 14
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-2_147_483_649"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 14
                    }
                ))
            ));
        }

        // byte
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+127_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(127))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-128_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-128_i8 as u8))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+128_i8"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 7
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-129_i8"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 7
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-1_u8"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 5
            //         }
            //     ))
            // ));
        }

        // short
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+32767_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(32767))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-32768_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-32768_i16 as u16))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+32768_i16"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 10
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-32769_i16"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 10
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-1_u16"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 6
            //         }
            //     ))
            // ));
        }

        // int
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+2_147_483_647_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(2_147_483_647i32 as u32))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-2_147_483_648_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-2_147_483_648i32 as u32))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+2_147_483_648_i32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 18
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-2_147_483_649_i32"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 18
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-1_u32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 6
            //         }
            //     ))
            // ));
        }

        // long
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+9_223_372_036_854_775_807_i64")
                    .unwrap(),
                vec![Token::Number(NumberToken::I64(
                    9_223_372_036_854_775_807i64 as u64
                )),]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-9_223_372_036_854_775_808_i64")
                    .unwrap(),
                vec![Token::Number(NumberToken::I64(
                    -9_223_372_036_854_775_808i64 as u64
                )),]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+9_223_372_036_854_775_808_i64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 30
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-9_223_372_036_854_775_809_i64"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 30
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-1_u64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 6
            //         }
            //     ))
            // ));
        }

        // location

        {
            assert_eq!(
                normalize_and_lex_from_str("+11").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    3
                ),]
            );

            assert_eq!(
                normalize_and_lex_from_str("-13").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(-13_i32 as u32)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    3
                ),]
            );

            assert_eq!(
                normalize_and_lex_from_str("+11,-13").unwrap(),
                vec![
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(11)),
                        &Location::new_position(/*0,*/ 0, 0, 0),
                        3
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Comma,
                        &Location::new_position(/*0,*/ 3, 0, 3),
                        1
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(-13_i32 as u32)),
                        &Location::new_position(/*0,*/ 4, 0, 4),
                        3
                    ),
                ]
            );
        }

        // +EOF
        assert!(matches!(
            normalize_and_lex_from_str_without_location("abc,+"),
            Err(ParserError::UnexpectedEndOfDocument(_,))
        ));

        // -EOF
        assert!(matches!(
            normalize_and_lex_from_str_without_location("xyz,-"),
            Err(ParserError::UnexpectedEndOfDocument(_,))
        ));

        // err: plus sign is added to non-numbers
        assert!(matches!(
            normalize_and_lex_from_str_without_location("+true"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Location {
                    /*unit: 0,*/
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 5
                }
            ))
        ));

        // err: minus sign is added to non-numbers
        assert!(matches!(
            normalize_and_lex_from_str_without_location("-true"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Location {
                    /*unit: 0,*/
                    index: 0,
                    line: 0,
                    column: 0,
                    length: 5
                }
            ))
        ));
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_numbers() {
        // general
        assert_eq!(
            normalize_and_lex_from_str("+3.402_823_5e+38").unwrap(),
            vec![TokenWithRange::from_position_and_length(
                Token::Number(NumberToken::F64(3.402_823_5e38f64)),
                &Location::new_position(/*0,*/ 0, 0, 0),
                16
            )]
        );

        assert_eq!(
            normalize_and_lex_from_str("-3.402_823_5e+38").unwrap(),
            vec![TokenWithRange::from_position_and_length(
                Token::Number(NumberToken::F64(-3.402_823_5e38f64)),
                &Location::new_position(/*0,*/ 0, 0, 0),
                16
            )]
        );

        // 0.0, +0.0, -0.0
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("0.0").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                normalize_and_lex_from_str("+0.0").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    4
                )]
            );

            // +0 == -0
            assert_eq!(
                normalize_and_lex_from_str("-0.0").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    4
                )]
            );
        }

        // NaN
        // {
        //     let t = normalize_and_lex_from_str_without_location("NaN").unwrap();
        //     assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));
        // }

        // Inf
        // {
        //     assert_eq!(
        //         normalize_and_lex_from_str_without_location("Inf").unwrap(),
        //         vec![Token::Number(NumberToken::F64(f64::INFINITY))]
        //     );
        //     assert_eq!(
        //         normalize_and_lex_from_str("+Inf").unwrap(),
        //         vec![TokenWithRange::from_position_and_length(
        //             Token::Number(NumberToken::F64(f64::INFINITY)),
        //             &Location::new_position(/*0,*/ 0, 0, 0),
        //             4
        //         )]
        //     );
        //     assert_eq!(
        //         normalize_and_lex_from_str("-Inf").unwrap(),
        //         vec![TokenWithRange::from_position_and_length(
        //             Token::Number(NumberToken::F64(f64::NEG_INFINITY)),
        //             &Location::new_position(/*0,*/ 0, 0, 0),
        //             4
        //         )]
        //     );
        // }

        // err: +NaN
        // assert!(matches!(
        //     normalize_and_lex_from_str_without_location("+NaN"),
        //     Err(PreprocessError::MessageWithPosition(
        //         _,
        //         Location {
        //             /*unit: 0,*/
        //             index: 0,
        //             line: 0,
        //             column: 0,
        //             length: 4
        //         }
        //     ))
        // ));

        // err: -NaN
        // assert!(matches!(
        //     normalize_and_lex_from_str_without_location("-NaN"),
        //     Err(PreprocessError::MessageWithPosition(
        //         _,
        //         Location {
        //             /*unit: 0,*/
        //             index: 0,
        //             line: 0,
        //             column: 0,
        //             length: 4
        //         }
        //     ))
        // ));
    }

    #[test]
    fn test_normalize_plus_and_minus_floating_point_numbers_with_explicit_type() {
        // single precision, f32
        {
            assert_eq!(
                normalize_and_lex_from_str("+1.602_176_6e-19_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(1.602_176_6e-19f32)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    20
                )]
            );

            assert_eq!(
                normalize_and_lex_from_str("-1.602_176_6e-19_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(-1.602_176_6e-19f32)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    20
                )]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("0_f32").unwrap(),
                vec![Token::Number(NumberToken::F32(0f32))]
            );

            assert_eq!(
                normalize_and_lex_from_str("+0_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(0f32)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    6
                )]
            );

            // +0 == -0
            assert_eq!(
                normalize_and_lex_from_str("-0_f32").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(0f32)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    6
                )]
            );

            // let t = normalize_and_lex_from_str_without_location("NaN_f32").unwrap();
            // assert!(matches!(t[0], Token::Number(NumberToken::F32(v)) if v.is_nan()));
            // assert_eq!(
            //     normalize_and_lex_from_str_without_location("Inf_f32").unwrap(),
            //     vec![Token::Number(NumberToken::F32(f32::INFINITY))]
            // );
            // assert_eq!(
            //     normalize_and_lex_from_str("+Inf_f32").unwrap(),
            //     vec![TokenWithRange::from_position_and_length(
            //         Token::Number(NumberToken::F32(f32::INFINITY)),
            //         &Location::new_position(/*0,*/ 0, 0, 0),
            //         8
            //     )]
            // );
            // assert_eq!(
            //     normalize_and_lex_from_str("-Inf_f32").unwrap(),
            //     vec![TokenWithRange::from_position_and_length(
            //         Token::Number(NumberToken::F32(f32::NEG_INFINITY)),
            //         &Location::new_position(/*0,*/ 0, 0, 0),
            //         8
            //     )]
            // );

            // err: +NaN
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+NaN_f32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));

            // err: -NaN
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-NaN_f32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }

        // double precision, f64
        {
            assert_eq!(
                normalize_and_lex_from_str("+1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(1.797_693_134_862_315_7e308_f64)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    33
                )]
            );

            assert_eq!(
                normalize_and_lex_from_str("-1.797_693_134_862_315_7e+308_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(-1.797_693_134_862_315_7e308_f64)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    33
                )]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("0_f64").unwrap(),
                vec![Token::Number(NumberToken::F64(0f64))]
            );

            assert_eq!(
                normalize_and_lex_from_str("+0_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    6
                )]
            );

            // +0 == -0
            assert_eq!(
                normalize_and_lex_from_str("-0_f64").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(0f64)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    6
                )]
            );

            // let t = normalize_and_lex_from_str_without_location("NaN_f64").unwrap();
            // assert!(matches!(t[0], Token::Number(NumberToken::F64(v)) if v.is_nan()));
            // assert_eq!(
            //     normalize_and_lex_from_str_without_location("Inf_f64").unwrap(),
            //     vec![Token::Number(NumberToken::F64(f64::INFINITY))]
            // );
            // assert_eq!(
            //     normalize_and_lex_from_str("+Inf_f64").unwrap(),
            //     vec![TokenWithRange::from_position_and_length(
            //         Token::Number(NumberToken::F64(f64::INFINITY)),
            //         &Location::new_position(/*0,*/ 0, 0, 0),
            //         8
            //     )]
            // );
            // assert_eq!(
            //     normalize_and_lex_from_str("-Inf_f64").unwrap(),
            //     vec![TokenWithRange::from_position_and_length(
            //         Token::Number(NumberToken::F64(f64::NEG_INFINITY)),
            //         &Location::new_position(/*0,*/ 0, 0, 0),
            //         8
            //     )]
            // );

            // err: +NaN
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+NaN_f64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));

            // err: -NaN
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-NaN_f64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_hex_numbers() {
        // implicit type, default int
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0x11").unwrap(),
                vec![Token::Number(NumberToken::I32(0x11))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0x13").unwrap(),
                vec![Token::Number(NumberToken::I32(-0x13_i32 as u32))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0x8000_0000"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 12
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0x8000_0001"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 12
                    }
                ))
            ));
        }

        // byte
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0x7f_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0x80_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-0x80_i8 as u8))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0x80_i8"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0x81_i8"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 8
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0x1_u8"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 7
            //         }
            //     ))
            // ));
        }

        // short
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0x7fff_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0x8000_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-0x8000_i16 as u16))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0x8000_i16"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 11
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0x8001_i16"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 11
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0x1_u16"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }

        // int
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0x7fff_ffff_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0x8000_0000_i32").unwrap(),
                vec![Token::Number(NumberToken::I32(-0x8000_0000_i32 as u32))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0x8000_0000_i32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 16
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0x8000_0001_i32"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 16
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0x1_u32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }

        // long
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0x7fff_ffff_ffff_ffff_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    0x7fff_ffff_ffff_ffff_i64 as u64
                ))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0x8000_0000_0000_0000_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(
                    -0x8000_0000_0000_0000_i64 as u64
                ))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0x8000_0000_0000_0000_i64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 26
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0x8000_0000_0000_0001_i64"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 26
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0x1_u64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }

        // location

        {
            assert_eq!(
                normalize_and_lex_from_str("+0x11").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(0x11)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    5
                ),]
            );

            assert_eq!(
                normalize_and_lex_from_str("-0x13").unwrap(),
                vec![TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::I32(-0x13_i32 as u32)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    5
                ),]
            );

            assert_eq!(
                normalize_and_lex_from_str("+0x11,-0x13").unwrap(),
                vec![
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(0x11)),
                        &Location::new_position(/*0,*/ 0, 0, 0),
                        5
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Comma,
                        &Location::new_position(/*0,*/ 5, 0, 5),
                        1
                    ),
                    TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(-0x13_i32 as u32)),
                        &Location::new_position(/*0,*/ 6, 0, 6),
                        5
                    ),
                ]
            );
        }
    }

    #[test]
    fn test_normalize_plus_and_minus_hex_floating_point_numbers() {
        // 3.1415927f32
        assert_eq!(
            normalize_and_lex_from_str_without_location("+0x1.921fb6p1f32").unwrap(),
            vec![Token::Number(NumberToken::F32(std::f32::consts::PI))]
        );

        // -2.718281828459045f64
        assert_eq!(
            normalize_and_lex_from_str_without_location("-0x1.5bf0a8b145769p+1_f64").unwrap(),
            vec![Token::Number(NumberToken::F64(-std::f64::consts::E))]
        );

        // location

        assert_eq!(
            normalize_and_lex_from_str("+0x1.921fb6p1f32,-0x1.5bf0a8b145769p+1_f64").unwrap(),
            vec![
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F32(std::f32::consts::PI)),
                    &Location::new_position(/*0,*/ 0, 0, 0),
                    16
                ),
                TokenWithRange::from_position_and_length(
                    Token::Comma,
                    &Location::new_position(/*0,*/ 16, 0, 16),
                    1
                ),
                TokenWithRange::from_position_and_length(
                    Token::Number(NumberToken::F64(-std::f64::consts::E)),
                    &Location::new_position(/*0,*/ 17, 0, 17),
                    25
                ),
            ]
        );
    }

    // check type range also
    #[test]
    fn test_normalize_plus_and_minus_binary_numbers() {
        // implicit type, default int
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0b101").unwrap(),
                vec![Token::Number(NumberToken::I32(0b101_i32 as u32))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0b010").unwrap(),
                vec![Token::Number(NumberToken::I32(-0b010_i32 as u32))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location(
            //         "+0b1000_0000_0000_0000__0000_0000_0000_0000"
            //     ),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 43
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location(
                    "-0b1000_0000_0000_0000__0000_0000_0000_0001"
                ),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 43
                    }
                ))
            ));
        }

        // byte
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("0b0111_1111_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0b1000_0000_i8").unwrap(),
                vec![Token::Number(NumberToken::I8(-0x80_i8 as u8))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0b1000_0000_i8"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 15
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0b1000_0001_i8"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 15
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0b1_u8"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 7
            //         }
            //     ))
            // ));
        }

        // short
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("+0b0111_1111_1111_1111_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0b1000_0000_0000_0000_i16").unwrap(),
                vec![Token::Number(NumberToken::I16(-0x8000_i16 as u16))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("+0b1000_0000_0000_0000_i16"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 26
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0b1000_0000_0000_0001_i16"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 26
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0b1_u16"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }

        // int
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location(
                    "+0b0111_1111_1111_1111__1111_1111_1111_1111_i32"
                )
                .unwrap(),
                vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location(
                    "-0b1000_0000_0000_0000__0000_0000_0000_0000_i32"
                )
                .unwrap(),
                vec![Token::Number(NumberToken::I32(-0x8000_0000_i32 as u32))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str("+0b1000_0000_0000_0000__0000_0000_0000_0000_i32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 47
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location(
                    "-0b1000_0000_0000_0000__0000_0000_0000_0001_i32"
                ),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 47
                    }
                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0b1_u32"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));
        }

        // long
        {
            assert_eq!(
                normalize_and_lex_from_str_without_location("0b0111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(0x7fff_ffff_ffff_ffff_i64 as u64))]
            );

            assert_eq!(
                normalize_and_lex_from_str_without_location("-0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64").unwrap(),
                vec![Token::Number(NumberToken::I64(-0x8000_0000_0000_0000_i64 as u64))]
            );

            // err: positive overflow
            // assert!(matches!(
            //     normalize_and_lex_from_str("+0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000_i64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 89
            //         }
            //     ))
            // ));

            // err: negative overflow
            assert!(matches!(
                normalize_and_lex_from_str_without_location("-0b1000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0000__0000_0000_0000_0001_i64"),
                Err(PreprocessError::MessageWithPosition(
                    _,
                    Location {
                        /*unit: 0,*/
                        index: 0,
                        line: 0,
                        column: 0,
                        length: 89
                    }

                ))
            ));

            // err: unsigned number with minus sign
            // assert!(matches!(
            //     normalize_and_lex_from_str_without_location("-0b1_u64"),
            //     Err(PreprocessError::MessageWithPosition(
            //         _,
            //         Location {
            //             /*unit: 0,*/
            //             index: 0,
            //             line: 0,
            //             column: 0,
            //             length: 8
            //         }
            //     ))
            // ));

            // location

            {
                assert_eq!(
                    normalize_and_lex_from_str("+0b101").unwrap(),
                    vec![TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(0b101_i32 as u32)),
                        &Location::new_position(/*0,*/ 0, 0, 0),
                        6
                    )]
                );

                assert_eq!(
                    normalize_and_lex_from_str("-0b010").unwrap(),
                    vec![TokenWithRange::from_position_and_length(
                        Token::Number(NumberToken::I32(-0b010_i32 as u32)),
                        &Location::new_position(/*0,*/ 0, 0, 0),
                        6
                    )]
                );

                assert_eq!(
                    normalize_and_lex_from_str("+0b101,-0b010").unwrap(),
                    vec![
                        TokenWithRange::from_position_and_length(
                            Token::Number(NumberToken::I32(0b101_i32 as u32)),
                            &Location::new_position(/*0,*/ 0, 0, 0),
                            6
                        ),
                        TokenWithRange::from_position_and_length(
                            Token::Comma,
                            &Location::new_position(/*0,*/ 6, 0, 6),
                            1
                        ),
                        TokenWithRange::from_position_and_length(
                            Token::Number(NumberToken::I32(-0b010_i32 as u32)),
                            &Location::new_position(/*0,*/ 7, 0, 7),
                            6
                        )
                    ]
                );
            }
        }
    }
}
