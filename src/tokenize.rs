// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::char;

use crate::{
    PreprocessError,
    charwithposition::{CharWithPosition, CharsWithPositionIter},
    peekableiter::PeekableIter,
    position::Position,
    range::Range,
    token::{Token, TokenWithRange},
};

pub const PEEK_BUFFER_MERGE_CONTINUED_LINES: usize = 2;
pub const PEEK_BUFFER_REMOVE_COMMENTS: usize = 2;
pub const PEEK_BUFFER_REMOVE_SHEBANG: usize = 3;
pub const PEEK_BUFFER_TOKENIZE: usize = 4;

// Initial preprocessing steps for tokenization.
// Reference: https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html
//
// 1. (skipped) The input file is loaded into memory and split into lines.
// 2. (not supported) If trigraphs are enabled, they are replaced with their corresponding single characters.
// 3. Lines ending with a backslash ('\') are merged with the following line.
// 4. All comments are replaced by a single space character.
fn pre_tokenize(source_text: &str) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut chars = source_text.chars();
    let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
    let mut peekable_char_position_iter =
        PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_MERGE_CONTINUED_LINES);

    let merged = merge_continued_lines(&mut peekable_char_position_iter)?;
    let mut merged_iter = merged.into_iter();
    let mut peekable_merged_iter = PeekableIter::new(&mut merged_iter, PEEK_BUFFER_REMOVE_COMMENTS);

    let clean = remove_comments(&mut peekable_merged_iter)?;
    let mut clean_iter = clean.into_iter();
    let mut peekable_clean_iter = PeekableIter::new(&mut clean_iter, PEEK_BUFFER_REMOVE_SHEBANG);

    remove_shebang(&mut peekable_clean_iter)
}

/// Merge continued lines in the input text.
/// Lines that end with a backslash ('\') are merged with the following line.
/// It also handles whitespace characters between the backslash and the newline character.
fn merge_continued_lines(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut merged = vec![];
    while let Some(char_with_position) = chars.next() {
        match char_with_position.character {
            '\\' => {
                // Consume any whitespace characters between '\' and the newline ('\n' or "\r\n").
                let mut first_space = None;
                while let Some(next_char_with_position) = chars.peek(0) {
                    if next_char_with_position.character == ' ' {
                        first_space = chars.next(); // Consume the space
                    } else {
                        break;
                    }
                }

                if matches!(
                    chars.peek(0),
                    Some(CharWithPosition {
                        character: '\r',
                        ..
                    })
                ) && matches!(
                    chars.peek(1),
                    Some(CharWithPosition {
                        character: '\n',
                        ..
                    })
                ) {
                    // Found a line continuation with "\...\r\n"
                    chars.next(); // Consume '\r'
                    chars.next(); // Consume '\n'
                } else if matches!(
                    chars.peek(0),
                    Some(CharWithPosition {
                        character: '\n',
                        ..
                    })
                ) {
                    // Found a line continuation with "\...\n"
                    chars.next(); // Consume '\n'
                } else {
                    // No line continuation found.
                    // Restore the '\' character and the first space (if any) to the output.
                    merged.push(char_with_position);
                    if let Some(space) = first_space {
                        merged.push(space);
                    }
                }
            }
            '\r' if matches!(
                chars.peek(0),
                Some(CharWithPosition {
                    character: '\n',
                    ..
                })
            ) =>
            {
                // Convert Windows-style line ending "\r\n" to a single '\n'
                let newline = chars.next(); // Consume '\n'
                merged.push(newline.unwrap());
            }
            _ => {
                // Leave all other characters unchanged
                merged.push(char_with_position);
            }
        }
    }

    Ok(merged)
}

/// Replace line comments (line comment include the ending newline character)
/// and block comments with a single space character.
fn remove_comments(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut clean = vec![];
    while let Some(char_with_position) = chars.next() {
        match char_with_position.character {
            '/' if matches!(chars.peek(0), Some(CharWithPosition { character: '/', .. })) => {
                chars.next(); // Consume '/'

                // Line comment: consume all characters until the end of the line.
                // Note: This includes the newline character.
                while let Some(next_char_with_position) = chars.next() {
                    if next_char_with_position.character == '\n' {
                        break;
                    }
                }

                // Insert a space in place of the comment.
                clean.push(CharWithPosition {
                    character: ' ',
                    position: char_with_position.position,
                });
            }
            '/' if matches!(chars.peek(0), Some(CharWithPosition { character: '*', .. })) => {
                chars.next(); // Consume '*'

                // Block comment: consume all characters until the closing '*/'.
                let mut found_closing = false;
                while let Some(next_char_with_position) = chars.next() {
                    if next_char_with_position.character == '*'
                        && matches!(chars.peek(0), Some(CharWithPosition { character: '/', .. }))
                    {
                        chars.next(); // Consume '/'

                        found_closing = true;
                        break;
                    }
                }

                if !found_closing {
                    return Err(PreprocessError::UnexpectedEndOfDocument(
                        "Unexpected end of document inside a block comment.".to_owned(),
                    ));
                }

                // Insert a space in place of the comment.
                clean.push(CharWithPosition {
                    character: ' ',
                    position: char_with_position.position,
                });
            }
            _ => clean.push(char_with_position), // Leave all other characters unchanged
        }
    }
    Ok(clean)
}

/// Remove the shebang line (if present) from the beginning of the input.
/// It also removes any leading whitespace characters (includes newline characters)
/// before the document content starts.
fn remove_shebang(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    while let Some(CharWithPosition { character, .. }) = chars.peek(0) {
        if character.is_whitespace() {
            // Skip whitespace characters
            chars.next();
        } else {
            break; // Stop when we reach a non-whitespace character
        }
    }

    if matches!(chars.peek(0), Some(CharWithPosition { character: '#', .. }))
        && matches!(chars.peek(1), Some(CharWithPosition { character: '!', .. }))
    {
        chars.next(); // Consume '#'
        chars.next(); // Consume '!'

        // Consume shebang line
        while let Some(next_char_with_position) = chars.next() {
            if next_char_with_position.character == '\n' {
                break; // Stop at the end of the line
            }
        }
    }

    // removes any leading whitespace characters (includes newline characters)
    // before the document content starts.
    while let Some(CharWithPosition { character, .. }) = chars.peek(0) {
        if character.is_whitespace() {
            // Skip whitespace characters
            chars.next();
        } else {
            break; // Stop when we reach a non-whitespace character
        }
    }

    let pure = chars.collect::<Vec<_>>();
    Ok(pure)
}

pub fn tokenize_from_str(s: &str) -> Result<Vec<TokenWithRange>, PreprocessError> {
    let chars = pre_tokenize(s)?;
    let mut chars_iter = chars.into_iter();
    let mut peekable_char_iter = PeekableIter::new(&mut chars_iter, PEEK_BUFFER_TOKENIZE);
    let mut tokenizer = Tokenizer::new(&mut peekable_char_iter);
    tokenizer.tokenize()
}

struct Tokenizer<'a> {
    upstream: &'a mut PeekableIter<'a, CharWithPosition>,

    // the last position of the character consumed by `next_char()`
    last_position: Position,

    // positions stack.
    // This is used to store the positions of characters when consuming characters continusely.
    // use functions `push_position_into_store` and `pop_position_from_store` to manipulate this stack.
    stored_positions: Vec<Position>,
}

impl<'a> Tokenizer<'a> {
    fn new(upstream: &'a mut PeekableIter<'a, CharWithPosition>) -> Self {
        Self {
            upstream,
            last_position: Position::new(0, 0, 0),
            stored_positions: vec![],
        }
    }

    fn next_char(&mut self) -> Option<char> {
        match self.upstream.next() {
            Some(CharWithPosition {
                character,
                position,
            }) => {
                self.last_position = position;
                Some(character)
            }
            None => None,
        }
    }

    fn peek_char(&self, offset: usize) -> Option<&char> {
        match self.upstream.peek(offset) {
            Some(CharWithPosition { character, .. }) => Some(character),
            None => None,
        }
    }

    fn peek_position(&self, offset: usize) -> Option<&Position> {
        match self.upstream.peek(offset) {
            Some(CharWithPosition { position, .. }) => Some(position),
            None => None,
        }
    }

    fn peek_and_char_equals(&self, offset: usize, expected_char: char) -> bool {
        matches!(
            self.upstream.peek(offset),
            Some(CharWithPosition { character, .. }) if character == &expected_char)
    }

    fn peek_and_char_any_of(&self, offset: usize, expected_chars: &[char]) -> bool {
        matches!(
            self.upstream.peek(offset),
            Some(CharWithPosition { character, .. }) if expected_chars.contains(character))
    }

    /// Save the last position into the stack.
    fn push_last_position_into_store(&mut self) {
        let last_position = self.last_position;
        self.push_position_into_store(&last_position);
    }

    /// Save the current position into the stack.
    /// The current position is ahead of one of the last position.
    /// It equals to `self.peek_position(0)`.
    fn push_peek_position_into_store(&mut self) {
        let position = *self.peek_position(0).unwrap();
        self.push_position_into_store(&position);
    }

    fn push_position_into_store(&mut self, position: &Position) {
        self.stored_positions.push(*position);
    }

    fn pop_position_from_store(&mut self) -> Position {
        self.stored_positions.pop().unwrap()
    }
}

impl Tokenizer<'_> {
    fn tokenize(&mut self) -> Result<Vec<TokenWithRange>, PreprocessError> {
        let mut token_with_ranges = vec![];

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                ' ' | '\t' => {
                    self.next_char(); // Consume whitespace
                }
                '\n' => {
                    self.next_char(); // consume '\n'

                    token_with_ranges.push(TokenWithRange::new(
                        Token::Newline,
                        Range::from_position(&self.last_position),
                    ));
                }
                '\'' => {
                    // char
                    token_with_ranges.push(self.tokenize_char()?);
                }
                '"' => {
                    // string
                    token_with_ranges.push(self.tokenize_string()?);
                }
                //                 ',' => {
                //                     self.next_char(); // consume ','
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::Comma,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 ':' => {
                //                     self.next_char(); // consume ':'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::Colon,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 '=' => {
                //                     self.next_char(); // consume '='
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::Equal,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 '-' if self.peek_char_and_equals(1, '>') => {
                //                     self.push_peek_position_into_store();
                //
                //                     self.next_char(); // consume '-'
                //                     self.next_char(); // consume '>'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::RightArrow,
                //                         &self.pop_position_from_store(),
                //                         2,
                //                     ));
                //                 }
                //                 '-' => {
                //                     self.next_char(); // consume '-'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::Minus,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 '+' => {
                //                     self.next_char(); // consume '+'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::Plus,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 '{' => {
                //                     self.next_char(); // consume '{'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::LeftBrace,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 '}' => {
                //                     self.next_char(); // consume '}'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::RightBrace,
                //                         &self.last_position,
                //                         1,
                //                     ))
                //                 }
                //                 '[' => {
                //                     self.next_char(); // consume '['
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::LeftBracket,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 ']' => {
                //                     self.next_char(); // consume ']'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::RightBracket,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 '(' => {
                //                     self.next_char(); // consume '('
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::LeftParen,
                //                         &self.last_position,
                //                         1,
                //                     ));
                //                 }
                //                 ')' => {
                //                     self.next_char(); // consume ')'
                //
                //                     token_with_ranges.push(TokenWithRange::new(
                //                         Token::RightParen,
                //                         &self.last_position,
                //                         1,
                //                     ))
                //                 }
                //                 '0'..='9' => {
                //                     // number
                //                     token_with_ranges.push(self.lex_number()?);
                //                 }
                //                 'h' if self.peek_char_and_equals(1, '"') => {
                //                     // hex byte data
                //                     token_with_ranges.push(self.lex_hexadecimal_byte_data()?);
                //                 }
                //                 'r' if self.peek_char_and_equals(1, '"') => {
                //                     // raw string
                //                     token_with_ranges.push(self.lex_raw_string()?);
                //                 }
                //                 'r' if self.peek_char_and_equals(1, '#') && self.peek_char_and_equals(2, '"') => {
                //                     // raw string with hash symbol
                //                     token_with_ranges.push(self.lex_raw_string_with_hash_symbol()?);
                //                 }
                //                 '/' if self.peek_char_and_equals(1, '/') => {
                //                     // line comment
                //                     token_with_ranges.push(self.lex_line_comment()?);
                //                 }
                //                 '/' if self.peek_char_and_equals(1, '*') => {
                //                     // block comment
                //                     token_with_ranges.push(self.lex_block_comment()?);
                //                 }
                //                 'a'..='z' | 'A'..='Z' | '_' | '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
                //                     // identifier/name/keyword
                //                     token_with_ranges.push(self.lex_identifier()?);
                //                 }
                current_char => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Unexpected char '{}'.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        Ok(token_with_ranges)
    }

    //     fn consume_shebang(&mut self) {
    //         // #!...\n?  //
    //         // ^^     ^__// to here
    //         // ||________// current char, validated
    //         //
    //         // current char = the character of `iter.upstream.peek(0)``
    //
    //         while let Some(current_char) = self.next_char() {
    //             if current_char == '\n' {
    //                 break;
    //             }
    //         }
    //     }
    //
    //     fn lex_identifier(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // key_nameT  //
    //         // ^       ^__// to here
    //         // |__________// current char, validated
    //         //
    //         // current char = the character of `iter.upstream.peek(0)``
    //         // T = terminator chars || EOF
    //
    //         let mut name_string = String::new();
    //         let mut found_double_colon = false; // to indicate whether the variant separator "::" is found
    //
    //         self.push_peek_position_into_store();
    //
    //         while let Some(current_char) = self.peek_char(0) {
    //             match current_char {
    //                 '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
    //                     name_string.push(*current_char);
    //                     self.next_char(); // consume char
    //                 }
    //                 ':' if self.peek_char_and_equals(1, ':') => {
    //                     found_double_colon = true;
    //                     name_string.push_str("::");
    //                     self.next_char(); // consume the 1st ":"
    //                     self.next_char(); // consume the 2nd ":"
    //                 }
    //                 '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
    //                     // A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than a surrogate code point.
    //                     // This has a fixed numerical definition: code points are in the range 0 to 0x10FFFF,
    //                     // inclusive. Surrogate code points, used by UTF-16, are in the range 0xD800 to 0xDFFF.
    //                     //
    //                     // check out:
    //                     // https://doc.rust-lang.org/std/primitive.char.html
    //                     //
    //                     // CJK chars: '\u{4e00}'..='\u{9fff}'
    //                     // for complete CJK chars, check out Unicode standard
    //                     // Ch. 18.1 Han CJK Unified Ideographs
    //                     //
    //                     // summary:
    //                     // Block Location Comment
    //                     // CJK Unified Ideographs 4E00–9FFF Common
    //                     // CJK Unified Ideographs Extension A 3400–4DBF Rare
    //                     // CJK Unified Ideographs Extension B 20000–2A6DF Rare, historic
    //                     // CJK Unified Ideographs Extension C 2A700–2B73F Rare, historic
    //                     // CJK Unified Ideographs Extension D 2B740–2B81F Uncommon, some in current use
    //                     // CJK Unified Ideographs Extension E 2B820–2CEAF Rare, historic
    //                     // CJK Unified Ideographs Extension F 2CEB0–2EBEF Rare, historic
    //                     // CJK Unified Ideographs Extension G 30000–3134F Rare, historic
    //                     // CJK Unified Ideographs Extension H 31350–323AF Rare, historic
    //                     // CJK Compatibility Ideographs F900–FAFF Duplicates, unifiable variants, corporate characters
    //                     // CJK Compatibility Ideographs Supplement 2F800–2FA1F Unifiable variants
    //                     //
    //                     // https://www.unicode.org/versions/Unicode15.0.0/ch18.pdf
    //                     // https://en.wikipedia.org/wiki/CJK_Unified_Ideographs
    //                     // https://www.unicode.org/versions/Unicode15.0.0/
    //                     //
    //                     // see also
    //                     // https://www.unicode.org/reports/tr31/tr31-37.html
    //
    //                     name_string.push(*current_char);
    //                     self.next_char(); // consume char
    //                 }
    //                 ' ' | '\t' | '\r' | '\n' | ',' | ':' | '=' | '+' | '-' | '{' | '}' | '[' | ']'
    //                 | '(' | ')' | '/' | '"' => {
    //                     // terminator chars
    //                     break;
    //                 }
    //                 _ => {
    //                     return Err(PreprocessError::MessageWithPosition(
    //                         format!("Invalid char '{}' for identifier.", current_char),
    //                         *self.peek_position(0).unwrap(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         let name_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         let token = if found_double_colon {
    //             Token::FullName(name_string)
    //         } else {
    //             match name_string.as_str() {
    //                 "import" | "as" | "from" | "external" | "fn" | "data" | "type" | "pub"
    //                 | "readonly" | "uninit" | "align" | "block" | "when" | "if" | "break"
    //                 | "break_fn" | "recur" | "recur_fn" => Token::Keyword(name_string),
    //                 "i64" | "i32" | "i16" | "i8" | "f64" | "f32" | "byte" => {
    //                     Token::DataTypeName(name_string)
    //                 }
    //                 _ => Token::Name(name_string),
    //             }
    //         };
    //
    //         Ok(TokenWithRange::new(token, name_range))
    //     }
    //
    //     fn lex_number(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // 123456T  //
    //         // ^     ^__// to here
    //         // |________// current char, validated
    //         //
    //         // T = terminator chars || EOF
    //
    //         if self.peek_char_and_equals(0, '0') && self.peek_char_and_equals(1, 'b') {
    //             // '0b...'
    //             self.lex_number_binary()
    //         } else if self.peek_char_and_equals(0, '0') && self.peek_char_and_equals(1, 'x') {
    //             // '0x...'
    //             self.lex_number_hex()
    //         } else {
    //             // '1234'
    //             // '1.23'
    //             self.lex_number_decimal()
    //         }
    //     }
    //
    //     fn lex_number_decimal(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // 123456T  //
    //         // ^     ^__// to here
    //         // |________// current char, validated
    //         //
    //         // T = terminator chars || EOF
    //
    //         let mut num_string = String::new();
    //         let mut num_type: Option<NumberType> = None; // "_ixx", "_uxx", "_fxx"
    //         let mut found_point = false; // to indicated whether char '.' is found
    //         let mut found_e = false; // to indicated whether char 'e' is found
    //
    //         // samples:
    //         //
    //         // 123
    //         // 3.14
    //         // 2.99e8
    //         // 2.99e+8
    //         // 6.672e-34
    //
    //         self.push_peek_position_into_store();
    //
    //         while let Some(current_char) = self.peek_char(0) {
    //             match current_char {
    //                 '0'..='9' => {
    //                     // valid digits for decimal number
    //                     num_string.push(*current_char);
    //
    //                     self.next_char(); // consume digit
    //                 }
    //                 '_' => {
    //                     self.next_char(); // consume '_'
    //                 }
    //                 '.' if !found_point => {
    //                     found_point = true;
    //                     num_string.push(*current_char);
    //
    //                     self.next_char(); // consume '.'
    //                 }
    //                 'e' if !found_e => {
    //                     found_e = true;
    //
    //                     // 123e45
    //                     // 123e+45
    //                     // 123e-45
    //                     if self.peek_char_and_equals(1, '-') {
    //                         num_string.push_str("e-");
    //                         self.next_char(); // consume 'e'
    //                         self.next_char(); // consume '-'
    //                     } else if self.peek_char_and_equals(1, '+') {
    //                         num_string.push_str("e+");
    //                         self.next_char(); // consume 'e'
    //                         self.next_char(); // consume '+'
    //                     } else {
    //                         num_string.push(*current_char);
    //                         self.next_char(); // consume 'e'
    //                     }
    //                 }
    //                 'i' | 'f' if num_type.is_none() && matches!(self.peek_char(1), Some('0'..='9')) => {
    //                     let nt = self.lex_number_type_suffix()?;
    //                     num_type.replace(nt);
    //                     break;
    //                 }
    //                 ' ' | '\t' | '\r' | '\n' | ',' | ':' | '=' | '+' | '-' | '{' | '}' | '[' | ']'
    //                 | '(' | ')' | '/' | '"' => {
    //                     // terminator chars
    //                     break;
    //                 }
    //                 _ => {
    //                     return Err(PreprocessError::MessageWithPosition(
    //                         format!("Invalid char '{}' for decimal number.", current_char),
    //                         *self.peek_position(0).unwrap(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         // check syntax
    //         if num_string.ends_with('.') {
    //             return Err(PreprocessError::MessageWithPosition(
    //                 "Decimal number can not ends with \".\".".to_owned(),
    //                 self.last_position,
    //             ));
    //         }
    //
    //         if num_string.ends_with('e') {
    //             return Err(PreprocessError::MessageWithPosition(
    //                 "Decimal number can not ends with \"e\".".to_owned(),
    //                 self.last_position,
    //             ));
    //         }
    //
    //         let num_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         let num_token: NumberToken = if let Some(nt) = num_type {
    //             // numbers with explicit type
    //             match nt {
    //                 NumberType::I8 => {
    //                     let v = num_string.parse::<u8>().map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i8 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I8(v)
    //                 }
    //                 NumberType::I16 => {
    //                     let v = num_string.parse::<u16>().map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i16 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I16(v)
    //                 }
    //                 NumberType::I32 => {
    //                     let v = num_string.parse::<u32>().map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i32 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I32(v)
    //                 }
    //                 NumberType::I64 => {
    //                     let v = num_string.parse::<u64>().map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i64 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I64(v)
    //                 }
    //                 NumberType::F32 => {
    //                     let v = num_string.parse::<f32>().map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!(
    //                                 "Can not convert \"{}\" to f32 floating-point number.",
    //                                 num_string
    //                             ),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     // overflow when parsing from string
    //                     if v.is_infinite() {
    //                         return Err(PreprocessError::MessageWithPosition(
    //                             format!("F32 floating point number \"{}\" is overflow.", num_string),
    //                             num_range,
    //                         ));
    //                     }
    //
    //                     NumberToken::F32(v)
    //                 }
    //                 NumberType::F64 => {
    //                     let v = num_string.parse::<f64>().map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!(
    //                                 "Can not convert \"{}\" to f64 floating-point number.",
    //                                 num_string
    //                             ),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     // overflow when parsing from string
    //                     if v.is_infinite() {
    //                         return Err(PreprocessError::MessageWithPosition(
    //                             format!("F64 floating point number \"{}\" is overflow.", num_string),
    //                             num_range,
    //                         ));
    //                     }
    //
    //                     NumberToken::F64(v)
    //                 }
    //             }
    //         } else if found_point || found_e {
    //             // the default floating-point number type is f64
    //
    //             let v = num_string.parse::<f64>().map_err(|_| {
    //                 PreprocessError::MessageWithPosition(
    //                     format!(
    //                         "Can not convert \"{}\" to f64 floating-point number.",
    //                         num_string
    //                     ),
    //                     num_range,
    //                 )
    //             })?;
    //
    //             // overflow when parsing from string
    //             if v.is_infinite() {
    //                 return Err(PreprocessError::MessageWithPosition(
    //                     format!("F64 floating point number \"{}\" is overflow.", num_string),
    //                     num_range,
    //                 ));
    //             }
    //
    //             NumberToken::F64(v)
    //         } else {
    //             // the default integer number type is i32
    //
    //             let v = num_string.parse::<u32>().map_err(|_| {
    //                 PreprocessError::MessageWithPosition(
    //                     format!("Can not convert \"{}\" to i32 integer number.", num_string,),
    //                     num_range,
    //                 )
    //             })?;
    //
    //             NumberToken::I32(v)
    //         };
    //
    //         Ok(TokenWithRange::new(Token::Number(num_token), num_range))
    //     }
    //
    //     fn lex_number_type_suffix(&mut self) -> Result<NumberType, PreprocessError> {
    //         // iddT  //
    //         // ^^ ^__// to here
    //         // ||____// d = 0..9, validated
    //         // |_____// current char, validated
    //         //
    //         // i = i/f
    //         // d = 0..=9
    //         // T = terminator chars || EOF
    //
    //         self.push_peek_position_into_store();
    //
    //         let first_char = self.next_char().unwrap(); // consume char 'i/u/f'
    //
    //         let mut type_name = String::new();
    //         type_name.push(first_char);
    //
    //         while let Some(current_char) = self.peek_char(0) {
    //             match current_char {
    //                 '0'..='9' => {
    //                     // valid char for type name
    //                     type_name.push(*current_char);
    //
    //                     // consume digit
    //                     self.next_char();
    //                 }
    //                 _ => {
    //                     break;
    //                 }
    //             }
    //         }
    //
    //         let type_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         let nt = NumberType::from_str(&type_name)
    //             .map_err(|msg| PreprocessError::MessageWithPosition(msg, type_range))?;
    //
    //         Ok(nt)
    //     }
    //
    //     fn lex_number_hex(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // 0xaabbT  //
    //         // ^^    ^__// to here
    //         // ||_______// validated
    //         // |________// current char, validated
    //         //
    //         // T = terminator chars || EOF
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume '0'
    //         self.next_char(); // consume 'x'
    //
    //         let mut num_string = String::new();
    //         let mut num_type: Option<NumberType> = None; // "_ixx"
    //
    //         let mut found_point: bool = false; // to indicated whether char '.' is found
    //         let mut found_p: bool = false; // to indicated whether char 'p' is found
    //
    //         while let Some(current_char) = self.peek_char(0) {
    //             match current_char {
    //                 'f' if num_type.is_none()
    //                     && found_p
    //                     && matches!(self.peek_char(1), Some('0'..='9')) =>
    //                 {
    //                     // 'f' is allowed only in the hex floating point literal mode, (i.e. the
    //                     //  character 'p' should be detected first)
    //                     let nt = self.lex_number_type_suffix()?;
    //                     num_type.replace(nt);
    //                     break;
    //                 }
    //                 '0'..='9' | 'a'..='f' | 'A'..='F' => {
    //                     // valid digits for hex number
    //                     num_string.push(*current_char);
    //
    //                     self.next_char(); // consume digit
    //                 }
    //                 '_' => {
    //                     self.next_char(); // consume '_'
    //                 }
    //                 '.' if !found_point && !found_p => {
    //                     // going to be hex floating point literal mode
    //                     found_point = true;
    //
    //                     num_string.push(*current_char);
    //
    //                     self.next_char(); // consume '.'
    //                 }
    //                 'p' | 'P' if !found_p => {
    //                     // hex floating point literal mode
    //                     found_p = true;
    //
    //                     // 0x0.123p45
    //                     // 0x0.123p+45
    //                     // 0x0.123p-45
    //                     if self.peek_char_and_equals(1, '-') {
    //                         num_string.push_str("p-");
    //                         self.next_char(); // consume 'p'
    //                         self.next_char(); // consume '-'
    //                     } else if self.peek_char_and_equals(1, '+') {
    //                         num_string.push_str("p+");
    //                         self.next_char(); // consume 'p'
    //                         self.next_char(); // consume '+'
    //                     } else {
    //                         num_string.push(*current_char);
    //                         self.next_char(); // consume 'p'
    //                     }
    //                 }
    //                 'i' if num_type.is_none()
    //                     && !found_point
    //                     && !found_p
    //                     && matches!(self.peek_char(1), Some('0'..='9')) =>
    //                 {
    //                     // only 'i' and 'u' are allowed for hexadecimal integer numbers,
    //                     // and 'f' is a ordinary hex digit.
    //                     let nt = self.lex_number_type_suffix()?;
    //                     num_type.replace(nt);
    //
    //                     break;
    //                 }
    //                 ' ' | '\t' | '\r' | '\n' | ',' | ':' | '=' | '+' | '-' | '{' | '}' | '[' | ']'
    //                 | '(' | ')' | '/' | '"' => {
    //                     // terminator chars
    //                     break;
    //                 }
    //                 _ => {
    //                     return Err(PreprocessError::MessageWithPosition(
    //                         format!("Invalid char '{}' for hexadecimal number.", current_char),
    //                         *self.peek_position(0).unwrap(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         let num_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         if num_string.is_empty() {
    //             return Err(PreprocessError::MessageWithPosition(
    //                 "Empty hexadecimal number".to_owned(),
    //                 num_range,
    //             ));
    //         }
    //
    //         if found_point && !found_p {
    //             return Err(PreprocessError::MessageWithPosition(
    //                 format!(
    //                     "Hexadecimal floating point number \"{}\" is missing the exponent.",
    //                     num_string
    //                 ),
    //                 num_range,
    //             ));
    //         }
    //
    //         let num_token = if found_p {
    //             // the default type for floating-point is f64
    //             let mut to_f64 = true;
    //
    //             if let Some(nt) = num_type {
    //                 match nt {
    //                     NumberType::F32 => {
    //                         to_f64 = false;
    //                     }
    //                     NumberType::F64 => {
    //                         to_f64 = true;
    //                     }
    //                     _ => {
    //                         return Err(PreprocessError::MessageWithPosition(format!(
    //                                 "Invalid type \"{}\" for hexadecimal floating-point numbers, only type \"f32\" and \"f64\" are allowed.",
    //                                 nt
    //                             ),
    //                             num_range
    //                         ));
    //                     }
    //                 }
    //             };
    //
    //             num_string.insert_str(0, "0x");
    //
    //             if to_f64 {
    //                 let v = hexfloat2::parse::<f64>(&num_string).map_err(|_| {
    //                     // there is no detail message provided by `hexfloat2::parse`.
    //                     PreprocessError::MessageWithPosition(
    //                         format!(
    //                             "Can not convert \"{}\" to f64 floating-point number.",
    //                             num_string
    //                         ),
    //                         num_range,
    //                     )
    //                 })?;
    //
    //                 NumberToken::F64(v)
    //             } else {
    //                 let v = hexfloat2::parse::<f32>(&num_string).map_err(|_| {
    //                     // there is no detail message provided by `hexfloat2::parse`.
    //                     PreprocessError::MessageWithPosition(
    //                         format!(
    //                             "Can not convert \"{}\" to f32 floating-point number.",
    //                             num_string
    //                         ),
    //                         num_range,
    //                     )
    //                 })?;
    //
    //                 NumberToken::F32(v)
    //             }
    //         } else if let Some(nt) = num_type {
    //             match nt {
    //                 NumberType::I8 => {
    //                     let v = u8::from_str_radix(&num_string, 16).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i8 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I8(v)
    //                 }
    //                 NumberType::I16 => {
    //                     let v = u16::from_str_radix(&num_string, 16).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i16 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I16(v)
    //                 }
    //                 NumberType::I32 => {
    //                     let v = u32::from_str_radix(&num_string, 16).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i32 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I32(v)
    //                 }
    //                 NumberType::I64 => {
    //                     let v = u64::from_str_radix(&num_string, 16).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i64 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I64(v)
    //                 }
    //                 NumberType::F32 | NumberType::F64 => {
    //                     // '0x..f32' and '0x..f64' would only be parsed
    //                     // as ordinary hex digits
    //                     unreachable!()
    //                 }
    //             }
    //         } else {
    //             // default
    //             // convert to i32
    //             let v = u32::from_str_radix(&num_string, 16).map_err(|_| {
    //                 PreprocessError::MessageWithPosition(
    //                     format!("Can not convert \"{}\" to i32 integer number.", num_string),
    //                     num_range,
    //                 )
    //             })?;
    //
    //             NumberToken::I32(v)
    //         };
    //
    //         Ok(TokenWithRange::new(Token::Number(num_token), num_range))
    //     }
    //
    //     fn lex_number_binary(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // 0b1010T  //
    //         // ^^    ^__// to here
    //         // ||_______// validated
    //         // |________// current char, validated
    //         //
    //         // T = terminator chars || EOF
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume '0'
    //         self.next_char(); // consume 'b'
    //
    //         let mut num_string = String::new();
    //         let mut num_type: Option<NumberType> = None;
    //
    //         while let Some(current_char) = self.peek_char(0) {
    //             match current_char {
    //                 '0' | '1' => {
    //                     // valid digits for binary number
    //                     num_string.push(*current_char);
    //
    //                     self.next_char(); // consume digit
    //                 }
    //                 '_' => {
    //                     self.next_char(); // consume '_'
    //                 }
    //                 // binary form only supports integer numbers, does not support floating-point numbers
    //                 'i' if num_type.is_none() && matches!(self.peek_char(1), Some('0'..='9')) => {
    //                     let nt = self.lex_number_type_suffix()?;
    //                     num_type.replace(nt);
    //                     break;
    //                 }
    //                 ' ' | '\t' | '\r' | '\n' | ',' | ':' | '=' | '+' | '-' | '{' | '}' | '[' | ']'
    //                 | '(' | ')' | '/' | '"' => {
    //                     // terminator chars
    //                     break;
    //                 }
    //                 _ => {
    //                     return Err(PreprocessError::MessageWithPosition(
    //                         format!("Invalid char '{}' for binary number.", current_char),
    //                         *self.peek_position(0).unwrap(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         let num_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         if num_string.is_empty() {
    //             return Err(PreprocessError::MessageWithPosition(
    //                 "Empty binary number.".to_owned(),
    //                 num_range,
    //             ));
    //         }
    //
    //         let num_token = if let Some(nt) = num_type {
    //             match nt {
    //                 NumberType::I8 => {
    //                     let v = u8::from_str_radix(&num_string, 2).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i8 integer number.", num_string,),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I8(v)
    //                 }
    //                 NumberType::I16 => {
    //                     let v = u16::from_str_radix(&num_string, 2).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i16 integer number.", num_string,),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I16(v)
    //                 }
    //                 NumberType::I32 => {
    //                     let v = u32::from_str_radix(&num_string, 2).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i32 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I32(v)
    //                 }
    //                 NumberType::I64 => {
    //                     let v = u64::from_str_radix(&num_string, 2).map_err(|_| {
    //                         PreprocessError::MessageWithPosition(
    //                             format!("Can not convert \"{}\" to i64 integer number.", num_string),
    //                             num_range,
    //                         )
    //                     })?;
    //
    //                     NumberToken::I64(v)
    //                 }
    //                 NumberType::F32 | NumberType::F64 => {
    //                     unreachable!()
    //                 }
    //             }
    //         } else {
    //             // default
    //             // convert to i32
    //
    //             let v = u32::from_str_radix(&num_string, 2).map_err(|_| {
    //                 PreprocessError::MessageWithPosition(
    //                     format!("Can not convert \"{}\" to i32 integer number.", num_string),
    //                     num_range,
    //                 )
    //             })?;
    //
    //             NumberToken::I32(v)
    //         };
    //
    //         Ok(TokenWithRange::new(Token::Number(num_token), num_range))
    //     }
    //
    fn tokenize_char(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // 'a'?  //
        // ^  ^__// to here
        // |_____// current char, validated

        // save the start position of the char literal (i.e. the first "'")
        self.push_peek_position_into_store();

        self.next_char(); // Consumes "'"

        let character = match self.next_char() {
            Some(current_char) => {
                match current_char {
                    '\\' => {
                        // save the start position of the escape sequence (i.e. the "\" char)
                        self.push_last_position_into_store();

                        // escape chars.
                        // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Escape_sequences
                        let escaped_char = match self.next_char() {
                            Some(current_char2) => {
                                match current_char2 {
                                    'a' => 7u8 as char,  // bell (BEL, ascii 7)
                                    'b' => 8u8 as char,  // backspace (BS, ascii 8)
                                    'e' => 27u8 as char, // escape character (ESC, ascii 27){
                                    'f' => 12u8 as char, // form feed (FF, ascii 12)
                                    'n' => '\n', // new line character (line feed, LF, ascii 10)
                                    'r' => '\r', // carriage return (CR, ascii 13)
                                    't' => '\t', // horizontal tabulation (HT, ascii 9)
                                    'v' => 11u8 as char, // vertical tabulation (VT, ascii 11)
                                    '\\' => '\\', // backslash
                                    '\'' => '\'', // single quote
                                    '"' => '"',  // double quote
                                    '?' => {
                                        // question mark (?)
                                        return Err(PreprocessError::MessageWithRange(
                                                "Escape character '\\?' is not supported since trigraphs are not supported.".to_owned(),
                                                Range::new(
                                                    &self.pop_position_from_store(),
                                                    &self.last_position,
                                                ),
                                            ));
                                    }
                                    '0'..'9' => {
                                        // Octal escape sequence.
                                        // format: `\o`, `\oo`, and `\ooo`.
                                        // e.g. `\0`, `\70` (`'8'`), `\101` (`'A'`)
                                        // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Octal

                                        const MAX_OCTAL_DIGITS: usize = 3; // max 3 octal digits

                                        let mut buffer = String::new();
                                        buffer.push(current_char2);

                                        while let Some(next_char) = self.peek_char(0) {
                                            if next_char.is_ascii_digit() {
                                                buffer.push(*next_char);
                                                self.next_char(); // consume digit

                                                if buffer.len() >= MAX_OCTAL_DIGITS {
                                                    break;
                                                }
                                            } else {
                                                break;
                                            }
                                        }

                                        // convert octal escape sequence to char
                                        match u8::from_str_radix(&buffer, 8) {
                                            Ok(octal_value) => {
                                                // convert to char
                                                octal_value as char
                                            }
                                            Err(_) => {
                                                return Err(PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Invalid octal escape sequence '\\{}'.",
                                                        buffer
                                                    ),
                                                    Range::new(
                                                        &self.pop_position_from_store(),
                                                        &self.last_position,
                                                    ),
                                                ));
                                            }
                                        }
                                    }
                                    'u' | 'U' => {
                                        // - Unicode code point below 10000.
                                        //   format: `\uhhhh`.
                                        // - code points located at U+10000 or higher.
                                        //   format: `\Uhhhhhhhh`.
                                        //
                                        // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Universal_character_names
                                        let length = if current_char2 == 'u' {
                                            4 // 4 hex digits
                                        } else {
                                            8 // 8 hex digits
                                        };

                                        let mut buffer = String::new();

                                        while let Some(next_char) = self.peek_char(0) {
                                            if next_char.is_ascii_hexdigit() {
                                                buffer.push(*next_char);
                                                self.next_char(); // consume hex digit

                                                if buffer.len() >= length {
                                                    // reached the required length
                                                    break;
                                                }
                                            } else {
                                                break;
                                            }
                                        }

                                        if buffer.len() != length {
                                            return Err(PreprocessError::MessageWithRange(
                                                format!(
                                                    "Invalid unicode escape sequence '\\{}{}'. Expected {} hex digits.",
                                                    current_char2, buffer, length
                                                ),
                                                Range::new(
                                                    &self.pop_position_from_store(),
                                                    &self.last_position,
                                                ),
                                            ));
                                        }

                                        // convert hex escape sequence to char
                                        match u32::from_str_radix(&buffer, 16) {
                                            Ok(codepoint) => {
                                                if let Some(c) = char::from_u32(codepoint) {
                                                    c // valid code point
                                                } else {
                                                    return Err(PreprocessError::MessageWithRange(
                                                        format!(
                                                            "Invalid unicode code point '\\{}{}'.",
                                                            current_char2, buffer
                                                        ),
                                                        Range::new(
                                                            &self.pop_position_from_store(),
                                                            &self.last_position,
                                                        ),
                                                    ));
                                                }
                                            }
                                            Err(_) => {
                                                return Err(PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Invalid unicode escape sequence '\\{}{}'.",
                                                        current_char2, buffer
                                                    ),
                                                    Range::new(
                                                        &self.pop_position_from_store(),
                                                        &self.last_position,
                                                    ),
                                                ));
                                            }
                                        }
                                    }
                                    'x' => {
                                        // Hexadecimal escape sequence,
                                        // format: `\xhh`.
                                        // e.g. `\x38` (`'8'`), `\x41` (`'A'`)
                                        // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Hex

                                        const HEX_DIGITS: usize = 2; // only format `\xhh` is supported

                                        let mut buffer = String::new();

                                        while let Some(next_char) = self.peek_char(0) {
                                            if next_char.is_ascii_hexdigit() {
                                                buffer.push(*next_char);
                                                self.next_char(); // consume digit

                                                if buffer.len() >= HEX_DIGITS {
                                                    break;
                                                }
                                            } else {
                                                break;
                                            }
                                        }

                                        if buffer.len() != HEX_DIGITS {
                                            return Err(PreprocessError::MessageWithRange(
                                                format!(
                                                    "Invalid hex escape sequence '\\x{}'. Expected {} hex digits.",
                                                    buffer, HEX_DIGITS
                                                ),
                                                Range::new(
                                                    &self.pop_position_from_store(),
                                                    &self.last_position,
                                                ),
                                            ));
                                        }

                                        // convert hex escape sequence to char
                                        match u8::from_str_radix(&buffer, 16) {
                                            Ok(hex_value) => hex_value as char,
                                            Err(_) => {
                                                return Err(PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Invalid hex escape sequence '\\x{}'.",
                                                        buffer
                                                    ),
                                                    Range::new(
                                                        &self.pop_position_from_store(),
                                                        &self.last_position,
                                                    ),
                                                ));
                                            }
                                        }
                                    }
                                    _ => {
                                        // Unexpected escape char.
                                        return Err(PreprocessError::MessageWithPosition(
                                            format!(
                                                "Invalid escape character '{}'.",
                                                current_char2
                                            ),
                                            self.last_position,
                                        ));
                                    }
                                }
                            }
                            None => {
                                // `\` + EOF
                                return Err(PreprocessError::UnexpectedEndOfDocument(
                                    "Incomplete escape character sequence.".to_owned(),
                                ));
                            }
                        };

                        // discard the saved position of the escape sequence
                        self.pop_position_from_store();

                        // return the escaped character
                        escaped_char
                    }
                    '\'' => {
                        // Empty char (`''`).
                        return Err(PreprocessError::MessageWithRange(
                            "Empty character is not allowed.".to_owned(),
                            Range::new(&self.pop_position_from_store(), &self.last_position),
                        ));
                    }
                    _ => {
                        // ordinary char
                        current_char
                    }
                }
            }
            None => {
                // Incomplete char literal (`'EOF`).
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Incomplete character literal.".to_owned(),
                ));
            }
        };

        // consumes the closing single quote
        match self.next_char() {
            Some('\'') => {
                // Ok
            }
            Some(_) => {
                // missing closing single quote for char (`'a?`).
                return Err(PreprocessError::MessageWithPosition(
                    "Missing closing single quote for character literal.".to_owned(),
                    self.last_position,
                ));
            }
            None => {
                // Incomplete char literal (`'aEOF`).
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Incomplete character literal.".to_owned(),
                ));
            }
        }

        let character_range = Range::new(&self.pop_position_from_store(), &self.last_position);
        Ok(TokenWithRange::new(Token::Char(character), character_range))
    }

    fn tokenize_string(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // "abc"?  //
        // ^    ^__// to here
        // |_______// current char, validated

        // save the start position of the string literal (i.e. the first '"')
        self.push_peek_position_into_store();

        self.next_char(); // Consumes '"'

        let mut final_string = String::new();

        loop {
            match self.next_char() {
                Some(current_char) => {
                    match current_char {
                        '\\' => {
                            // save the start position of the escape sequence (i.e. the "\" char)
                            self.push_last_position_into_store();

                            // escape chars.
                            // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Escape_sequences
                            let escaped_char = match self.next_char() {
                                Some(current_char2) => {
                                    match current_char2 {
                                        'a' => 7u8 as char,  // bell (BEL, ascii 7)
                                        'b' => 8u8 as char,  // backspace (BS, ascii 8)
                                        'e' => 27u8 as char, // escape character (ESC, ascii 27){
                                        'f' => 12u8 as char, // form feed (FF, ascii 12)
                                        'n' => '\n', // new line character (line feed, LF, ascii 10)
                                        'r' => '\r', // carriage return (CR, ascii 13)
                                        't' => '\t', // horizontal tabulation (HT, ascii 9)
                                        'v' => 11u8 as char, // vertical tabulation (VT, ascii 11)
                                        '\\' => '\\', // backslash
                                        '\'' => '\'', // single quote
                                        '"' => '"',  // double quote
                                        '?' => {
                                            // question mark (?)
                                            return Err(PreprocessError::MessageWithRange(
                                                    "Escape character '\\?' is not supported since trigraphs are not supported.".to_owned(),
                                                    Range::new(
                                                    &self.pop_position_from_store(),
                                                    &self.last_position,
                                                ),
                                                ));
                                        }
                                        '0'..'9' => {
                                            // Octal escape sequence.
                                            // format: `\o`, `\oo`, and `\ooo`.
                                            // e.g. `\0`, `\70` (`'8'`), `\101` (`'A'`)
                                            // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Octal

                                            const MAX_OCTAL_DIGITS: usize = 3; // max 3 octal digits

                                            let mut buffer = String::new();
                                            buffer.push(current_char2);

                                            while let Some(next_char) = self.peek_char(0) {
                                                if next_char.is_ascii_digit() {
                                                    buffer.push(*next_char);
                                                    self.next_char(); // consume digit

                                                    if buffer.len() >= MAX_OCTAL_DIGITS {
                                                        break;
                                                    }
                                                } else {
                                                    break;
                                                }
                                            }

                                            // convert octal escape sequence to char
                                            match u8::from_str_radix(&buffer, 8) {
                                                Ok(octal_value) => {
                                                    // convert to char
                                                    octal_value as char
                                                }
                                                Err(_) => {
                                                    return Err(PreprocessError::MessageWithRange(
                                                        format!(
                                                            "Invalid octal escape sequence '\\{}'.",
                                                            buffer
                                                        ),
                                                        Range::new(
                                                            &self.pop_position_from_store(),
                                                            &self.last_position,
                                                        ),
                                                    ));
                                                }
                                            }
                                        }
                                        'u' | 'U' => {
                                            // - Unicode code point below 10000.
                                            //   format: `\uhhhh`.
                                            // - code points located at U+10000 or higher.
                                            //   format: `\Uhhhhhhhh`.
                                            //
                                            // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Universal_character_names
                                            let length = if current_char2 == 'u' {
                                                4 // 4 hex digits
                                            } else {
                                                8 // 8 hex digits
                                            };

                                            let mut buffer = String::new();

                                            while let Some(next_char) = self.peek_char(0) {
                                                if next_char.is_ascii_hexdigit() {
                                                    buffer.push(*next_char);
                                                    self.next_char(); // consume hex digit

                                                    if buffer.len() >= length {
                                                        // reached the required length
                                                        break;
                                                    }
                                                } else {
                                                    break;
                                                }
                                            }

                                            if buffer.len() != length {
                                                return Err(PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Invalid unicode escape sequence '\\{}{}'. Expected {} hex digits.",
                                                        current_char2, buffer, length
                                                    ),
                                                    Range::new(
                                                        &self.pop_position_from_store(),
                                                        &self.last_position,
                                                    ),
                                                ));
                                            }

                                            // convert hex escape sequence to char
                                            match u32::from_str_radix(&buffer, 16) {
                                                Ok(codepoint) => {
                                                    if let Some(c) = char::from_u32(codepoint) {
                                                        c // valid code point
                                                    } else {
                                                        return Err(
                                                            PreprocessError::MessageWithRange(
                                                                format!(
                                                                    "Invalid unicode code point '\\{}{}'.",
                                                                    current_char2, buffer
                                                                ),
                                                                Range::new(
                                                                    &self.pop_position_from_store(),
                                                                    &self.last_position,
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                }
                                                Err(_) => {
                                                    return Err(PreprocessError::MessageWithRange(
                                                        format!(
                                                            "Invalid unicode escape sequence '\\{}{}'.",
                                                            current_char2, buffer
                                                        ),
                                                        Range::new(
                                                            &self.pop_position_from_store(),
                                                            &self.last_position,
                                                        ),
                                                    ));
                                                }
                                            }
                                        }
                                        'x' => {
                                            // Hexadecimal escape sequence,
                                            // format: `\xhh`.
                                            // e.g. `\x38` (`'8'`), `\x41` (`'A'`)
                                            // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Hex

                                            const HEX_DIGITS: usize = 2; // only format `\xhh` is supported

                                            let mut buffer = String::new();

                                            while let Some(next_char) = self.peek_char(0) {
                                                if next_char.is_ascii_hexdigit() {
                                                    buffer.push(*next_char);
                                                    self.next_char(); // consume digit

                                                    if buffer.len() >= HEX_DIGITS {
                                                        break;
                                                    }
                                                } else {
                                                    break;
                                                }
                                            }

                                            if buffer.len() != HEX_DIGITS {
                                                return Err(PreprocessError::MessageWithRange(
                                                    format!(
                                                        "Invalid hex escape sequence '\\x{}'. Expected {} hex digits.",
                                                        buffer, HEX_DIGITS
                                                    ),
                                                    Range::new(
                                                        &self.pop_position_from_store(),
                                                        &self.last_position,
                                                    ),
                                                ));
                                            }

                                            // convert hex escape sequence to char
                                            match u8::from_str_radix(&buffer, 16) {
                                                Ok(hex_value) => hex_value as char,
                                                Err(_) => {
                                                    return Err(PreprocessError::MessageWithRange(
                                                        format!(
                                                            "Invalid hex escape sequence '\\x{}'.",
                                                            buffer
                                                        ),
                                                        Range::new(
                                                            &self.pop_position_from_store(),
                                                            &self.last_position,
                                                        ),
                                                    ));
                                                }
                                            }
                                        }
                                        _ => {
                                            // Unexpected escape char.
                                            return Err(PreprocessError::MessageWithPosition(
                                                format!(
                                                    "Invalid escape character '{}'.",
                                                    current_char2
                                                ),
                                                self.last_position,
                                            ));
                                        }
                                    }
                                }
                                None => {
                                    // `\` + EOF
                                    return Err(PreprocessError::UnexpectedEndOfDocument(
                                        "Incomplete escape character sequence.".to_owned(),
                                    ));
                                }
                            };

                            // discard the saved position of the escape sequence
                            self.pop_position_from_store();

                            final_string.push(escaped_char);
                        }
                        '"' => {
                            // encounter the closing double quote, which
                            // means the end of the string literal.
                            break;
                        }
                        _ => {
                            // ordinary char
                            final_string.push(current_char);
                        }
                    }
                }
                None => {
                    // Incomplete string literal (`"...EOF`).
                    return Err(PreprocessError::UnexpectedEndOfDocument(
                        "Incomplete string literal.".to_owned(),
                    ));
                }
            }
        }

        let final_string_range = Range::new(&self.pop_position_from_store(), &self.last_position);

        Ok(TokenWithRange::new(
            Token::String(final_string),
            final_string_range,
        ))
    }

    //     fn consume_all_leading_whitespaces(&mut self) -> Result<(), PreprocessError> {
    //         // \nssssS  //
    //         //   ^   ^__// to here ('s' = whitespace, 'S' = not whitespace)
    //         //   |______// current char, UNVALIDATED
    //
    //         loop {
    //             match self.peek_char(0) {
    //                 Some(current_char) => {
    //                     match current_char {
    //                         ' ' | '\t' => {
    //                             self.next_char(); // consume ' ' or '\t'
    //                         }
    //                         _ => {
    //                             break;
    //                         }
    //                     }
    //                 }
    //                 None => {
    //                     // EOF
    //                     return Err(PreprocessError::UnexpectedEndOfDocument(
    //                         "Incomplete string.".to_owned(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         Ok(())
    //     }
    //
    //     fn lex_raw_string(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // r"abc"?  //
    //         // ^^    ^__// to here
    //         // ||_______// validated
    //         // |________// current char, validated
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume char 'r'
    //         self.next_char(); // consume the '"'
    //
    //         let mut final_string = String::new();
    //
    //         loop {
    //             match self.next_char() {
    //                 Some(previous_char) => {
    //                     match previous_char {
    //                         '"' => {
    //                             // end of the string
    //                             break;
    //                         }
    //                         _ => {
    //                             // ordinary char
    //                             final_string.push(previous_char);
    //                         }
    //                     }
    //                 }
    //                 None => {
    //                     // `r"...EOF`
    //                     return Err(PreprocessError::UnexpectedEndOfDocument(
    //                         "Incomplete string.".to_owned(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         let final_string_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         Ok(TokenWithRange::new(
    //             Token::String(final_string),
    //             final_string_range,
    //         ))
    //     }
    //
    //     fn lex_raw_string_with_hash_symbol(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // r#"abc"#?  //
    //         // ^^^     ^__// to here
    //         // |||________// validated
    //         // ||_________// validated
    //         // |__________// current char, validated
    //
    //         // hash symbol = '#', i.e. the pound sign
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume 'r'
    //         self.next_char(); // consume '#'
    //         self.next_char(); // consume '"'
    //
    //         let mut final_string = String::new();
    //
    //         loop {
    //             match self.next_char() {
    //                 Some(previous_char) => {
    //                     match previous_char {
    //                         '"' if self.peek_char_and_equals(0, '#') => {
    //                             // it is the end of the string
    //                             self.next_char(); // consume '#'
    //                             break;
    //                         }
    //                         _ => {
    //                             // ordinary char
    //                             final_string.push(previous_char);
    //                         }
    //                     }
    //                 }
    //                 None => {
    //                     // `r#"...EOF`
    //                     return Err(PreprocessError::UnexpectedEndOfDocument(
    //                         "Incomplete string.".to_owned(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         let final_string_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         Ok(TokenWithRange::new(
    //             Token::String(final_string),
    //             final_string_range,
    //         ))
    //     }
    //
    //     fn lex_auto_trimmed_string(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // """\n                    //
    //         // ^^^  auto-trimmed string //
    //         // |||  ...\n               //
    //         // |||  """?                //
    //         // |||     ^________________// to here ('?' = any chars or EOF)
    //         // |||______________________// validated
    //         // ||_______________________// validated
    //         // |________________________// current char, validated
    //
    //         // note:
    //         // - the '\n' of the first line is necessary.
    //         // - the closed `"""` must be started with a new line.
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume the 1st '"'
    //         self.next_char(); // consume the 2nd '"'
    //         self.next_char(); // consume the 3rd '"'
    //
    //         if self.peek_char_and_equals(0, '\n') {
    //             self.next_char(); // consume '\n'
    //         } else if self.peek_char_and_equals(0, '\r') && self.peek_char_and_equals(1, '\n') {
    //             self.next_char(); // consume '\r'
    //             self.next_char(); // consume '\n'
    //         } else {
    //             return Err(PreprocessError::MessageWithPosition(
    //                 "The content of auto-trimmed string should start on a new line.".to_owned(),
    //                 self.last_position.move_position_forward(),
    //             ));
    //         }
    //
    //         let mut lines = vec![]; // String::new();
    //         let mut current_line = vec![]; //String::new();
    //
    //         loop {
    //             match self.next_char() {
    //                 Some(previous_char) => {
    //                     match previous_char {
    //                         '\n' => {
    //                             current_line.push('\n');
    //                             lines.push(current_line);
    //
    //                             current_line = vec![];
    //                         }
    //                         '\r' if self.peek_char_and_equals(0, '\n') => {
    //                             self.next_char(); // consume '\n'
    //
    //                             current_line.push('\r');
    //                             current_line.push('\n');
    //                             lines.push(current_line);
    //
    //                             current_line = vec![];
    //                         }
    //                         '"' if current_line.iter().all(|&c| c == ' ' || c == '\t')
    //                             && self.peek_char_and_equals(0, '"')
    //                             && self.peek_char_and_equals(1, '"') =>
    //                         {
    //                             // it is the end of string
    //                             self.next_char(); // consume '"'
    //                             self.next_char(); // consume '"'
    //                             break;
    //                         }
    //                         _ => {
    //                             // ordinary char
    //                             current_line.push(previous_char);
    //                         }
    //                     }
    //                 }
    //                 None => {
    //                     // `"""\n...EOF`
    //                     return Err(PreprocessError::UnexpectedEndOfDocument(
    //                         "Incomplete string.".to_owned(),
    //                     ));
    //                 }
    //             }
    //         }
    //
    //         let range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         if lines.is_empty() {
    //             return Ok(TokenWithRange::new(Token::String(String::new()), range));
    //         }
    //
    //         // calculate leading spaces of each line
    //         //
    //         // the empty lines would be excluded
    //         let spaces: Vec<usize> = lines
    //             .iter()
    //             .filter(|line| {
    //                 let is_empty = (line.len() == 1 && line[0] == '\n')
    //                     || (line.len() == 2 && line[0] == '\r' && line[1] == '\n');
    //                 !is_empty
    //             })
    //             .map(|line| {
    //                 let mut count = 0;
    //                 while count < line.len() {
    //                     if !(line[count] == ' ' || line[count] == '\t') {
    //                         break;
    //                     }
    //                     count += 1;
    //                 }
    //                 count
    //             })
    //             .collect();
    //
    //         let min = *spaces.iter().min().unwrap_or(&0);
    //
    //         // trim leading spaces
    //         lines
    //             .iter_mut()
    //             .filter(|line| {
    //                 let is_empty = (line.len() == 1 && line[0] == '\n')
    //                     || (line.len() == 2 && line[0] == '\r' && line[1] == '\n');
    //                 !is_empty
    //             })
    //             .for_each(|line| {
    //                 line.drain(0..min);
    //             });
    //
    //         // trim the ending '\n' or "\r\n"
    //         let last_index = lines.len() - 1;
    //         let last_line = &mut lines[last_index];
    //         if matches!(last_line.last(), Some('\n')) {
    //             last_line.pop();
    //         }
    //         if matches!(last_line.last(), Some('\r')) {
    //             last_line.pop();
    //         }
    //
    //         let content = lines
    //             .iter()
    //             .map(|line| line.iter().collect::<String>())
    //             .collect::<Vec<String>>()
    //             .join("");
    //
    //         Ok(TokenWithRange::new(Token::String(content), range))
    //     }
    //
    //     fn lex_hexadecimal_byte_data(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // h"00 11 aa bb"?  //
    //         // ^^            ^__// to here
    //         // ||_______________// validated
    //         // |________________// current char, validated
    //
    //         let consume_zero_or_more_whitespaces = |iter: &mut Lexer| -> Result<usize, PreprocessError> {
    //             // exit when encounting non-whitespaces or EOF
    //             let mut amount: usize = 0;
    //
    //             while let Some(' ' | '\t' | '\r' | '\n') = iter.peek_char(0) {
    //                 amount += 1;
    //                 iter.next_char();
    //             }
    //
    //             Ok(amount)
    //         };
    //
    //         let consume_one_or_more_whitespaces = |iter: &mut Lexer| -> Result<usize, PreprocessError> {
    //             let mut amount: usize = 0;
    //
    //             loop {
    //                 match iter.peek_char(0) {
    //                     Some(current_char) => {
    //                         match current_char {
    //                             ' ' | '\t' | '\r' | '\n' => {
    //                                 // consume whitespace
    //                                 iter.next_char();
    //                                 amount += 1;
    //                             }
    //                             _ => {
    //                                 if amount > 0 {
    //                                     break;
    //                                 } else {
    //                                     return Err(PreprocessError::MessageWithPosition(
    //                                             "Expect a whitespace between the hexadecimal byte data digits."
    //                                                 .to_owned(),
    //                                             iter.last_position.move_position_forward()
    //                                         ));
    //                                 }
    //                             }
    //                         }
    //                     }
    //                     None => {
    //                         // h"...EOF
    //                         return Err(PreprocessError::UnexpectedEndOfDocument(
    //                             "Incomplete hexadecimal byte data.".to_owned(),
    //                         ));
    //                     }
    //                 }
    //             }
    //
    //             Ok(amount)
    //         };
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume char 'h'
    //         self.next_char(); // consume quote '"'
    //
    //         let mut bytes: Vec<u8> = Vec::new();
    //         let mut chars: [char; 2] = ['0', '0'];
    //
    //         consume_zero_or_more_whitespaces(self)?;
    //
    //         loop {
    //             if self.peek_char_and_equals(0, '"') {
    //                 break;
    //             }
    //
    //             for c in &mut chars {
    //                 match self.next_char() {
    //                     Some(previous_char) => match previous_char {
    //                         'a'..='f' | 'A'..='F' | '0'..='9' => {
    //                             *c = previous_char;
    //                         }
    //                         _ => {
    //                             return Err(PreprocessError::MessageWithPosition(
    //                                 format!(
    //                                     "Invalid digit '{}' for hexadecimal byte data.",
    //                                     previous_char
    //                                 ),
    //                                 self.last_position,
    //                             ));
    //                         }
    //                     },
    //                     None => {
    //                         return Err(PreprocessError::UnexpectedEndOfDocument(
    //                             "Incomplete hexadecimal byte data.".to_owned(),
    //                         ))
    //                     }
    //                 }
    //             }
    //
    //             let byte_string = String::from_iter(chars);
    //             let byte_number = u8::from_str_radix(&byte_string, 16).unwrap();
    //             bytes.push(byte_number);
    //
    //             if self.peek_char_and_equals(0, '"') {
    //                 break;
    //             }
    //
    //             // consume at lease one whitespace
    //             consume_one_or_more_whitespaces(self)?;
    //         }
    //
    //         self.next_char(); // consume '"'
    //
    //         let bytes_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         Ok(TokenWithRange::new(Token::HexByteData(bytes), bytes_range))
    //     }
    //
    //     fn lex_line_comment(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // xx...[\r]\n?  //
    //         // ^^         ^__// to here ('?' = any char or EOF)
    //         // ||____________// validated
    //         // |_____________// current char, validated
    //         //
    //         // x = '/'
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume the 1st '/'
    //         self.next_char(); // consume the 2nd '/'
    //
    //         let mut comment_string = String::new();
    //
    //         while let Some(current_char) = self.peek_char(0) {
    //             // ignore all chars except '\n' or '\r\n'
    //             // note that the "line comment token" does not include the trailing new line chars (\n or \r\n),
    //
    //             match current_char {
    //                 '\n' => {
    //                     break;
    //                 }
    //                 '\r' if self.peek_char_and_equals(0, '\n') => {
    //                     break;
    //                 }
    //                 _ => {
    //                     comment_string.push(*current_char);
    //
    //                     self.next_char(); // consume char
    //                 }
    //             }
    //         }
    //
    //         let comment_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         Ok(TokenWithRange::new(
    //             Token::Comment(Comment::Line(comment_string)),
    //             comment_range,
    //         ))
    //     }
    //
    //     fn lex_block_comment(&mut self) -> Result<TokenWithRange, PreprocessError> {
    //         // /*...*/?  //
    //         // ^^     ^__// to here
    //         // ||________// validated
    //         // |_________// current char, validated
    //
    //         self.push_peek_position_into_store();
    //
    //         self.next_char(); // consume '/'
    //         self.next_char(); // consume '*'
    //
    //         let mut comment_string = String::new();
    //         let mut depth = 1; // nested depth
    //
    //         loop {
    //             match self.next_char() {
    //                 Some(previous_char) => {
    //                     match previous_char {
    //                         '/' if self.peek_char_and_equals(0, '*') => {
    //                             // nested block comment
    //                             comment_string.push_str("/*");
    //
    //                             self.next_char(); // consume '*'
    //
    //                             // increase depth
    //                             depth += 1;
    //                         }
    //                         '*' if self.peek_char_and_equals(0, '/') => {
    //                             self.next_char(); // consume '/'
    //
    //                             // decrease depth
    //                             depth -= 1;
    //
    //                             // check pairs
    //                             if depth == 0 {
    //                                 break;
    //                             } else {
    //                                 comment_string.push_str("*/");
    //                             }
    //                         }
    //                         _ => {
    //                             // ignore all chars except "/*" and "*/"
    //                             // note that line comments within block comments are ignored also.
    //                             comment_string.push(previous_char);
    //                         }
    //                     }
    //                 }
    //                 None => {
    //                     let msg = if depth > 1 {
    //                         "Incomplete nested block comment.".to_owned()
    //                     } else {
    //                         "Incomplete block comment.".to_owned()
    //                     };
    //
    //                     return Err(PreprocessError::UnexpectedEndOfDocument(msg));
    //                 }
    //             }
    //         }
    //
    //         let comment_range = Location::from_position_pair_with_end_included(
    //             &self.pop_position_from_store(),
    //             &self.last_position,
    //         );
    //
    //         Ok(TokenWithRange::new(
    //             Token::Comment(Comment::Block(comment_string)),
    //             comment_range,
    //         ))
    //     }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        PreprocessError,
        charwithposition::{CharWithPosition, CharsWithPositionIter},
        peekableiter::PeekableIter,
        position::Position,
        range::Range,
        token::{Token, TokenWithRange},
        tokenize::{
            PEEK_BUFFER_MERGE_CONTINUED_LINES, PEEK_BUFFER_REMOVE_COMMENTS,
            PEEK_BUFFER_REMOVE_SHEBANG, pre_tokenize,
        },
    };

    use super::{merge_continued_lines, remove_comments, remove_shebang, tokenize_from_str};

    impl Token {
        pub fn new_identifier(s: &str) -> Self {
            Token::Identifier(s.to_owned())
        }

        pub fn new_number(s: &str) -> Self {
            Token::Number(s.to_owned())
        }

        pub fn new_string(s: &str) -> Self {
            Token::String(s.to_owned())
        }
    }

    fn tokenize_from_str_without_range_strip(s: &str) -> Result<Vec<Token>, PreprocessError> {
        let tokens = tokenize_from_str(s)?
            .into_iter()
            .map(|e| e.token)
            .collect::<Vec<Token>>();
        Ok(tokens)
    }

    #[test]
    fn test_merge_continued_lines() {
        let source_text = "012\n456\\\n901\\    \n890\\\r\n456\n890";
        // chars index: 0123 4567 8 9012 34567 8901 2 3 4567 890

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut iter =
            PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_MERGE_CONTINUED_LINES);

        let merged = merge_continued_lines(&mut iter).unwrap();

        assert_eq!(
            merged,
            vec![
                // line 0
                CharWithPosition::new('0', Position::new(0, 0, 0)),
                CharWithPosition::new('1', Position::new(1, 0, 1)),
                CharWithPosition::new('2', Position::new(2, 0, 2)),
                CharWithPosition::new('\n', Position::new(3, 0, 3)),
                // line 1
                CharWithPosition::new('4', Position::new(4, 1, 0)),
                CharWithPosition::new('5', Position::new(5, 1, 1)),
                CharWithPosition::new('6', Position::new(6, 1, 2)),
                // line 2
                CharWithPosition::new('9', Position::new(9, 2, 0)),
                CharWithPosition::new('0', Position::new(10, 2, 1)),
                CharWithPosition::new('1', Position::new(11, 2, 2)),
                // line 3
                CharWithPosition::new('8', Position::new(18, 3, 0)),
                CharWithPosition::new('9', Position::new(19, 3, 1)),
                CharWithPosition::new('0', Position::new(20, 3, 2)),
                // line 4
                CharWithPosition::new('4', Position::new(24, 4, 0)),
                CharWithPosition::new('5', Position::new(25, 4, 1)),
                CharWithPosition::new('6', Position::new(26, 4, 2)),
                CharWithPosition::new('\n', Position::new(27, 4, 3)),
                // line 5
                CharWithPosition::new('8', Position::new(28, 5, 0)),
                CharWithPosition::new('9', Position::new(29, 5, 1)),
                CharWithPosition::new('0', Position::new(30, 5, 2)),
            ]
        );
    }

    #[test]
    fn test_remove_comments() {
        let source_text = "012 // foo\n123 /* bar \n buzz */ 234";
        // chars index: 01234567890 123456789012 345678901234

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut iter = PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_REMOVE_COMMENTS);

        let clean = remove_comments(&mut iter).unwrap();

        assert_eq!(
            clean,
            vec![
                // line 0
                CharWithPosition::new('0', Position::new(0, 0, 0)),
                CharWithPosition::new('1', Position::new(1, 0, 1)),
                CharWithPosition::new('2', Position::new(2, 0, 2)),
                CharWithPosition::new(' ', Position::new(3, 0, 3)),
                CharWithPosition::new(' ', Position::new(4, 0, 4)), // in place of comment
                // line 1
                CharWithPosition::new('1', Position::new(11, 1, 0)),
                CharWithPosition::new('2', Position::new(12, 1, 1)),
                CharWithPosition::new('3', Position::new(13, 1, 2)),
                CharWithPosition::new(' ', Position::new(14, 1, 3)),
                CharWithPosition::new(' ', Position::new(15, 1, 4)), // in place of comment
                // line 2
                CharWithPosition::new(' ', Position::new(31, 2, 8)),
                CharWithPosition::new('2', Position::new(32, 2, 9)),
                CharWithPosition::new('3', Position::new(33, 2, 10)),
                CharWithPosition::new('4', Position::new(34, 2, 11)),
            ]
        );
    }

    #[test]
    fn test_remove_shebang() {
        let source_text = "//foo\n#!/usr/bin/env bar\n567";
        // chars index: 012345 6789012345678901234 567

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut peekable_chars =
            PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_REMOVE_COMMENTS);

        let clean = remove_comments(&mut peekable_chars).unwrap();
        let mut clean_iter = clean.into_iter();
        let mut peekable_clean_iter =
            PeekableIter::new(&mut clean_iter, PEEK_BUFFER_REMOVE_SHEBANG);
        let pure = remove_shebang(&mut peekable_clean_iter).unwrap();

        assert_eq!(
            pure,
            vec![
                // line 3
                CharWithPosition::new('5', Position::new(25, 2, 0)),
                CharWithPosition::new('6', Position::new(26, 2, 1)),
                CharWithPosition::new('7', Position::new(27, 2, 2)),
            ]
        );
    }

    #[test]
    fn test_pre_tokenize() {
        let source_text = "0/\\\n/foo\n1/\\\n*bar*/2";
        // chars index: 012 3 45678 901 2 3456789

        let clean = pre_tokenize(source_text).unwrap();

        assert_eq!(
            clean,
            vec![
                // line 0
                CharWithPosition::new('0', Position::new(0, 0, 0)),
                CharWithPosition::new(' ', Position::new(1, 0, 1)), // in place of comment
                // line 2
                CharWithPosition::new('1', Position::new(9, 2, 0)),
                CharWithPosition::new(' ', Position::new(10, 2, 1)), // in place of comment
                // line 3
                CharWithPosition::new('2', Position::new(19, 3, 6)),
            ]
        );
    }

    #[test]
    fn test_tokenize_whitespaces() {
        //         assert_eq!(tokenize_from_str_without_range_strip("  ").unwrap(), vec![]);
        //
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip("()").unwrap(),
        //             vec![Token::LeftParen, Token::RightParen]
        //         );
        //
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip("(  )").unwrap(),
        //             vec![Token::LeftParen, Token::RightParen]
        //         );
        //
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip("(\t\r\n\n\n)").unwrap(),
        //             vec![
        //                 Token::LeftParen,
        //                 Token::Newline,
        //                 Token::Newline,
        //                 Token::Newline,
        //                 Token::RightParen,
        //             ]
        //         );
        //
        //         // location
        //
        //         assert_eq!(tokenize_from_str("  ").unwrap(), vec![]);
        //
        //         assert_eq!(
        //             tokenize_from_str("()").unwrap(),
        //             vec![
        //                 TokenWithRange::new(Token::LeftParen, Location::new_range(/*0,*/ 0, 0, 0, 1)),
        //                 TokenWithRange::new(Token::RightParen, Location::new_range(/*0,*/ 1, 0, 1, 1)),
        //             ]
        //         );
        //
        //         assert_eq!(
        //             tokenize_from_str("(  )").unwrap(),
        //             vec![
        //                 TokenWithRange::new(Token::LeftParen, Location::new_range(/*0,*/ 0, 0, 0, 1)),
        //                 TokenWithRange::new(Token::RightParen, Location::new_range(/*0,*/ 3, 0, 3, 1)),
        //             ]
        //         );
        //
        //         // "(\t\r\n\n\n)"
        //         //  _--____--__-
        //         //  0  2   4 5 6    // index
        //         //  0  0   1 2 3    // line
        //         //  0  2   0 0 1    // column
        //         //  1  2   1 1 1    // length
        //
        //         assert_eq!(
        //             tokenize_from_str("(\t\r\n\n\n)").unwrap(),
        //             vec![
        //                 TokenWithRange::new(Token::LeftParen, Location::new_range(/*0,*/ 0, 0, 0, 1)),
        //                 TokenWithRange::new(Token::Newline, Location::new_range(/*0,*/ 2, 0, 2, 2,)),
        //                 TokenWithRange::new(Token::Newline, Location::new_range(/*0,*/ 4, 1, 0, 1,)),
        //                 TokenWithRange::new(Token::Newline, Location::new_range(/*0,*/ 5, 2, 0, 1,)),
        //                 TokenWithRange::new(Token::RightParen, Location::new_range(/*0,*/ 6, 3, 0, 1)),
        //             ]
        //         );
    }

    #[test]
    fn test_tokenize_line_comment() {
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip(
        //                 r#"
        //                 7 //11
        //                 13 17// 19 23
        //                 //  29
        //                 31//    37
        //                 "#
        //             )
        //             .unwrap(),
        //             vec![
        //                 Token::Newline,
        //                 Token::Number(NumberToken::I32(7)),
        //                 Token::Comment(Comment::Line("11".to_owned())),
        //                 Token::Newline,
        //                 Token::Number(NumberToken::I32(13)),
        //                 Token::Number(NumberToken::I32(17)),
        //                 Token::Comment(Comment::Line(" 19 23".to_owned())),
        //                 Token::Newline,
        //                 Token::Comment(Comment::Line("  29".to_owned())),
        //                 Token::Newline,
        //                 Token::Number(NumberToken::I32(31)),
        //                 Token::Comment(Comment::Line("    37".to_owned())),
        //                 Token::Newline,
        //             ]
        //         );
        //
        //         // location
        //
        //         assert_eq!(
        //             tokenize_from_str("foo // bar").unwrap(),
        //             vec![
        //                 TokenWithRange::new(
        //                     Token::new_name("foo"),
        //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
        //                     3
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Comment(Comment::Line(" bar".to_owned())),
        //                     Range::from_detail_and_length(/*0,*/ 4, 0, 4),
        //                     6
        //                 ),
        //             ]
        //         );
        //
        //         assert_eq!(
        //             tokenize_from_str("abc // def\n// xyz\n").unwrap(),
        //             vec![
        //                 TokenWithRange::new(
        //                     Token::new_name("abc"),
        //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
        //                     3
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Comment(Comment::Line(" def".to_owned())),
        //                     Range::from_detail_and_length(/*0,*/ 4, 0, 4),
        //                     6
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Newline,
        //                     Range::from_detail_and_length(/*0,*/ 10, 0, 10),
        //                     1
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Comment(Comment::Line(" xyz".to_owned())),
        //                     Range::from_detail_and_length(/*0,*/ 11, 1, 0),
        //                     6
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Newline,
        //                     Range::from_detail_and_length(/*0,*/ 17, 1, 6),
        //                     1
        //                 ),
        //             ]
        //         );
    }

    #[test]
    fn test_tokenize_block_comment() {
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip(
        //                 r#"
        //                 7 /* 11 13 */ 17
        //                 "#
        //             )
        //             .unwrap(),
        //             vec![
        //                 Token::Newline,
        //                 Token::Number(NumberToken::I32(7)),
        //                 Token::Comment(Comment::Block(" 11 13 ".to_owned())),
        //                 Token::Number(NumberToken::I32(17)),
        //                 Token::Newline,
        //             ]
        //         );
        //
        //         // nested block comment
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip(
        //                 r#"
        //                 7 /* 11 /* 13 */ 17 */ 19
        //                 "#
        //             )
        //             .unwrap(),
        //             vec![
        //                 Token::Newline,
        //                 Token::Number(NumberToken::I32(7)),
        //                 Token::Comment(Comment::Block(" 11 /* 13 */ 17 ".to_owned())),
        //                 Token::Number(NumberToken::I32(19)),
        //                 Token::Newline,
        //             ]
        //         );
        //
        //         // line comment chars "//" within the block comment
        //         assert_eq!(
        //             tokenize_from_str_without_range_strip(
        //                 r#"
        //                 7 /* 11 // 13 17 */ 19
        //                 "#
        //             )
        //             .unwrap(),
        //             vec![
        //                 Token::Newline,
        //                 Token::Number(NumberToken::I32(7)),
        //                 Token::Comment(Comment::Block(" 11 // 13 17 ".to_owned())),
        //                 Token::Number(NumberToken::I32(19)),
        //                 Token::Newline,
        //             ]
        //         );
        //
        //         // location
        //
        //         assert_eq!(
        //             tokenize_from_str("foo /* hello */ bar").unwrap(),
        //             vec![
        //                 TokenWithRange::new(
        //                     Token::new_name("foo"),
        //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
        //                     3
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Comment(Comment::Block(" hello ".to_owned())),
        //                     Range::from_detail_and_length(/*0,*/ 4, 0, 4),
        //                     11
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::new_name("bar"),
        //                     Range::from_detail_and_length(/*0,*/ 16, 0, 16),
        //                     3
        //                 ),
        //             ]
        //         );
        //
        //         assert_eq!(
        //             tokenize_from_str("/* abc\nxyz */ /* hello */").unwrap(),
        //             vec![
        //                 TokenWithRange::new(
        //                     Token::Comment(Comment::Block(" abc\nxyz ".to_owned())),
        //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
        //                     13
        //                 ),
        //                 TokenWithRange::new(
        //                     Token::Comment(Comment::Block(" hello ".to_owned())),
        //                     Range::from_detail_and_length(/*0,*/ 14, 1, 7),
        //                     11
        //                 ),
        //             ]
        //         );
        //
        //         // err: incomplete, missing "*/"
        //         assert!(matches!(
        //             tokenize_from_str_without_range_strip("7 /* 11"),
        //             Err(PreprocessError::UnexpectedEndOfDocument(_))
        //         ));
        //
        //         // err: incomplete, missing "*/", ends with \n
        //         assert!(matches!(
        //             tokenize_from_str_without_range_strip("7 /* 11\n"),
        //             Err(PreprocessError::UnexpectedEndOfDocument(_))
        //         ));
        //
        //         // err: incomplete, unpaired, missing "*/"
        //         assert!(matches!(
        //             tokenize_from_str_without_range_strip("a /* b /* c */"),
        //             Err(PreprocessError::UnexpectedEndOfDocument(_))
        //         ));
        //
        //         // err: incomplete, unpaired, missing "*/", ends with \n
        //         assert!(matches!(
        //             tokenize_from_str_without_range_strip("a /* b /* c */\n"),
        //             Err(PreprocessError::UnexpectedEndOfDocument(_))
        //         ));
    }

    #[test]
    fn test_tokenize_shebang() {
        // TODO
    }

    //
    //     #[test]
    //     fn test_lex_punctuations() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("\n,:=->+-{}[]()").unwrap(),
    //             vec![
    //                 Token::Newline,
    //                 Token::Comma,
    //                 Token::Colon,
    //                 Token::Equal,
    //                 Token::RightArrow,
    //                 Token::Plus,
    //                 Token::Minus,
    //                 Token::LeftBrace,
    //                 Token::RightBrace,
    //                 Token::LeftBracket,
    //                 Token::RightBracket,
    //                 Token::LeftParen,
    //                 Token::RightParen,
    //             ]
    //         );
    //
    //         // location
    //
    //         assert_eq!(
    //             tokenize_from_str("-->").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::Minus,
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::RightArrow,
    //                     Range::from_detail_and_length(/*0,*/ 1, 0, 1),
    //                     2
    //                 ),
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_lex_identifier() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("name").unwrap(),
    //             vec![Token::new_name("name")]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("(name)").unwrap(),
    //             vec![Token::LeftParen, Token::new_name("name"), Token::RightParen,]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("( a )").unwrap(),
    //             vec![Token::LeftParen, Token::new_name("a"), Token::RightParen,]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("a__b__c").unwrap(),
    //             vec![Token::new_name("a__b__c")]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("foo bar").unwrap(),
    //             vec![Token::new_name("foo"), Token::new_name("bar")]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("αβγ 文字 🍞🥛").unwrap(),
    //             vec![
    //                 Token::new_name("αβγ"),
    //                 Token::new_name("文字"),
    //                 Token::new_name("🍞🥛"),
    //             ]
    //         );
    //
    //         // location
    //
    //         assert_eq!(
    //             tokenize_from_str("hello ASON").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::new_name("hello"),
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                     5
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_name("ASON"),
    //                     Range::from_detail_and_length(/*0,*/ 6, 0, 6),
    //                     4
    //                 )
    //             ]
    //         );
    //
    //         // err: invalid char
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("abc&xyz"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 3,
    //                     line: 0,
    //                     column: 3,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_full_name() {
    //         assert_eq!(
    //             tokenize_from_str("foo::bar a::bc::def a:b").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::new_full_name("foo::bar"),
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                     8
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_full_name("a::bc::def"),
    //                     Range::from_detail_and_length(/*0,*/ 9, 0, 9),
    //                     10
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_name("a"),
    //                     Range::from_detail_and_length(/*0,*/ 20, 0, 20),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Colon,
    //                     Range::from_detail_and_length(/*0,*/ 21, 0, 21),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_name("b"),
    //                     Range::from_detail_and_length(/*0,*/ 22, 0, 22),
    //                     1
    //                 )
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_lex_keywords() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip(
    //                 "import as from \
    // external fn data type \
    // pub readonly uninit align \
    // block when if \
    // break break_fn \
    // recur recur_fn"
    //             )
    //             .unwrap(),
    //             vec![
    //                 Token::new_keyword("import"),
    //                 Token::new_keyword("as"),
    //                 Token::new_keyword("from"),
    //                 //
    //                 Token::new_keyword("external"),
    //                 Token::new_keyword("fn"),
    //                 Token::new_keyword("data"),
    //                 Token::new_keyword("type"),
    //                 //
    //                 Token::new_keyword("pub"),
    //                 Token::new_keyword("readonly"),
    //                 Token::new_keyword("uninit"),
    //                 Token::new_keyword("align"),
    //                 //
    //                 Token::new_keyword("block"),
    //                 Token::new_keyword("when"),
    //                 Token::new_keyword("if"),
    //                 //
    //                 Token::new_keyword("break"),
    //                 Token::new_keyword("break_fn"),
    //                 Token::new_keyword("recur"),
    //                 Token::new_keyword("recur_fn"),
    //             ]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("i64 i32 i16 i8 f64 f32 byte").unwrap(),
    //             vec![
    //                 Token::new_datatype_name("i64"),
    //                 Token::new_datatype_name("i32"),
    //                 Token::new_datatype_name("i16"),
    //                 Token::new_datatype_name("i8"),
    //                 Token::new_datatype_name("f64"),
    //                 Token::new_datatype_name("f32"),
    //                 Token::new_datatype_name("byte"),
    //             ]
    //         );
    //
    //         // location
    //         assert_eq!(
    //             tokenize_from_str("data foo:i32").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::new_keyword("data"),
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                     4
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_name("foo"),
    //                     Range::from_detail_and_length(/*0,*/ 5, 0, 5),
    //                     3
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Colon,
    //                     Range::from_detail_and_length(/*0,*/ 8, 0, 8),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_datatype_name("i32"),
    //                     Range::from_detail_and_length(/*0,*/ 9, 0, 9),
    //                     3
    //                 ),
    //             ]
    //         );
    //     }
    //
    //     #[test]
    //     fn test_lex_multiline_location() {
    //         // "[\n  pub\n    data\n]"
    //         //  01 234567 890123456 7   // index
    //         //  00 111111 222222222 3   // line
    //         //  01 012345 012345678 0   // column
    //         //  11   3  1     4   1 1   // length
    //
    //         assert_eq!(
    //             tokenize_from_str("[\n  pub\n    data\n]").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::LeftBracket,
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Newline,
    //                     Range::from_detail_and_length(/*0,*/ 1, 0, 1),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_keyword("pub"),
    //                     Range::from_detail_and_length(/*0,*/ 4, 1, 2),
    //                     3
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Newline,
    //                     Range::from_detail_and_length(/*0,*/ 7, 1, 5),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::new_keyword("data"),
    //                     Range::from_detail_and_length(/*0,*/ 12, 2, 4),
    //                     4
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Newline,
    //                     Range::from_detail_and_length(/*0,*/ 16, 2, 8),
    //                     1
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::RightBracket,
    //                     Range::from_detail_and_length(/*0,*/ 17, 3, 0),
    //                     1
    //                 ),
    //             ]
    //         )
    //     }
    //
    //     #[test]
    //     fn test_lex_decimal_number() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("(211)").unwrap(),
    //             vec![
    //                 Token::LeftParen,
    //                 Token::Number(NumberToken::I32(211)),
    //                 Token::RightParen,
    //             ]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("211").unwrap(),
    //             vec![Token::Number(NumberToken::I32(211))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("-2017").unwrap(),
    //             vec![Token::Minus, Token::Number(NumberToken::I32(2017))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("+2024").unwrap(),
    //             vec![Token::Plus, Token::Number(NumberToken::I32(2024))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("223_211").unwrap(),
    //             vec![Token::Number(NumberToken::I32(223_211))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("223 211").unwrap(),
    //             vec![
    //                 Token::Number(NumberToken::I32(223)),
    //                 Token::Number(NumberToken::I32(211)),
    //             ]
    //         );
    //
    //         // location
    //
    //         assert_eq!(
    //             tokenize_from_str("223 211").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::Number(NumberToken::I32(223)),
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0,),
    //                     3
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Number(NumberToken::I32(211)),
    //                     Range::from_detail_and_length(/*0,*/ 4, 0, 4,),
    //                     3
    //                 ),
    //             ]
    //         );
    //
    //         // err: invalid char for decimal number
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("12x34"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 2,
    //                     line: 0,
    //                     column: 2,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: integer number overflow
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("4_294_967_296"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 13
    //                 }
    //             ))
    //         ));
    //     }
    //
    //     #[allow(clippy::approx_constant)]
    //     #[test]
    //     fn test_lex_decimal_number_floating_point() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("3.14").unwrap(),
    //             vec![Token::Number(NumberToken::F64(3.14))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("+1.414").unwrap(),
    //             vec![Token::Plus, Token::Number(NumberToken::F64(1.414))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("-2.718").unwrap(),
    //             vec![Token::Minus, Token::Number(NumberToken::F64(2.718))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("2.998e8").unwrap(),
    //             vec![Token::Number(NumberToken::F64(2.998e8))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("2.998e+8").unwrap(),
    //             vec![Token::Number(NumberToken::F64(2.998e+8))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("6.626e-34").unwrap(),
    //             vec![Token::Number(NumberToken::F64(6.626e-34))]
    //         );
    //
    //         // err: incomplete floating point number since ends with '.'
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("123."),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 3,
    //                     line: 0,
    //                     column: 3,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: incomplete floating point number since ends with 'e'
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("123e"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 3,
    //                     line: 0,
    //                     column: 3,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: multiple '.' (point)
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("1.23.456"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 4,
    //                     line: 0,
    //                     column: 4,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: multiple 'e' (exponent)
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("1e23e456"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 4,
    //                     line: 0,
    //                     column: 4,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: unsupports start with dot
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip(".123"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_decimal_number_with_explicit_type() {
    //         // general
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("11i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(11))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("11_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(11))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("11__i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(11))]
    //             );
    //
    //             // location
    //
    //             // "101_i16 103_u32" // text
    //             //  012345678901234  // index
    //             assert_eq!(
    //                 tokenize_from_str("101_i16 103_i32").unwrap(),
    //                 vec![
    //                     TokenWithRange::new(
    //                         Token::Number(NumberToken::I16(101)),
    //                         Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                         7
    //                     ),
    //                     TokenWithRange::new(
    //                         Token::Number(NumberToken::I32(103)),
    //                         Range::from_detail_and_length(/*0,*/ 8, 0, 8),
    //                         7
    //                     ),
    //                 ]
    //             );
    //         }
    //
    //         // byte
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("127_i8").unwrap(),
    //                 vec![Token::Number(NumberToken::I8(127))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("255_i8").unwrap(),
    //                 vec![Token::Number(NumberToken::I8(255))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("256_i8"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 6
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // short
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("32767_i16").unwrap(),
    //                 vec![Token::Number(NumberToken::I16(32767))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("65535_i16").unwrap(),
    //                 vec![Token::Number(NumberToken::I16(65535))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("65536_i16"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 9
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // int
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("2_147_483_647_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(2_147_483_647i32 as u32))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("4_294_967_295_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(u32::MAX))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("4_294_967_296_i32"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 17
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // long
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("9_223_372_036_854_775_807_i64").unwrap(),
    //                 vec![Token::Number(NumberToken::I64(
    //                     9_223_372_036_854_775_807i64 as u64
    //                 )),]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("18_446_744_073_709_551_615_i64").unwrap(),
    //                 vec![Token::Number(NumberToken::I64(u64::MAX))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("18_446_744_073_709_551_616_i64"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 30
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // float
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("3.402_823_5e+38_f32").unwrap(),
    //                 vec![Token::Number(NumberToken::F32(3.402_823_5e38f32))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("1.175_494_4e-38_f32").unwrap(),
    //                 vec![Token::Number(NumberToken::F32(1.175_494_4e-38f32))]
    //             );
    //
    //             // err: overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("3.4e39_f32"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 10
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // double
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("1.797_693_134_862_315_7e+308_f64").unwrap(),
    //                 vec![Token::Number(NumberToken::F64(
    //                     1.797_693_134_862_315_7e308_f64
    //                 )),]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("2.2250738585072014e-308_f64").unwrap(),
    //                 vec![Token::Number(NumberToken::F64(2.2250738585072014e-308f64)),]
    //             );
    //
    //             // err: overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("1.8e309_f64"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 11
    //                     }
    //                 ))
    //             ));
    //         }
    //     }
    //
    //     #[test]
    //     fn test_lex_hex_number() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("0xabcd").unwrap(),
    //             vec![Token::Number(NumberToken::I32(0xabcd))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("-0xaabb").unwrap(),
    //             vec![Token::Minus, Token::Number(NumberToken::I32(0xaabb))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("+0xccdd").unwrap(),
    //             vec![Token::Plus, Token::Number(NumberToken::I32(0xccdd))]
    //         );
    //
    //         // location
    //
    //         assert_eq!(
    //             tokenize_from_str("0xab 0xdef").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::Number(NumberToken::I32(0xab)),
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                     4
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Number(NumberToken::I32(0xdef)),
    //                     Range::from_detail_and_length(/*0,*/ 5, 0, 5,),
    //                     5
    //                 ),
    //             ]
    //         );
    //
    //         // err: invalid char for hex number
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1234xyz"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 6,
    //                     line: 0,
    //                     column: 6,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: hex number overflow
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1_0000_0000"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 13
    //                 }
    //             ))
    //         ));
    //
    //         // err: empty hex number
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 2
    //                 }
    //             ))
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_hex_number_with_explicit_type() {
    //         // general
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x11i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0x11))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x11_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0x11))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x11__i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0x11))]
    //             );
    //
    //             // location
    //
    //             // "0x101_i16 0x103_u32" // text
    //             //  0123456789012345678  // index
    //             assert_eq!(
    //                 tokenize_from_str("0x101_i16 0x103_i32").unwrap(),
    //                 vec![
    //                     TokenWithRange::new(
    //                         Token::Number(NumberToken::I16(0x101)),
    //                         Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                         9
    //                     ),
    //                     TokenWithRange::new(
    //                         Token::Number(NumberToken::I32(0x103)),
    //                         Range::from_detail_and_length(/*0,*/ 10, 0, 10),
    //                         9
    //                     ),
    //                 ]
    //             );
    //         }
    //
    //         // byte
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x7f_i8").unwrap(),
    //                 vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0xff_i8").unwrap(),
    //                 vec![Token::Number(NumberToken::I8(0xff_u8))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0x1_ff_i8"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 9
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // short
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x7fff_i16").unwrap(),
    //                 vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0xffff_i16").unwrap(),
    //                 vec![Token::Number(NumberToken::I16(0xffff_u16))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0x1_ffff_i16"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 12
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // int
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x7fff_ffff_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0xffff_ffff_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0xffff_ffff_u32))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0x1_ffff_ffff_i32"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 17
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // long
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0x7fff_ffff_ffff_ffff_i64").unwrap(),
    //                 vec![Token::Number(NumberToken::I64(
    //                     0x7fff_ffff_ffff_ffff_i64 as u64
    //                 ))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0xffff_ffff_ffff_ffff_i64").unwrap(),
    //                 vec![Token::Number(NumberToken::I64(0xffff_ffff_ffff_ffff_u64))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0x1_ffff_ffff_ffff_ffff_i64"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 27
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // hex decimal
    //         {
    //             // note: this is not a hex floating pointer number
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0xaa_f32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0xaaf32))]
    //             );
    //
    //             // note: this is not a hex floating pointer number
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0xbb_f64").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0xbbf64))]
    //             );
    //         }
    //     }
    //
    //     #[test]
    //     fn test_lex_hex_number_floating_point() {
    //         // default type is f64
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("0x1.4p3").unwrap(),
    //             vec![Token::Number(NumberToken::F64(10f64))]
    //         );
    //
    //         // 3.1415927f32
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("0x1.921fb6p1f32").unwrap(),
    //             vec![Token::Number(NumberToken::F32(std::f32::consts::PI))]
    //         );
    //
    //         // 2.718281828459045f64
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("0x1.5bf0a8b145769p+1_f64").unwrap(),
    //             vec![Token::Number(NumberToken::F64(std::f64::consts::E))]
    //         );
    //
    //         // https://observablehq.com/@jrus/hexfloat
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("0x1.62e42fefa39efp-1_f64").unwrap(),
    //             vec![Token::Number(NumberToken::F64(std::f64::consts::LN_2))]
    //         );
    //
    //         // location
    //
    //         assert_eq!(
    //             tokenize_from_str("0x1.4p3").unwrap(),
    //             vec![TokenWithRange::new(
    //                 Token::Number(NumberToken::F64(10f64)),
    //                 Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                 7
    //             )]
    //         );
    //
    //         // err: missing the exponent
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1.23"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 6
    //                 }
    //             ))
    //         ));
    //
    //         // err: multiple '.' (point)
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1.2.3"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 5,
    //                     line: 0,
    //                     column: 5,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: multiple 'p' (exponent)
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1.2p3p4"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 7,
    //                     line: 0,
    //                     column: 7,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: incorrect type (invalid dot '.' after 'p')
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1.23p4.5"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 8,
    //                     line: 0,
    //                     column: 8,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: incorrect type (invalid char 'i' after 'p')
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0x1.23p4_i32"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 9,
    //                     line: 0,
    //                     column: 9,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_binary_number() {
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("0b1100").unwrap(),
    //             vec![Token::Number(NumberToken::I32(0b1100))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("-0b1010").unwrap(),
    //             vec![Token::Minus, Token::Number(NumberToken::I32(0b1010))]
    //         );
    //
    //         assert_eq!(
    //             tokenize_from_str_without_range_strip("+0b0101").unwrap(),
    //             vec![Token::Plus, Token::Number(NumberToken::I32(0b0101))]
    //         );
    //
    //         // location
    //
    //         assert_eq!(
    //             tokenize_from_str("0b10 0b0101").unwrap(),
    //             vec![
    //                 TokenWithRange::new(
    //                     Token::Number(NumberToken::I32(0b10)),
    //                     Range::from_detail_and_length(/*0,*/ 0, 0, 0,),
    //                     4
    //                 ),
    //                 TokenWithRange::new(
    //                     Token::Number(NumberToken::I32(0b0101)),
    //                     Range::from_detail_and_length(/*0,*/ 5, 0, 5,),
    //                     6
    //                 ),
    //             ]
    //         );
    //
    //         // err: does not support binary floating point
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0b11.10"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 4,
    //                     line: 0,
    //                     column: 4,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: binary number overflow
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0b1_0000_0000_0000_0000_0000_0000_0000_0000"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 43
    //                 }
    //             ))
    //         ));
    //
    //         // err: invalid char for binary number
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0b101xyz"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 5,
    //                     line: 0,
    //                     column: 5,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //
    //         // err: empty binary number
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0b"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 0,
    //                     line: 0,
    //                     column: 0,
    //                     length: 2
    //                 }
    //             ))
    //         ));
    //     }
    //
    //     #[test]
    //     fn test_lex_binary_number_with_explicit_type() {
    //         // general
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b11i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0b11))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b11_i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0b11))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b11__i32").unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0b11))]
    //             );
    //
    //             // location
    //
    //             // "0b101_i16 0b1010_u32"
    //             //  01234567890123456789  // index
    //             assert_eq!(
    //                 tokenize_from_str("0b101_i16 0b1010_i32").unwrap(),
    //                 vec![
    //                     TokenWithRange::new(
    //                         Token::Number(NumberToken::I16(0b101)),
    //                         Range::from_detail_and_length(/*0,*/ 0, 0, 0),
    //                         9
    //                     ),
    //                     TokenWithRange::new(
    //                         Token::Number(NumberToken::I32(0b1010)),
    //                         Range::from_detail_and_length(/*0,*/ 10, 0, 10),
    //                         10
    //                     ),
    //                 ]
    //             );
    //         }
    //
    //         // byte
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b0111_1111_i8").unwrap(),
    //                 vec![Token::Number(NumberToken::I8(0x7f_i8 as u8))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b1111_1111_i8").unwrap(),
    //                 vec![Token::Number(NumberToken::I8(0xff_u8))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0b1_1111_1111_i8"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 16
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // short
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b0111_1111_1111_1111_i16").unwrap(),
    //                 vec![Token::Number(NumberToken::I16(0x7fff_i16 as u16))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b1111_1111_1111_1111_i16").unwrap(),
    //                 vec![Token::Number(NumberToken::I16(0xffff_u16))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0b1_1111_1111_1111_1111_i16"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 27
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // int
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b0111_1111_1111_1111__1111_1111_1111_1111_i32")
    //                     .unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0x7fff_ffff_i32 as u32))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b1111_1111_1111_1111__1111_1111_1111_1111_i32")
    //                     .unwrap(),
    //                 vec![Token::Number(NumberToken::I32(0xffff_ffff_u32))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0b1_1111_1111_1111_1111__1111_1111_1111_1111_i32"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 48
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // long
    //         {
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b0111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64").unwrap(),
    //                 vec![Token::Number(NumberToken::I64(0x7fff_ffff_ffff_ffff_i64 as u64))]
    //             );
    //
    //             assert_eq!(
    //                 tokenize_from_str_without_range_strip("0b1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64").unwrap(),
    //                 vec![Token::Number(NumberToken::I64(0xffff_ffff_ffff_ffff_u64))]
    //             );
    //
    //             // err: unsigned overflow
    //             assert!(matches!(
    //                 tokenize_from_str_without_range_strip("0b1_1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111__1111_1111_1111_1111_i64"),
    //                 Err(PreprocessError::MessageWithPosition(
    //                     _,
    //                     Position {
    //                         /* unit: 0, */
    //                         index: 0,
    //                         line: 0,
    //                         column: 0,
    //                         length: 90
    //                     }
    //                 ))
    //             ));
    //         }
    //
    //         // err: does not support binary floating pointer number (invalid char 'f' for binary number)
    //         assert!(matches!(
    //             tokenize_from_str_without_range_strip("0b11_f32"),
    //             Err(PreprocessError::MessageWithPosition(
    //                 _,
    //                 Position {
    //                     /* unit: 0, */
    //                     index: 5,
    //                     line: 0,
    //                     column: 5,
    //                     length: 0
    //                 }
    //             ))
    //         ));
    //     }

    #[test]
    fn test_tokenize_char() {
        assert_eq!(
            tokenize_from_str_without_range_strip("'a'").unwrap(),
            vec![Token::Char('a')]
        );

        assert_eq!(
            tokenize_from_str_without_range_strip("'a' 'z'").unwrap(),
            vec![Token::Char('a'), Token::Char('z')]
        );

        // CJK
        assert_eq!(
            tokenize_from_str_without_range_strip("'文'").unwrap(),
            vec![Token::Char('文')]
        );

        // emoji
        assert_eq!(
            tokenize_from_str_without_range_strip("'😊'").unwrap(),
            vec![Token::Char('😊')]
        );

        // escape char `\\`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\\\'").unwrap(),
            vec![Token::Char('\\')]
        );

        // escape char `\'`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\\''").unwrap(),
            vec![Token::Char('\'')]
        );

        // escape char `"`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\\"'").unwrap(),
            vec![Token::Char('"')]
        );

        // escape char `\t`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\t'").unwrap(),
            vec![Token::Char('\t')]
        );

        // escape char `\r`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\r'").unwrap(),
            vec![Token::Char('\r')]
        );

        // escape char `\n`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\n'").unwrap(),
            vec![Token::Char('\n')]
        );

        // escape char `\0`
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\0'").unwrap(),
            vec![Token::Char('\0')]
        );

        // escape char, octal
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\70'").unwrap(),
            vec![Token::Char('8')]
        );

        // escape char, octal
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\101'").unwrap(),
            vec![Token::Char('A')]
        );

        // escape char, hex
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\x38'").unwrap(),
            vec![Token::Char('8')]
        );

        // escape char, hex
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\x41'").unwrap(),
            vec![Token::Char('A')]
        );

        // escape char, unicode
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\u002d'").unwrap(),
            vec![Token::Char('-')]
        );

        // escape char, unicode
        assert_eq!(
            tokenize_from_str_without_range_strip("'\\U00006587'").unwrap(),
            vec![Token::Char('文')]
        );

        // location

        assert_eq!(
            tokenize_from_str("'a' '文'").unwrap(),
            vec![
                TokenWithRange::new(Token::Char('a'), Range::from_detail_and_length(0, 0, 0, 3)),
                TokenWithRange::new(Token::Char('文'), Range::from_detail_and_length(4, 0, 4, 3))
            ]
        );

        assert_eq!(
            tokenize_from_str("'\\t'").unwrap(),
            vec![TokenWithRange::new(
                Token::Char('\t'),
                Range::from_detail_and_length(0, 0, 0, 4)
            )]
        );

        assert_eq!(
            tokenize_from_str("'\\u6587'").unwrap(),
            vec![TokenWithRange::new(
                Token::Char('文'),
                Range::from_detail_and_length(0, 0, 0, 8)
            )]
        );

        // err: empty char
        assert!(matches!(
            tokenize_from_str_without_range_strip("''"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 0,
                        line: 0,
                        column: 0,
                    },
                    end_included: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    }
                }
            ))
        ));

        // err: incomplete char, missing the content, encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip("'"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete char, missing the closing single quote, encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip("'a"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: invalid char, expect the right quote, encounter another char
        assert!(matches!(
            tokenize_from_str_without_range_strip("'ab"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 2,
                    line: 0,
                    column: 2,
                }
            ))
        ));

        // err: invalid char, expect the right quote, encounter char 'b'
        assert!(matches!(
            tokenize_from_str_without_range_strip("'ab'"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 2,
                    line: 0,
                    column: 2,
                }
            ))
        ));

        // err: unsupported escape char `'\?'`
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#"'\?'"#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 2,
                        line: 0,
                        column: 2,
                    }
                }
            ))
        ));

        // err: incorrect hex escape `'\x3'`
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#"'\x3'"#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 3,
                        line: 0,
                        column: 3,
                    }
                }
            ))
        ));

        // err: empty unicode escape string
        // '\\u'
        // 01 23    // index
        assert!(matches!(
            tokenize_from_str_without_range_strip("'\\u'"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 2,
                        line: 0,
                        column: 2,
                    }
                }
            ))
        ));

        // err: invalid unicode escape string, the number of digits should be 4 or 8.
        assert!(matches!(
            tokenize_from_str_without_range_strip("'\\u123"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 5,
                        line: 0,
                        column: 5,
                    }
                }
            ))
        ));

        // err: invalid unicode escape string, the number of digits should be 4 or 8.
        // '\\U1000111'
        // 01 234567890    // index
        assert!(matches!(
            tokenize_from_str_without_range_strip("'\\U1000111'"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 9,
                        line: 0,
                        column: 9,
                    }
                }
            ))
        ));

        // err: invalid unicode code point, code point out of range
        // '\\U00123456'
        // 01 2345678901    // index
        assert!(matches!(
            tokenize_from_str_without_range_strip("'\\U00123456'"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 10,
                        line: 0,
                        column: 10,
                    }
                }
            ))
        ));

        // err: invalid char in the unicode escape sequence
        assert!(matches!(
            tokenize_from_str_without_range_strip("'\\u12mn''"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 1,
                        line: 0,
                        column: 1,
                    },
                    end_included: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    }
                }
            ))
        ));
    }

    #[test]
    fn test_tokenize_string() {
        assert_eq!(
            tokenize_from_str_without_range_strip(r#""abc""#).unwrap(),
            vec![Token::new_string("abc")]
        );

        assert_eq!(
            tokenize_from_str_without_range_strip(r#""abc" "xyz""#).unwrap(),
            vec![Token::new_string("abc"), Token::new_string("xyz")]
        );

        assert_eq!(
            tokenize_from_str_without_range_strip("\"abc\"\n\n\"xyz\"").unwrap(),
            vec![
                Token::new_string("abc"),
                Token::Newline,
                Token::Newline,
                Token::new_string("xyz"),
            ]
        );

        // unicode
        assert_eq!(
            tokenize_from_str_without_range_strip(
                r#"
                "abc文字😊"
                "#
            )
            .unwrap(),
            vec![
                Token::new_string("abc文字😊"),
                Token::Newline,
            ]
        );

        // empty string
        assert_eq!(
            tokenize_from_str_without_range_strip("\"\"").unwrap(),
            vec![Token::new_string("")]
        );

        // escape chars
        assert_eq!(
            tokenize_from_str_without_range_strip(
                r#"
                "\\\'\"\t\r\n\0\x2d\u6587"
                "#
            )
            .unwrap(),
            vec![
                Token::new_string("\\\'\"\t\r\n\0-文"),
                Token::Newline,
            ]
        );

        // location
        // "abc" "文字😊"
        // 01234567 8 9 0

        assert_eq!(
            tokenize_from_str(r#""abc" "文字😊""#).unwrap(),
            vec![
                TokenWithRange::new(
                    Token::new_string("abc"),
                    Range::from_detail_and_length(0, 0, 0, 5)
                ),
                TokenWithRange::new(
                    Token::new_string("文字😊"),
                    Range::from_detail_and_length(6, 0, 6, 5)
                ),
            ]
        );

        // err: incomplete string, missing the content, encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip("\""),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete string, missing the closing double quote, encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip("\"abc"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete string, missing the closing double quote, ends with '\n' and encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip("\"abc\n"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete string, missing the closing double quote, ends with whitespaces and encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip("\"abc\n   "),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: unsupported escape char `'\?'`.
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\?xyz""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 5,
                        line: 0,
                        column: 5,
                    }
                }
            ))
        ));

        // err: incorrect hex escape `'\x3'`
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\x3xyz""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 6,
                        line: 0,
                        column: 6,
                    }
                }
            ))
        ));

        // err: empty unicode escape string
        // "abc\\u"
        // 01234 5  // index
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\u""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 5,
                        line: 0,
                        column: 5,
                    }
                }
            ))
        ));

        // err: invalid unicode escape string, the number of digits should be 4 or 8.
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\u123""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 8,
                        line: 0,
                        column: 8,
                    }
                }
            ))
        ));

        // err: invalid unicode escape string, the number of digits should be 4 or 8.
        // "abc\\U1000111xyz"
        // 01234 567890234567   // index
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\U1000111xyz""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 12,
                        line: 0,
                        column: 12,
                    }
                }
            ))
        ));

        // err: invalid unicode code point, code point out of range
        // "abc\\U00123456xyz"
        // 01234 5678901234567
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\U00123456xyz""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 13,
                        line: 0,
                        column: 13,
                    }
                }
            ))
        ));

        // err: invalid char in the unicode escape sequence
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\u12mnxyz""#),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4,
                    },
                    end_included: Position {
                        index: 7,
                        line: 0,
                        column: 7,
                    }
                }
            ))
        ));

        // err: incomplete string, missing the closing double quote, ends with unicode escape sequence and encounter EOF
        assert!(matches!(
            tokenize_from_str_without_range_strip(r#""abc\u1234"#),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));
    }

    #[test]
    fn test_tokenize_continued_string() {
        // TODO
    }
}
