// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    PreprocessError,
    char_with_position::{CharWithPosition, CharsWithPositionIter},
    peekable_iter::PeekableIter,
};

// Buffer sizes for lookahead in `PeekableIter`.
const PEEK_BUFFER_LENGTH_MERGE_CONTINUED_LINES: usize = 2;
const PEEK_BUFFER_LENGTH_REMOVE_COMMENTS: usize = 2;
const PEEK_BUFFER_LENGTH_REMOVE_SHEBANG: usize = 3;

/// Preprocessing steps before tokenization.
///
/// References:
/// - https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html
/// - https://en.cppreference.com/w/c/language/translation_phases.html
///
/// 1. The input file is loaded into memory and split into lines. (Skipped)
/// 2. Trigraph sequences are replaced with their corresponding single characters if enabled. (ANCPP does not support)
/// 3. Lines ending with a backslash ('\') are joined with the following line.
/// 4. All comments are replaced by a single space character.
/// 5. Remove the shebang (`#!`) line. (ANCPP new added)
pub fn initialize(source_text: &str) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut chars = source_text.chars();
    let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
    let mut peekable_char_position_iter = PeekableIter::new(
        &mut char_position_iter,
        PEEK_BUFFER_LENGTH_MERGE_CONTINUED_LINES,
    );

    let merged = merge_continued_lines(&mut peekable_char_position_iter)?;
    let mut merged_iter = merged.into_iter();
    let mut peekable_merged_iter =
        PeekableIter::new(&mut merged_iter, PEEK_BUFFER_LENGTH_REMOVE_COMMENTS);

    let clean = remove_comments(&mut peekable_merged_iter)?;
    let mut clean_iter = clean.into_iter();
    let mut peekable_clean_iter =
        PeekableIter::new(&mut clean_iter, PEEK_BUFFER_LENGTH_REMOVE_SHEBANG);

    remove_shebang(&mut peekable_clean_iter)
}

/// Merges continued lines in the input text.
/// If a line ends with a backslash ('\'), it is joined with the next line.
/// Whitespace between the backslash and the newline is also handled.
///
/// This rule is applied even if the backslash is inside:
/// - string literals
/// - character constants
/// - comments (although comments are removed in a later step)
/// or there are spaces/tabs between the backslash and the newline.
fn merge_continued_lines(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut output = vec![];
    while let Some(char_with_position) = chars.next() {
        match char_with_position.character {
            '\\' => {
                // Try to consume any whitespace characters between '\' and the newline ('\n' or "\r\n").
                let mut saved_whitespaces = vec![];
                while let Some(next_char_with_position) = chars.peek(0) {
                    // Store all whitespaces (space, tab, vertical tab, form feed) to `saved_whitespaces`.
                    match next_char_with_position.character {
                        ' ' | '\t' | '\u{0b}' | '\u{0c}' => {
                            saved_whitespaces.push(chars.next().unwrap());
                        }
                        _ => {
                            break;
                        }
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
                    // Restore the '\' character and the stored whitespaces.
                    output.push(char_with_position); // Restore '\'
                    output.extend(saved_whitespaces); // Restore stored whitespaces
                }
            }
            _ => {
                // Leave all other characters unchanged
                output.push(char_with_position);
            }
        }
    }

    Ok(output)
}

/// Replaces both line comments and block comments
/// with a single space character.
fn remove_comments(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut output = vec![];
    while let Some(char_with_position) = chars.next() {
        match char_with_position.character {
            '/' if matches!(chars.peek(0), Some(CharWithPosition { character: '/', .. })) => {
                chars.next(); // Consume '/'

                // Found a line comment:
                // Consume all characters until the end of the line.
                while let Some(next_char_with_position) = chars.next() {
                    if next_char_with_position.character == '\n' {
                        break; // Stop at the end of the line
                    }
                }

                // Insert a space in place of the line comment.
                output.push(CharWithPosition {
                    character: ' ',
                    position: char_with_position.position,
                });
            }
            '/' if matches!(chars.peek(0), Some(CharWithPosition { character: '*', .. })) => {
                chars.next(); // Consume '*'

                // Found a block comment:
                // Consume all characters until the closing '*/'.
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

                // Insert a space in place of the block comment.
                output.push(CharWithPosition {
                    character: ' ',
                    position: char_with_position.position,
                });
            }
            _ => output.push(char_with_position), // Leave all other characters unchanged
        }
    }
    Ok(output)
}

/// Removes the shebang line (if present) from the start of the input.
/// Also removes any leading whitespace (including newlines) before the
/// shebang line.
///
/// Because the comments have already been removed in a previous step,
/// thus comments are allowed before the shebang line.
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
        for next_char_with_position in chars.by_ref() {
            if next_char_with_position.character == '\n' {
                break; // Stop at the end of the line
            }
        }
    }

    let output = chars.collect::<Vec<_>>();
    Ok(output)
}

#[cfg(test)]
mod tests {
    use crate::{
        char_with_position::{CharWithPosition, CharsWithPositionIter},
        initializer::{
            PEEK_BUFFER_LENGTH_MERGE_CONTINUED_LINES, PEEK_BUFFER_LENGTH_REMOVE_COMMENTS,
            PEEK_BUFFER_LENGTH_REMOVE_SHEBANG, initialize, merge_continued_lines, remove_comments,
            remove_shebang,
        },
        peekable_iter::PeekableIter,
        position::Position,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn test_merge_continued_lines() {
        let source_text = "012\n456\\\n901\\    \n890\\\r\n456\n890";
        // index:                0123 4567 8 9012 34567 8901 2 3 4567 890

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut iter = PeekableIter::new(
            &mut char_position_iter,
            PEEK_BUFFER_LENGTH_MERGE_CONTINUED_LINES,
        );

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
        // index:                01234567890 123456789012 345678901234

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut iter =
            PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_LENGTH_REMOVE_COMMENTS);

        let clean = remove_comments(&mut iter).unwrap();

        assert_eq!(
            clean,
            vec![
                // line 0
                CharWithPosition::new('0', Position::new(0, 0, 0)),
                CharWithPosition::new('1', Position::new(1, 0, 1)),
                CharWithPosition::new('2', Position::new(2, 0, 2)),
                CharWithPosition::new(' ', Position::new(3, 0, 3)),
                CharWithPosition::new(' ', Position::new(4, 0, 4)), // in place of line comment
                // line 1
                CharWithPosition::new('1', Position::new(11, 1, 0)),
                CharWithPosition::new('2', Position::new(12, 1, 1)),
                CharWithPosition::new('3', Position::new(13, 1, 2)),
                CharWithPosition::new(' ', Position::new(14, 1, 3)),
                CharWithPosition::new(' ', Position::new(15, 1, 4)), // in place of block comment
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
        // index:                012345 6789012345678901234 567

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut peekable_chars =
            PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_LENGTH_REMOVE_COMMENTS);

        let clean = remove_comments(&mut peekable_chars).unwrap();
        let mut clean_iter = clean.into_iter();
        let mut peekable_clean_iter =
            PeekableIter::new(&mut clean_iter, PEEK_BUFFER_LENGTH_REMOVE_SHEBANG);
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
    fn test_initialize() {
        // This source text combines continued lines, line comments, block comments,
        // and a shebang line.
        let source_text = "/\\\n/foo\n  #!/bin/ancc\n1/\\\n*bar*/2";
        // index:                01 2 34567 89012345678901 234 5 6789012

        let clean = initialize(source_text).unwrap();

        assert_eq!(
            clean,
            vec![
                CharWithPosition::new('1', Position::new(22, 3, 0)), // line 3
                CharWithPosition::new(' ', Position::new(23, 3, 1)), // in place of block comment
                CharWithPosition::new('2', Position::new(32, 4, 6)), // line 4
            ]
        );
    }
}
