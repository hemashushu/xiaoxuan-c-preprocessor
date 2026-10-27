// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    char_with_position::{CharWithPosition, CharsWithPositionIter},
    error::PreprocessError,
    peekable_iter::PeekableIter,
};

/// Prepares source characters for tokenization while retaining source positions.
///
/// References:
/// - <https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html>
/// - <https://en.cppreference.com/w/c/language/translation_phases.html>
///
/// 1. Splice backslash-continued lines.
/// 2. Replace block comments with spaces and line comments with newlines.
/// 3. Remove leading whitespace and an optional shebang (`#!`) line.
///
/// Shebang handling is an extension.
/// Trigraphs are not converted; they were removed from C23.
pub fn initialize(source_text: &str) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut chars = source_text.chars();
    let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
    let mut peekable_char_position_iter = PeekableIter::new(&mut char_position_iter);

    let merged = merge_continued_lines(&mut peekable_char_position_iter)?;
    let mut merged_iter = merged.into_iter();
    let mut peekable_merged_iter = PeekableIter::new(&mut merged_iter);

    let clean = remove_comments(&mut peekable_merged_iter)?;
    let mut clean_iter = clean.into_iter();
    let mut peekable_clean_iter = PeekableIter::new(&mut clean_iter);

    remove_shebang(&mut peekable_clean_iter)
}

/// Merges continued lines in the input text.
/// If a line ends with a backslash ('\'), it is joined with the next line.
/// The newline must immediately follow the backslash.
///
/// This rule is applied even if the backslash is inside:
/// - string literals
/// - character constants
/// - comments (although comments are removed in a later step)
///
fn merge_continued_lines(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut output = vec![];
    while let Some(char_with_position) = chars.next() {
        match char_with_position.character {
            '\\' => {
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
                    // Splice a continuation ending in CRLF.
                    chars.next(); // Consume '\r'
                    chars.next(); // Consume '\n'
                } else if matches!(
                    chars.peek(0),
                    Some(CharWithPosition {
                        character: '\n',
                        ..
                    })
                ) {
                    // Splice a continuation ending in LF.
                    chars.next(); // Consume '\n'
                } else {
                    output.push(char_with_position);
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

/// Replaces comments outside quoted literals, preserving literal spelling and
/// source positions. Apostrophes in preprocessing numbers are digit separators.
fn remove_comments(
    chars: &mut PeekableIter<CharWithPosition>,
) -> Result<Vec<CharWithPosition>, PreprocessError> {
    let mut output = vec![];
    while let Some(char_with_position) = chars.next() {
        match char_with_position.character {
            '"' | '\'' => {
                let quote = char_with_position.character;
                output.push(char_with_position);
                while let Some(literal_char) = chars.next() {
                    let character = literal_char.character;
                    output.push(literal_char);
                    if character == '\\' {
                        if let Some(escaped_char) = chars.next() {
                            output.push(escaped_char);
                        }
                    } else if character == quote || character == '\n' {
                        break;
                    }
                }
            }
            character
                if character.is_ascii_digit()
                    || (character == '.'
                        && chars
                            .peek(0)
                            .is_some_and(|item| item.character.is_ascii_digit())) =>
            {
                let mut previous = character;
                output.push(char_with_position);
                while let Some(next_char) = chars.peek(0) {
                    let character = next_char.character;
                    if character.is_alphanumeric()
                        || character == '_'
                        || character == '.'
                        || (matches!(character, '+' | '-')
                            && matches!(previous, 'e' | 'E' | 'p' | 'P'))
                        || (character == '\''
                            && chars.peek(1).is_some_and(|item| {
                                item.character.is_alphanumeric() || item.character == '_'
                            }))
                    {
                        output.push(chars.next().unwrap());
                        previous = character;
                    } else {
                        break;
                    }
                }
            }
            character if character.is_alphabetic() || character == '_' || character >= '\u{a0}' => {
                output.push(char_with_position);
                while chars.peek(0).is_some_and(|item| {
                    item.character.is_alphanumeric()
                        || item.character == '_'
                        || item.character >= '\u{a0}'
                }) {
                    output.push(chars.next().unwrap());
                }
            }
            '/' if matches!(chars.peek(0), Some(CharWithPosition { character: '/', .. })) => {
                chars.next(); // Consume '/'

                // Found a line comment:
                // Consume all characters until the end of the line.
                for next_char_with_position in chars.by_ref() {
                    if next_char_with_position.character == '\n' {
                        break; // Stop at the end of the line
                    }
                }

                // Preserve a logical line boundary so a following directive
                // is recognized. Associate it with the original comment start.
                output.push(CharWithPosition {
                    character: '\n',
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

/// Removes leading whitespace and then an optional shebang line.
///
/// Leading whitespace is discarded even without a shebang. Comments have
/// already been removed, so a shebang may also follow initial comments.
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
        initializer::{initialize, merge_continued_lines, remove_comments, remove_shebang},
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
        let mut iter = PeekableIter::new(&mut char_position_iter);

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
                CharWithPosition::new('\\', Position::new(12, 2, 3)),
                CharWithPosition::new(' ', Position::new(13, 2, 4)),
                CharWithPosition::new(' ', Position::new(14, 2, 5)),
                CharWithPosition::new(' ', Position::new(15, 2, 6)),
                CharWithPosition::new(' ', Position::new(16, 2, 7)),
                CharWithPosition::new('\n', Position::new(17, 2, 8)),
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
    fn test_merge_continued_lines_requires_adjacent_newline() {
        for newline in ["\n", "\r\n"] {
            for whitespace in [" ", "\t", "\u{0b}", "\u{0c}", " \t "] {
                let source_text = format!("first\\{whitespace}{newline}second");
                let mut chars = source_text.chars();
                let mut positioned = CharsWithPositionIter::new(&mut chars);
                let expected = positioned.by_ref().collect::<Vec<_>>();
                let mut chars = source_text.chars();
                let mut input = CharsWithPositionIter::new(&mut chars);
                let mut iter = PeekableIter::new(&mut input);

                assert_eq!(merge_continued_lines(&mut iter).unwrap(), expected);
            }
        }
    }

    #[test]
    fn test_remove_comments_preserves_literals() {
        for (source_text, expected) in [
            (
                r#""https://example.com/a/*b*/" /* comment */ '/'"#,
                r#""https://example.com/a/*b*/"   '/'"#,
            ),
            (
                r#""escaped \" // literal" /* comment */ '/'"#,
                r#""escaped \" // literal"   '/'"#,
            ),
            (r#""\\"/* comment */"// literal""#, r#""\\" "// literal""#),
            (
                r#"'\''/* comment */"/* literal */""#,
                r#"'\'' "/* literal */""#,
            ),
            (r#"'"'/* comment */"// literal""#, r#"'"' "// literal""#),
            ("1'000 /* comment */ 2'000", "1'000   2'000"),
            ("0xA'B /* comment */ 0xC'D", "0xA'B   0xC'D"),
        ] {
            let mut chars = source_text.chars();
            let mut positioned = CharsWithPositionIter::new(&mut chars);
            let mut iter = PeekableIter::new(&mut positioned);
            let clean = remove_comments(&mut iter).unwrap();

            assert_eq!(
                clean.iter().map(|item| item.character).collect::<String>(),
                expected
            );
            let original = source_text.chars().collect::<Vec<_>>();
            for item in clean.iter().filter(|item| !item.character.is_whitespace()) {
                assert_eq!(item.character, original[item.position.index]);
            }
        }
    }

    #[test]
    fn test_remove_comments() {
        let source_text = "012 // foo\n123 /* bar \n buzz */ 234";
        // index:                01234567890 123456789012 345678901234

        let mut chars = source_text.chars();
        let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
        let mut iter = PeekableIter::new(&mut char_position_iter);

        let clean = remove_comments(&mut iter).unwrap();

        assert_eq!(
            clean,
            vec![
                // line 0
                CharWithPosition::new('0', Position::new(0, 0, 0)),
                CharWithPosition::new('1', Position::new(1, 0, 1)),
                CharWithPosition::new('2', Position::new(2, 0, 2)),
                CharWithPosition::new(' ', Position::new(3, 0, 3)),
                CharWithPosition::new('\n', Position::new(4, 0, 4)), // in place of line comment
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
        let mut peekable_chars = PeekableIter::new(&mut char_position_iter);

        let clean = remove_comments(&mut peekable_chars).unwrap();
        let mut clean_iter = clean.into_iter();
        let mut peekable_clean_iter = PeekableIter::new(&mut clean_iter);
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
