// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::char;

use crate::{
    char_with_position::{CharWithPosition, CharsWithPositionIter},
    error::PreprocessError,
    initializer::initialize,
    peekable_iter::PeekableIter,
    position::Position,
    range::Range,
    token::{
        CharEncoding, FloatingPointNumber, FloatingPointNumberWidth, IntegerNumber,
        IntegerNumberWidth, Number, Punctuator, StringEncoding, Token, TokenWithRange,
    },
};

use unicode_normalization::UnicodeNormalization;

// Buffer sizes for lookahead in `PeekableIter`.
const PEEK_BUFFER_LENGTH_LEX: usize = 4;

pub fn lex_from_str(source_text: &str) -> Result<Vec<TokenWithRange>, PreprocessError> {
    let chars = initialize(source_text)?;
    let mut chars_iter = chars.into_iter();
    let mut peekable_char_iter = PeekableIter::new(&mut chars_iter, PEEK_BUFFER_LENGTH_LEX);
    let mut tokenizer = Lexer::new(&mut peekable_char_iter);
    tokenizer.lex()
}

pub fn lex_from_clean_str(
    clean_source_text: &str,
) -> Result<Vec<TokenWithRange>, PreprocessError> {
    let mut chars = clean_source_text.chars();
    let mut char_position_iter = CharsWithPositionIter::new(&mut chars);
    let mut peekable_char_iter = PeekableIter::new(&mut char_position_iter, PEEK_BUFFER_LENGTH_LEX);
    let mut tokenizer = Lexer::new(&mut peekable_char_iter);
    tokenizer.lex()
}

/// Lexer struct that turns a character stream into a token stream.
///
/// Note that there are some inconsistencies in C preprocessor syntax:
///
/// - In the `#include` and `#embed` directives, and the `has_include` and `has_embed`
///   operators, the file path is not a string literal.
/// - Whitespaces are not allowed between the macro identifier
///   and the opening parenthesis `(` in function-like macro replacement.
///
/// When implementing the lexer, these inconsistencies need to be taken into account.
struct Lexer<'a> {
    upstream: &'a mut PeekableIter<'a, CharWithPosition>,

    // The position of the last consumed character by `next_char()`.
    pub last_position: Position,

    // The token 1-based index of the last newline character.
    //
    // It is used to determine if the current `#` token is a directive start token.
    // When the `#` token appears immediately after a newline character,
    // it is considered as the start of a preprocessor directive.
    //
    // Initialized to 0 to make the `#` as directive start token if it is the first token.
    pub last_newline_token_index_1_based: usize,

    // The line index of last directive line.
    //
    // It is used to determine if the current newline character is the end of a directive.
    // When a newline character appears after a directive line,
    // a `DirectiveEnd` token is emitted.
    //
    // Initialized to `None` to indicate no directive line at initialization.
    pub last_directive_line_index: Option<usize>,

    // Stack of positions.
    // It is used to store the positions of characters when consuming them in sequence,
    // and later used to create the `Range` of tokens.
    position_stack: Vec<Position>,
}

impl<'a> Lexer<'a> {
    fn new(upstream: &'a mut PeekableIter<'a, CharWithPosition>) -> Self {
        Self {
            upstream,
            last_position: Position::default(),
            last_newline_token_index_1_based: 0, // to make the `#` as directive start token if it is the first token
            last_directive_line_index: None, // no directive line at initialization
            position_stack: vec![],
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

    fn peek_char_and_equals(&self, offset: usize, expected_char: char) -> bool {
        matches!(
            self.upstream.peek(offset),
            Some(CharWithPosition { character, .. }) if character == &expected_char)
    }

    fn peek_char_and_anyof(&self, offset: usize, expected_chars: &[char]) -> bool {
        matches!(
            self.upstream.peek(offset),
            Some(CharWithPosition { character, .. }) if expected_chars.contains(character))
    }

    /// Saves the last position to the stack.
    ///
    /// Where the last position is identical to position of
    /// the character consumed by `next_char()`.
    fn push_last_position_into_stack(&mut self) {
        self.position_stack.push(self.last_position);
    }

    /// Saves the current position to the stack.
    ///
    /// Where the current position is identical to the value returned by
    /// `self.peek_position(0)`.
    fn push_peek_position_into_stack(&mut self) {
        let position = *self.peek_position(0).unwrap();
        self.position_stack.push(position);
    }

    /// Pops a position from the stack.
    ///
    /// It is usually used after `push_last_position_into_stack()` or
    /// `push_peek_position_into_stack()` to form a `Range` for a token.
    fn pop_position_from_stack(&mut self) -> Position {
        self.position_stack.pop().unwrap()
    }
}

impl Lexer<'_> {
    fn lex(&mut self) -> Result<Vec<TokenWithRange>, PreprocessError> {
        // Output token stream.
        let mut output = vec![];

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                ' ' | '\t' | '\u{0b}' | '\u{0c}' => {
                    // Skip whitespaces (space, tab, vertical tab, form feed).
                    // https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
                    self.next_char();
                }
                '\r' if self.peek_char_and_equals(1, '\n') => {
                    // Convert Windows-style line ending "\r\n" to a single '\n'.
                    self.next_char(); // consume '\r'
                    self.next_char(); // consume '\n'

                    // Emit DirectiveEnd token if in a directive line:
                    // `# ... \r\n`
                    if self.in_directive_line() {
                        output.push(TokenWithRange::new(
                            Token::DirectiveEnd,
                            Range::from_single_position(&self.last_position),
                        ));
                    }

                    // Update the `last_newline_index` to the current output length.
                    self.last_newline_token_index_1_based = output.len();
                }
                '\n' => {
                    self.next_char(); // consume '\n'

                    // Emit DirectiveEnd token if in a directive line:
                    // `# ... \n`
                    if self.in_directive_line() {
                        output.push(TokenWithRange::new(
                            Token::DirectiveEnd,
                            Range::from_single_position(&self.last_position),
                        ));
                    }

                    // Update the `last_newline_index` to the current output length.
                    self.last_newline_token_index_1_based = output.len();
                }
                '\'' => {
                    // Char literal token
                    output.push(self.lex_char(CharEncoding::Default, 0)?);
                }
                '"' => {
                    if self.in_inclusion_directive(&output) || self.in_inclusion_operator(&output) {
                        // FilePath token
                        output.push(self.lex_quoted_file_path()?);
                    } else {
                        // String literal token
                        output.push(self.lex_string(StringEncoding::Default, 0)?);
                    }
                }
                '+' => {
                    if self.peek_char_and_equals(1, '+') {
                        // `++`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '+'
                        self.next_char(); // consume '+'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Increase),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '=') {
                        // `+=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '+'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::AddAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `+`
                        self.next_char(); // consume '+'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Add),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '-' => {
                    if self.peek_char_and_equals(1, '-') {
                        // `--`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '-'
                        self.next_char(); // consume '-'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Decrease),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '=') {
                        // `-=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '-'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::SubtractAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '>') {
                        // `->`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '-'
                        self.next_char(); // consume '>'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Arrow),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `-`
                        self.next_char(); // consume '-'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Subtract),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '*' => {
                    if self.peek_char_and_equals(1, '=') {
                        // `*=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '*'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::MultiplyAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `*`
                        self.next_char(); // consume '*'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Multiply),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '/' => {
                    if self.peek_char_and_equals(1, '=') {
                        // `/=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '/'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::DivideAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `/`
                        self.next_char(); // consume '/'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Divide),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '%' => {
                    if self.peek_char_and_equals(1, ':') || self.peek_char_and_equals(1, '>') {
                        // `%:` or `%>`
                        // Alternative tokens (digraphs) are not supported.
                        return Err(PreprocessError::MessageWithRange(
                            "Digraphs are not supported.".to_owned(),
                            Range::from_position_and_length(self.peek_position(0).unwrap(), 2),
                        ));
                    } else if self.peek_char_and_equals(1, '=') {
                        // `%=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '%'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::ModulusAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `%`
                        self.next_char(); // consume '%'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Modulo),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '=' => {
                    if self.peek_char_and_equals(1, '=') {
                        // `==`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '='
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Equal),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `=`
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Assign),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '!' => {
                    if self.peek_char_and_equals(1, '=') {
                        // `!=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '!'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::NotEqual),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `!`
                        self.next_char(); // consume '!'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Not),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '>' => {
                    if self.peek_char_and_equals(1, '=') {
                        // `>=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '>'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::GreaterThanOrEqual),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '>') {
                        // `>>` or `>>=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '>'
                        self.next_char(); // consume '>'

                        if self.peek_char_and_equals(0, '=') {
                            // `>>=`
                            self.next_char(); // consume '='

                            output.push(TokenWithRange::new(
                                Token::Punctuator(Punctuator::ShiftRightAssign),
                                Range::new(&self.pop_position_from_stack(), &self.last_position),
                            ));
                        } else {
                            // `>>`
                            output.push(TokenWithRange::new(
                                Token::Punctuator(Punctuator::ShiftRight),
                                Range::new(&self.pop_position_from_stack(), &self.last_position),
                            ));
                        }
                    } else {
                        // `>`
                        self.next_char(); // consume '>'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::GreaterThan),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '<' => {
                    if self.peek_char_and_equals(1, '%') || self.peek_char_and_equals(1, ':') {
                        // `<%` or `<:`
                        // Alternative tokens (digraphs) are not supported.
                        return Err(PreprocessError::MessageWithRange(
                            "Digraphs are not supported.".to_owned(),
                            Range::from_position_and_length(self.peek_position(0).unwrap(), 2),
                        ));
                    } else if self.in_inclusion_directive(&output)
                        || self.in_inclusion_operator(&output)
                    {
                        // FilePath token
                        output.push(self.lex_angle_bracket_file_path()?);
                    } else if self.peek_char_and_equals(1, '=') {
                        // `<=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '<'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::LessThanOrEqual),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '<') {
                        // `<<` or `<<=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '<'
                        self.next_char(); // consume '<'

                        if self.peek_char_and_equals(0, '=') {
                            // `<<=`
                            self.next_char(); // consume '='

                            output.push(TokenWithRange::new(
                                Token::Punctuator(Punctuator::ShiftLeftAssign),
                                Range::new(&self.pop_position_from_stack(), &self.last_position),
                            ));
                        } else {
                            // `<<`
                            output.push(TokenWithRange::new(
                                Token::Punctuator(Punctuator::ShiftLeft),
                                Range::new(&self.pop_position_from_stack(), &self.last_position),
                            ));
                        }
                    } else {
                        // `<`
                        self.next_char(); // consume '<'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::LessThan),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '&' => {
                    if self.peek_char_and_equals(1, '&') {
                        // `&&`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '&'
                        self.next_char(); // consume '&'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::And),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '=') {
                        // `&=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '&'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BitwiseAndAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `&`
                        self.next_char(); // consume '&'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BitwiseAnd),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '|' => {
                    if self.peek_char_and_equals(1, '|') {
                        // `||`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '|'
                        self.next_char(); // consume '|'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Or),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else if self.peek_char_and_equals(1, '=') {
                        // `|=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '|'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BitwiseOrAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `|`
                        self.next_char(); // consume '|'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BitwiseOr),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '^' => {
                    if self.peek_char_and_equals(1, '=') {
                        // `^=`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '^'
                        self.next_char(); // consume '='

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BitwiseXorAssign),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `^`
                        self.next_char(); // consume '^'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BitwiseXor),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '~' => {
                    // `~`
                    self.next_char(); // consume '~'

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::BitwiseNot),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                '?' => {
                    if self.peek_char_and_equals(1, '?') {
                        // `??` Trigraphs (removed in C23)
                        //
                        // https://en.cppreference.com/w/c/language/operator_alternative.html
                        //
                        // | Primary | Trigraph |
                        // |---------|-----------|
                        // | `{` | `??<` |
                        // | `}` | `??>` |
                        // | `[` | `??(` |
                        // | `]` | `??)` |
                        // | `#` | `??=` |
                        // | `\` | `??/` |
                        // | `^` | `??'` |
                        // | `|` | `??!` |
                        // | `~` | `??-` |
                        if self
                            .peek_char_and_anyof(2, &['<', '>', '(', ')', '=', '/', '\'', '!', '-'])
                        {
                            return Err(PreprocessError::MessageWithRange(
                                "Trigraphs are disabled".to_owned(),
                                Range::from_position_and_length(self.peek_position(0).unwrap(), 3),
                            ));
                        } else {
                            return Err(PreprocessError::MessageWithRange(
                                "Unsupported token '??'".to_owned(),
                                Range::from_position_and_length(self.peek_position(0).unwrap(), 2),
                            ));
                        }
                    } else {
                        // `?`
                        self.next_char(); // consume '?'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::QuestionMark),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                ',' => {
                    // `,`
                    self.next_char(); // consume ','

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::Comma),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                '.' => {
                    if matches!(self.peek_char(1), Some('0'..='9')) {
                        // `.NUMBER`
                        // This is a number with a leading dot, such as `.5`, `.25f`, etc.
                        output.push(self.lex_decimal_number(true)?);
                    } else if self.peek_char_and_equals(1, '.') && self.peek_char_and_equals(2, '.')
                    {
                        // `...`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '.'
                        self.next_char(); // consume '.'
                        self.next_char(); // consume '.'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Ellipsis),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `.`
                        self.next_char(); // consume '.'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Dot),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '{' => {
                    // `{`
                    self.next_char(); // consume '{'

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::BraceOpen),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                '}' => {
                    // `}`
                    self.next_char(); // consume '}'

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::BraceClose),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                '[' => {
                    if self.peek_char_and_equals(1, '[') {
                        // `[[`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume '['
                        self.next_char(); // consume '['

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::AttributeOpen),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                    } else {
                        // `[`
                        self.next_char(); // consume '['

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BracketOpen),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                ']' => {
                    if self.peek_char_and_equals(1, ']') {
                        // `]]`
                        self.push_peek_position_into_stack();

                        self.next_char(); // consume ']'
                        self.next_char(); // consume ']'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::AttributeClose),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
                        ));
                        continue; // skip the next token
                    } else {
                        // `]`
                        self.next_char(); // consume ']'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::BracketClose),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '(' => {
                    // `(`
                    self.next_char(); // consume '('

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::ParenthesisOpen),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                ')' => {
                    // `)`
                    self.next_char(); // consume ')'

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::ParenthesisClose),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                ';' => {
                    // `;`
                    self.next_char(); // consume ';'

                    output.push(TokenWithRange::new(
                        Token::Punctuator(Punctuator::Semicolon),
                        Range::from_single_position(&self.last_position),
                    ));
                }
                ':' => {
                    if self.peek_char_and_equals(1, '>') {
                        // `:>`
                        // Alternative tokens (digraphs) are not supported.
                        return Err(PreprocessError::MessageWithRange(
                            "Digraphs are not supported.".to_owned(),
                            Range::from_position_and_length(self.peek_position(0).unwrap(), 2),
                        ));
                    } else {
                        // `:`
                        self.next_char(); // consume ':'

                        output.push(TokenWithRange::new(
                            Token::Punctuator(Punctuator::Colon),
                            Range::from_single_position(&self.last_position),
                        ));
                    }
                }
                '#' if self.peek_char_and_equals(1, '#') && self.in_directive_line() => {
                    // `##` in a directive line
                    self.push_peek_position_into_stack();

                    self.next_char(); // consume '#'
                    self.next_char(); // consume '#'

                    output.push(TokenWithRange::new(
                        Token::PoundPound,
                        Range::new(&self.pop_position_from_stack(), &self.last_position),
                    ));
                }
                '#' if output.len() == self.last_newline_token_index_1_based => {
                    // `#` at the beginning of a line or after a newline
                    self.next_char(); // consume '#'

                    self.last_directive_line_index = Some(self.last_position.line);

                    output.push(TokenWithRange::new(
                        Token::DirectiveStart,
                        Range::from_single_position(&self.last_position),
                    ));
                }
                '#' if self.in_directive_line() => {
                    // `#` in a directive line
                    self.next_char(); // consume '#'

                    output.push(TokenWithRange::new(
                        Token::Pound,
                        Range::from_single_position(&self.last_position),
                    ));
                }
                '0' if matches!(self.peek_char(1), Some('x' | 'X')) => {
                    // hexadecimal number
                    output.push(self.lex_hexadecimal_number()?);
                }
                '0' if matches!(self.peek_char(1), Some('b' | 'B')) => {
                    // binary number
                    output.push(self.lex_binary_number()?);
                }
                '0' if matches!(self.peek_char(1), Some('.')) => {
                    // decimal number
                    output.push(self.lex_decimal_number(false)?);
                }
                '0' => {
                    // octal number
                    output.push(self.lex_octal_number()?);
                }
                '1'..='9' => {
                    // decimal number
                    output.push(self.lex_decimal_number(false)?);
                }
                'L' | 'u' | 'U' if matches!(self.peek_char(1), Some('\'' | '"')) => {
                    // wide char / string literal
                    let prefix = *current_char;
                    let literal_char = match self.peek_char(1) {
                        Some('\'') => true,
                        Some('"') => false,
                        _ => unreachable!(),
                    };

                    let token_with_range = if literal_char {
                        let char_type = match prefix {
                            'L' => CharEncoding::Wide,
                            'u' => CharEncoding::UTF16,
                            'U' => CharEncoding::UTF32,
                            _ => unreachable!(),
                        };
                        self.lex_char(char_type, 1)?
                    } else {
                        let string_type = match prefix {
                            'L' => StringEncoding::Wide,
                            'u' => StringEncoding::UTF16,
                            'U' => StringEncoding::UTF32,
                            _ => unreachable!(),
                        };
                        self.lex_string(string_type, 1)?
                    };

                    output.push(token_with_range);
                }
                'u' if matches!(self.peek_char(1), Some('8'))
                    && matches!(self.peek_char(2), Some('\'' | '"')) =>
                {
                    // UTF-8 char / string literal
                    let literal_char = match self.peek_char(2) {
                        Some('\'') => true,
                        Some('"') => false,
                        _ => unreachable!(),
                    };

                    let token_with_range = if literal_char {
                        self.lex_char(CharEncoding::UTF8, 2)?
                    } else {
                        self.lex_string(StringEncoding::UTF8, 2)?
                    };

                    output.push(token_with_range);
                }
                'a'..='z' | 'A'..='Z' | '_' | '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
                    // Identifier token
                    let token_with_range = self.lex_identifier()?;

                    if self.is_directive_define(&output) && self.peek_char_and_equals(0, '(') {
                        // If the current line is `#define` directive, and the current identifier
                        // is followed by '(' immediately, it indicates that the identifier is a
                        // Function-like macro identifier.
                        if let TokenWithRange {
                            token: Token::Identifier(id),
                            range,
                        } = token_with_range
                        {
                            let function_like_macro_identifier =
                                Token::FunctionLikeMacroIdentifier(id);
                            output.push(TokenWithRange::new(function_like_macro_identifier, range));
                        } else {
                            unreachable!()
                        }
                    } else {
                        // Normal identifier token
                        output.push(token_with_range);
                    }
                }
                current_char => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Unexpected char '{}'.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        Ok(output)
    }

    fn lex_identifier(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // key_nameT  //
        // ^       ^__// to here
        // |__________// current char, validated
        //
        // T = terminator chars || EOF
        // ```

        let mut identifier_buffer = String::new();

        self.push_peek_position_into_stack();

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                '0'..='9' | 'a'..='z' | 'A'..='Z' | '_' => {
                    identifier_buffer.push(*current_char);
                    self.next_char(); // consume char
                }
                ':' if self.peek_char_and_equals(1, ':') => {
                    // `::` is used to indicate a namespace or module
                    identifier_buffer.push_str("::");
                    self.next_char(); // consume the 1st ":"
                    self.next_char(); // consume the 2nd ":"
                }
                '\u{a0}'..='\u{d7ff}' | '\u{e000}'..='\u{10ffff}' => {
                    // A char is a ‘Unicode scalar value’, which is any ‘Unicode code point’ other than a surrogate code point.
                    // This has a fixed numerical definition: code points are in the range 0 to 0x10FFFF,
                    // inclusive. Surrogate code points, used by UTF-16, are in the range 0xD800 to 0xDFFF.
                    //
                    // See:
                    // https://doc.rust-lang.org/std/primitive.char.html
                    //
                    // the range of CJK chars is '\u{4e00}'..='\u{9fff}',
                    // for complete CJK chars, check out Unicode standard "Ch. 18.1 Han - CJK Unified Ideographs".
                    // this is "Table 18-1. Blocks Containing Han Ideographs":
                    //
                    // | Block                                   | LocRange    | Comment |
                    // |-----------------------------------------|-------------|---------|
                    // | CJK Unified Ideographs                  | 4E00–9FFF   | Common                         |
                    // | CJK Unified Ideographs Extension A      | 3400–4DBF   | Rare                           |
                    // | CJK Unified Ideographs Extension B      | 20000–2A6DF | Rare, historic                 |
                    // | CJK Unified Ideographs Extension C      | 2A700–2B73F | Rare, historic                 |
                    // | CJK Unified Ideographs Extension D      | 2B740–2B81F | Uncommon, some in current use  |
                    // | CJK Unified Ideographs Extension E      | 2B820–2CEAF | Rare, historic                 |
                    // | CJK Unified Ideographs Extension F      | 2CEB0–2EBEF | Rare, historic                 |
                    // | CJK Unified Ideographs Extension G      | 30000–3134F | Rare, historic                 |
                    // | CJK Unified Ideographs Extension H      | 31350–323AF | Rare, historic                 |
                    // | CJK Compatibility Ideographs            | F900–FAFF   | Duplicates, unifiable variants, corporate characters |
                    // | CJK Compatibility Ideographs Supplement | 2F800–2FA1F | Unifiable variants             |
                    //
                    // see:
                    // - https://www.unicode.org/versions/Unicode15.0.0/ch18.pdf
                    // - https://en.wikipedia.org/wiki/CJK_Unified_Ideographs
                    // - https://www.unicode.org/versions/Unicode15.0.0/
                    // - https://www.unicode.org/reports/tr31/tr31-37.html

                    identifier_buffer.push(*current_char);
                    self.next_char(); // consume char
                }
                // whitespaces
                // https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
                '\t'        // tab, 0x09
                | '\n'      // line feed, 0x0A
                | '\u{0b}'  // vertical tab, 0x0B
                | '\u{0c}'  // form feed, 0x0C
                | '\r'      // carriage return, 0x0D
                // all ASCII punctuations
                | ' '..='/'
                | ':'..='@'
                | '['..='`'
                | '{'..='~' => {
                    // terminator, all punctuation and whitespace, per ASCII table
                    break;
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid char '{}' for identifier.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        let identifier_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        // unicode normalization
        let identifier_normalized: String = identifier_buffer.nfc().collect();

        Ok(TokenWithRange::new(
            Token::Identifier(identifier_normalized),
            identifier_range,
        ))
    }

    fn lex_decimal_number(&mut self, leading_dot: bool) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // 123456T  //
        // ^     ^__// to here
        // |________// current char, validated
        //
        // T = terminator chars || EOF
        //
        // Examples:
        // - 123
        // - 3.14
        // - 2.99e8
        // - 2.99e+8
        // - 6.672e-34
        // ```

        let mut number_buffer = String::new();
        let mut found_point = leading_dot; // to indicated whether char '.' is found
        let mut found_e = false; // to indicated whether char 'e' is found

        let mut is_unsigned = false;
        let mut integer_number_width = IntegerNumberWidth::Default;

        let mut is_decimal = false; // suffix 'dd', 'df', and 'dl'.
        let mut floating_point_number_width = FloatingPointNumberWidth::Default; // default floating-point number type is `double`

        self.push_peek_position_into_stack();

        if leading_dot {
            // if the number starts with a dot, e.g. `.5`, then we should add a leading zero
            number_buffer.push_str("0.");
            self.next_char(); // consumes '.'
        }

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                '0'..='9' => {
                    // valid digits for decimal number
                    number_buffer.push(*current_char);
                    self.next_char(); // Consumes digit
                }
                '\'' => {
                    self.next_char(); // Consumes '\''
                }
                '.' => {
                    if found_point {
                        return Err(PreprocessError::MessageWithPosition(
                            "Decimal number can not have multiple '.' characters.".to_owned(),
                            *self.peek_position(0).unwrap(),
                        ));
                    } else {
                        found_point = true;
                        number_buffer.push(*current_char);

                        self.next_char(); // consumes '.'
                    }
                }
                'e' | 'E' => {
                    if found_e {
                        return Err(PreprocessError::MessageWithPosition(
                            "Decimal number can not have multiple 'e' characters.".to_owned(),
                            *self.peek_position(0).unwrap(),
                        ));
                    } else {
                        found_e = true;

                        if self.peek_char_and_equals(1, '-') {
                            number_buffer.push_str("e-");
                            self.next_char(); // consumes 'e' or 'E'
                            self.next_char(); // consumes '-'
                        } else if self.peek_char_and_equals(1, '+') {
                            number_buffer.push_str("e+");
                            self.next_char(); // consumes 'e' or 'E'
                            self.next_char(); // consumes '+'
                        } else {
                            number_buffer.push('e');
                            self.next_char(); // consumes 'e' or 'E'
                        }
                    }
                }
                'u' | 'U' | 'l' | 'L' | 'w' | 'W' | 'f' | 'F' | 'd' | 'D' => {
                    if found_e || found_point {
                        // floating-point number suffixes
                        let suffix = self.lex_floating_point_number_suffix()?;
                        is_decimal = suffix.0;
                        floating_point_number_width = suffix.1;
                    } else {
                        // integer number suffix
                        let suffix = self.lex_integer_number_suffix()?;
                        is_unsigned = suffix.0;
                        integer_number_width = suffix.1;
                    }

                    break;
                }
                // whitespaces
                // https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
                '\t'        // tab, 0x09
                | '\n'      // line feed, 0x0A
                | '\u{0b}'  // vertical tab, 0x0B
                | '\u{0c}'  // form feed, 0x0C
                | '\r'      // carriage return, 0x0D
                // all ASCII punctuations
                | ' '..='/'
                | ':'..='@'
                | '['..='`'
                | '{'..='~' => {
                    // terminator, all punctuation and whitespace, per ASCII table
                    break;
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid char '{}' for decimal number.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        // check syntax
        if number_buffer.ends_with('e') {
            return Err(PreprocessError::MessageWithRange(
                "Decimal number can not ends with character \"e\".".to_owned(),
                Range::new(&self.pop_position_from_stack(), &self.last_position),
            ));
        }

        // normalize
        if number_buffer.ends_with('.') {
            // if the number ends with a dot, e.g. `1.`, then we should add a trailing zero
            number_buffer.push('0');
        }

        let number_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        if found_e || found_point {
            Ok(TokenWithRange::new(
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    number_buffer,
                    is_decimal,
                    floating_point_number_width,
                ))),
                number_range,
            ))
        } else {
            Ok(TokenWithRange::new(
                Token::Number(Number::Integer(IntegerNumber::new(
                    number_buffer,
                    is_unsigned,
                    integer_number_width,
                ))),
                number_range,
            ))
        }
    }

    fn lex_hexadecimal_number(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // 0xaabbT  //
        // ^^    ^__// to here
        // ||_______// validated
        // |________// current char, validated
        //
        // T = terminator chars || EOF
        //
        // Examples:
        // - 0x1a2b
        // - 0xBEEF
        // - 0x0.123p45
        // - 0x0.123p+45
        // - 0x0.123p-45
        // - 0x12p3
        // ```

        let mut number_buffer = String::new();
        let mut found_point: bool = false; // to indicated whether char '.' is found
        let mut found_p: bool = false; // to indicated whether char 'p' is found

        let mut is_unsigned = false;
        let mut integer_number_width = IntegerNumberWidth::Default;

        let mut is_decimal = false; // suffix 'dd', 'df', and 'dl'.
        let mut floating_point_number_width = FloatingPointNumberWidth::Default; // default floating-point number type is `double`

        // Save the start position of the hexadecimal number (i.e. the first '0')
        self.push_peek_position_into_stack();

        number_buffer.push_str("0x");
        self.next_char(); // Consumes '0'
        self.next_char(); // Consumes 'x'

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                'f' | 'F' | 'd' | 'D' if found_p => {
                    // 'f' is allowed only in the hex floating point literal mode,
                    // and the character 'p' should be present, e.g. 0x1.2p3f)
                    let suffix = self.lex_floating_point_number_suffix()?;
                    is_decimal = suffix.0;
                    floating_point_number_width = suffix.1;
                    break;
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    // valid digits for hex number
                    number_buffer.push(*current_char);
                    self.next_char(); // Consumes digit
                }
                '\'' => {
                    self.next_char(); // consumes '\''
                }
                '.' => {
                    if found_point {
                        return Err(PreprocessError::MessageWithPosition(
                            "Hexadecimal floating point number can not have multiple '.' characters.".to_owned(),
                            *self.peek_position(0).unwrap(),
                        ));
                    } else if found_p {
                        return Err(PreprocessError::MessageWithPosition(
                            "The exponent of a hexadecimal floating point number can not have a '.' character.".to_owned(),
                            *self.peek_position(0).unwrap(),
                        ));
                    } else {
                        // going to be hex floating point literal mode
                        found_point = true;

                        if number_buffer.len() == 2 {
                            // if the number starts with a dot, e.g. `0x.5`, then we should add a leading zero
                            number_buffer.push('0');
                        }
                        number_buffer.push(*current_char);

                        self.next_char(); // consumes '.'
                    }
                }
                'p' | 'P' => {
                    // hexadecimal floating point literal
                    // see:
                    // - https://en.cppreference.com/w/cpp/language/floating_literal.html
                    // - https://www.gnu.org/software/c-intro-and-ref/manual/html_node/Floating-Constants.html
                    // - https://en.cppreference.com/w/c/language/floating_constant.html
                    if found_p {
                        return Err(PreprocessError::MessageWithPosition(
                            "Hexadecimal floating point number can not have multiple 'p' characters.".to_owned(),
                            *self.peek_position(0).unwrap(),
                        ));
                    } else {
                        found_p = true;

                        // normalize
                        if number_buffer.ends_with('.') {
                            // if the number ends with a dot, e.g. `0x1.`, then we should add a trailing zero
                            number_buffer.push('0');
                        }

                        if self.peek_char_and_equals(1, '-') {
                            number_buffer.push_str("p-");
                            self.next_char(); // consumes 'p'
                            self.next_char(); // consumes '-'
                        } else if self.peek_char_and_equals(1, '+') {
                            number_buffer.push_str("p+");
                            self.next_char(); // consumes 'p'
                            self.next_char(); // consumes '+'
                        } else {
                            number_buffer.push('p');
                            self.next_char(); // consumes 'p'
                        }
                    }
                }
                'u' | 'U' | 'l' | 'L' | 'w' | 'W' => {
                    if found_p || found_point {
                        // floating-point number suffixes
                        let suffix = self.lex_floating_point_number_suffix()?;
                        is_decimal = suffix.0;
                        floating_point_number_width = suffix.1;
                    } else {
                        // integer number suffix
                        let suffix = self.lex_integer_number_suffix()?;
                        is_unsigned = suffix.0;
                        integer_number_width = suffix.1;
                    }

                    break;
                }
                // whitespaces
                // https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
                '\t'        // tab, 0x09
                | '\n'      // line feed, 0x0A
                | '\u{0b}'  // vertical tab, 0x0B
                | '\u{0c}'  // form feed, 0x0C
                | '\r'      // carriage return, 0x0D
                // all ASCII punctuations
                | ' '..='/'
                | ':'..='@'
                | '['..='`'
                | '{'..='~' => {
                    // terminator, all punctuation and whitespace, per ASCII table
                    break;
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid char '{}' for hexadecimal number.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        // empty number
        if number_buffer.len() <= 2 {
            return Err(PreprocessError::MessageWithRange(
                "Hexadecimal number must have at least one digit after '0x'.".to_owned(),
                Range::new(&self.pop_position_from_stack(), &self.last_position),
            ));
        }

        // check syntax
        // missing exponent 'p'
        if found_point && !found_p {
            return Err(PreprocessError::MessageWithRange(
                "Hexadecimal floating point number must have an exponent 'p'.".to_owned(),
                Range::new(&self.pop_position_from_stack(), &self.last_position),
            ));
        }

        // empty exponent
        if number_buffer.ends_with('p') {
            return Err(PreprocessError::MessageWithRange(
                "The exponent of a hexadecimal floating point number can not be empty.".to_owned(),
                Range::new(&self.pop_position_from_stack(), &self.last_position),
            ));
        }

        let number_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        if found_p {
            Ok(TokenWithRange::new(
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    number_buffer,
                    is_decimal,
                    floating_point_number_width,
                ))),
                number_range,
            ))
        } else {
            Ok(TokenWithRange::new(
                Token::Number(Number::Integer(IntegerNumber::new(
                    number_buffer,
                    is_unsigned,
                    integer_number_width,
                ))),
                number_range,
            ))
        }
    }

    fn lex_binary_number(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // 0b101010T  //
        // ^^      ^__// to here
        // ||________// validated
        // |_________// current char, validated
        //
        // T = terminator chars || EOF
        // ```

        let mut number_buffer = String::new();
        let mut is_unsigned = false;
        let mut integer_number_width = IntegerNumberWidth::Default;

        // Save the start position of the binary number (i.e. the first '0')
        self.push_peek_position_into_stack();

        number_buffer.push_str("0b");
        self.next_char(); // Consumes '0'
        self.next_char(); // Consumes 'b'

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                '0' | '1' => {
                    // valid digits for binary number
                    number_buffer.push(*current_char);
                    self.next_char(); // Consumes digit
                }
                '\'' => {
                    self.next_char(); // Consumes '\''
                }
                'u' | 'U' | 'l' | 'L' | 'w' | 'W' => {
                    // integer suffix
                    let suffix = self.lex_integer_number_suffix()?;
                    is_unsigned = suffix.0;
                    integer_number_width = suffix.1;
                    break;
                }
                '2'..='9' | '.' | 'e' | 'E' | 'p' | 'P' => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid digit '{}' for binary number.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                // whitespaces
                // https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
                '\t'        // tab, 0x09
                | '\n'      // line feed, 0x0A
                | '\u{0b}'  // vertical tab, 0x0B
                | '\u{0c}'  // form feed, 0x0C
                | '\r'      // carriage return, 0x0D
                // all ASCII punctuations
                | ' '..='/'
                | ':'..='@'
                | '['..='`'
                | '{'..='~' => {
                    // terminator, all punctuation and whitespace, per ASCII table
                    break;
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid char '{}' for binary number.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        if number_buffer.len() <= 2 {
            return Err(PreprocessError::MessageWithRange(
                "Binary number must have at least one digit after \"0b\".".to_owned(),
                Range::new(&self.pop_position_from_stack(), &self.last_position),
            ));
        }

        let number_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        Ok(TokenWithRange::new(
            Token::Number(Number::Integer(IntegerNumber::new(
                number_buffer,
                is_unsigned,
                integer_number_width,
            ))),
            number_range,
        ))
    }

    fn lex_octal_number(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // 01234567T  //
        // ^^      ^__// to here
        // ||_________// next char, it is not 'x', 'b', '.', validated
        // |__________// current char, it is '0', validated
        //
        // T = terminator chars || EOF
        // ```

        // Save the start position of the octal number (i.e. the first '0')
        self.push_peek_position_into_stack();

        let mut number_buffer = String::new();
        let mut is_unsigned = false;
        let mut integer_number_width = IntegerNumberWidth::Default;

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                '0'..='7' => {
                    // valid digits for octal number
                    number_buffer.push(*current_char);
                    self.next_char(); // Consumes digit
                }
                '\'' => {
                    self.next_char(); // Consumes '\''
                }
                'u' | 'U' | 'l' | 'L' | 'w' | 'W' => {
                    // integer suffix
                    let suffix = self.lex_integer_number_suffix()?;
                    is_unsigned = suffix.0;
                    integer_number_width = suffix.1;
                    break;
                }
                '8'..='9' | '.' | 'e' | 'E' => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid digit '{}' for octal number.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                // whitespaces
                // https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
                '\t'        // tab, 0x09
                | '\n'      // line feed, 0x0A
                | '\u{0b}'  // vertical tab, 0x0B
                | '\u{0c}'  // form feed, 0x0C
                | '\r'      // carriage return, 0x0D
                // all ASCII punctuations
                | ' '..='/'
                | ':'..='@'
                | '['..='`'
                | '{'..='~' => {
                    // terminator, all punctuation and whitespace, per ASCII table
                    break;
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        format!("Invalid char '{}' for octal number.", current_char),
                        *self.peek_position(0).unwrap(),
                    ));
                }
            }
        }

        let num_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        Ok(TokenWithRange::new(
            Token::Number(Number::Integer(IntegerNumber::new(
                number_buffer,
                is_unsigned,
                integer_number_width,
            ))),
            num_range,
        ))
    }

    fn lex_integer_number_suffix(
        &mut self,
    ) -> Result<(/* is_unsigned */ bool, IntegerNumberWidth), PreprocessError> {
        // integer number suffixes consist of the two groups:
        // - group 1: `u`, `U`
        // - group 2: `l`, `L`, `ll`, `LL`, `wb`, `WB`
        // Both groups are optional, and can be combined.

        // to indicate whether the number is unsigned
        let mut is_unsigned = false;

        // to indicate the integer number type
        let mut integer_number_width = IntegerNumberWidth::Default;

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                'u' | 'U' => {
                    is_unsigned = true;
                    self.next_char(); // consume char 'u' or 'U'
                }
                'l' if self.peek_char_and_equals(1, 'l') => {
                    self.next_char(); // consume char 'l'
                    self.next_char(); // consume char 'l'
                    integer_number_width = IntegerNumberWidth::LongLong;
                }
                'L' if self.peek_char_and_equals(1, 'L') => {
                    self.next_char(); // consume char 'L'
                    self.next_char(); // consume char 'L'
                    integer_number_width = IntegerNumberWidth::LongLong;
                }
                'l' => {
                    self.next_char(); // consume char 'l'
                    integer_number_width = IntegerNumberWidth::Long;
                }
                'L' => {
                    self.next_char(); // consume char 'L'
                    integer_number_width = IntegerNumberWidth::Long;
                }
                'w' if self.peek_char_and_equals(1, 'b') => {
                    self.next_char(); // consume char 'w'
                    self.next_char(); // consume char 'b'
                    integer_number_width = IntegerNumberWidth::BitInt;
                }
                'W' if self.peek_char_and_equals(1, 'B') => {
                    self.next_char(); // consume char 'W'
                    self.next_char(); // consume char 'B'
                    integer_number_width = IntegerNumberWidth::BitInt;
                }
                'd' | 'D' => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Integer number can not have \"dd\", \"df\", or \"dl\" suffix.".to_owned(),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                'f' | 'F' => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Integer number can not have 'f' or 'F' suffix.".to_owned(),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                _ => {
                    break;
                }
            }
        }

        Ok((is_unsigned, integer_number_width))
    }

    fn lex_floating_point_number_suffix(
        &mut self,
    ) -> Result<(/* is_decimal */ bool, FloatingPointNumberWidth), PreprocessError> {
        // possible suffix for floating-point number:
        // `f`, `F`, `l`, `L`, `dd`, `df`, `dl`, `DD`, `DF`, `DL`

        // to indicate whether the number is decimal
        let mut is_decimal = false;

        // to indicate the floating-point number type
        let mut floating_point_number_width = FloatingPointNumberWidth::Default; // default floating-point number type is `double`

        while let Some(current_char) = self.peek_char(0) {
            match current_char {
                'd' if self.peek_char_and_equals(1, 'd') => {
                    self.next_char(); // consume char 'd'
                    self.next_char(); // consume char 'd'
                    is_decimal = true;
                    floating_point_number_width = FloatingPointNumberWidth::Default;
                }
                'D' if self.peek_char_and_equals(1, 'D') => {
                    self.next_char(); // consume char 'D'
                    self.next_char(); // consume char 'D'
                    is_decimal = true;
                    floating_point_number_width = FloatingPointNumberWidth::Default;
                }
                'd' if self.peek_char_and_equals(1, 'f') => {
                    self.next_char(); // consume char 'd'
                    self.next_char(); // consume char 'f'
                    is_decimal = true;
                    floating_point_number_width = FloatingPointNumberWidth::Float;
                }
                'D' if self.peek_char_and_equals(1, 'F') => {
                    self.next_char(); // consume char 'D'
                    self.next_char(); // consume char 'F'
                    is_decimal = true;
                    floating_point_number_width = FloatingPointNumberWidth::Float;
                }
                'd' if self.peek_char_and_equals(1, 'l') => {
                    self.next_char(); // consume char 'd'
                    self.next_char(); // consume char 'l'
                    is_decimal = true;
                    floating_point_number_width = FloatingPointNumberWidth::LongDouble;
                }
                'D' if self.peek_char_and_equals(1, 'L') => {
                    self.next_char(); // consume char 'd'
                    self.next_char(); // consume char 'l'
                    is_decimal = true;
                    floating_point_number_width = FloatingPointNumberWidth::LongDouble;
                }
                'f' | 'F' => {
                    self.next_char(); // consumes char 'f' or 'F'
                    floating_point_number_width = FloatingPointNumberWidth::Float;
                }
                'u' | 'U' => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Floating-point number can not have 'u' or 'U' suffix.".to_owned(),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                'l' if self.peek_char_and_equals(1, 'l') => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Floating-point number can not have 'll' or 'LL' suffix.".to_owned(),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                'L' if self.peek_char_and_equals(1, 'L') => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Floating-point number can not have 'll' or 'LL' suffix.".to_owned(),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                'l' | 'L' => {
                    self.next_char(); // consumes char 'l' or 'L'
                    floating_point_number_width = FloatingPointNumberWidth::LongDouble;
                }
                'w' | 'W' => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Floating-point number can not have \"wb\" or \"WB\" suffix.".to_owned(),
                        *self.peek_position(0).unwrap(),
                    ));
                }
                _ => {
                    break;
                }
            }
        }

        Ok((is_decimal, floating_point_number_width))
    }

    fn lex_char(
        &mut self,
        char_type: CharEncoding,
        prefix_length: usize,
    ) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // 'a'?  //
        // ^  ^__// to here
        // |_____// current char, validated
        // ```

        // save the start position of the char literal (i.e. the first "'")
        self.push_peek_position_into_stack();

        // consume the prefix characters, e.g. 'L', 'u', 'U', "u8"
        for _ in 0..prefix_length {
            self.next_char();
        }

        self.next_char(); // Consumes "'"

        let character = match self.next_char() {
            Some(current_char) => {
                match current_char {
                    '\\' => {
                        // save the start position of the escape sequence (i.e. the "\" char)
                        self.push_last_position_into_stack();

                        // escape chars.
                        // see:
                        // - https://en.cppreference.com/w/c/language/escape.html
                        // - https://en.wikipedia.org/wiki/Escape_sequences_in_C#Escape_sequences
                        let escaped_char = match self.next_char() {
                            Some(current_char2) => {
                                match current_char2 {
                                    'a' => '\x07', // bell (BEL, ascii 7)
                                    'b' => '\x08', // backspace (BS, ascii 8)
                                    't' => '\t',   // horizontal tabulation (HT, ascii 9)
                                    'n' => '\n',   // new line character (line feed, LF, ascii 10)
                                    'v' => '\x0b', // vertical tabulation (VT, ascii 11)
                                    'f' => '\x0c', // form feed (FF, ascii 12)
                                    'r' => '\r',   // carriage return (CR, ascii 13)
                                    // 'e' => '\x1b', // escape character (ESC, ascii 27) // non-standard escape sequence
                                    '\\' => '\\', // backslash
                                    '\'' => '\'', // single quote
                                    '"' => '"',   // double quote
                                    '?' => '?',   // question mark (?), used to avoid trigraphs
                                    '0'..='7' => {
                                        // Octal escape sequence.
                                        // format: `\o`, `\oo`, and `\ooo`.
                                        // e.g. `\0`, `\70` (`'8'`), `\101` (`'A'`)
                                        // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Octal

                                        const MAX_OCTAL_DIGITS: usize = 3; // max 3 octal digits

                                        let mut buffer = String::new();
                                        buffer.push(current_char2);

                                        while let Some(next_char) = self.peek_char(0) {
                                            match next_char {
                                                '0'..='7' => {
                                                    buffer.push(*next_char);
                                                    self.next_char(); // consume digit

                                                    if buffer.len() >= MAX_OCTAL_DIGITS {
                                                        break;
                                                    }
                                                }
                                                '8' | '9' => {
                                                    // invalid octal digit
                                                    return Err(PreprocessError::MessageWithRange(
                                                        format!(
                                                            "Invalid octal escape sequence '\\{}'. Expected octal digit.",
                                                            buffer
                                                        ),
                                                        Range::new(
                                                            &self.pop_position_from_stack(),
                                                            &self.last_position,
                                                        ),
                                                    ));
                                                }
                                                _ => {
                                                    break;
                                                }
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
                                                        &self.pop_position_from_stack(),
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
                                                    &self.pop_position_from_stack(),
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
                                                            &self.pop_position_from_stack(),
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
                                                        &self.pop_position_from_stack(),
                                                        &self.last_position,
                                                    ),
                                                ));
                                            }
                                        }
                                    }
                                    'x' | 'X' => {
                                        // Hexadecimal escape sequence,
                                        // format: `\xh...`.
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
                                                    &self.pop_position_from_stack(),
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
                                                        &self.pop_position_from_stack(),
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
                        self.pop_position_from_stack();

                        // return the escaped character
                        escaped_char
                    }
                    '\'' => {
                        // Empty char (`''`).
                        return Err(PreprocessError::MessageWithRange(
                            "Empty character is not allowed.".to_owned(),
                            Range::new(&self.pop_position_from_stack(), &self.last_position),
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

        let character_range = Range::new(&self.pop_position_from_stack(), &self.last_position);
        Ok(TokenWithRange::new(
            Token::Char(character, char_type),
            character_range,
        ))
    }

    fn lex_string(
        &mut self,
        string_encoding: StringEncoding,
        prefix_length: usize,
    ) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // "abc"?  //
        // ^    ^__// to here
        // |_______// current char, validated
        // ```

        // save the start position of the string literal (i.e. the first '"')
        self.push_peek_position_into_stack();

        // consume the prefix characters, e.g. 'L', 'u', 'U', "u8"
        for _ in 0..prefix_length {
            self.next_char();
        }

        self.next_char(); // Consumes '"'

        let mut string_buffer = String::new();

        loop {
            match self.next_char() {
                Some(current_char) => {
                    match current_char {
                        '\\' => {
                            // save the start position of the escape sequence (i.e. the "\" char)
                            self.push_last_position_into_stack();

                            // escape chars.
                            // see:
                            // - https://en.cppreference.com/w/c/language/escape.html
                            // - https://en.wikipedia.org/wiki/Escape_sequences_in_C#Escape_sequences
                            let escaped_char = match self.next_char() {
                                Some(current_char2) => {
                                    match current_char2 {
                                        'a' => '\x07', // bell (BEL, ascii 7)
                                        'b' => '\x08', // backspace (BS, ascii 8)
                                        't' => '\t',   // horizontal tabulation (HT, ascii 9)
                                        'n' => '\n', // new line character (line feed, LF, ascii 10)
                                        'v' => '\x0b', // vertical tabulation (VT, ascii 11)
                                        'f' => '\x0c', // form feed (FF, ascii 12)
                                        'r' => '\r', // carriage return (CR, ascii 13)
                                        // 'e' => '\x1b', // escape character (ESC, ascii 27) // non-standard escape sequence
                                        '\\' => '\\', // backslash
                                        '\'' => '\'', // single quote
                                        '"' => '"',   // double quote
                                        '?' => '?',   // question mark (?), used to avoid trigraphs
                                        '0'..='7' => {
                                            // Octal escape sequence.
                                            // format: `\o`, `\oo`, and `\ooo`.
                                            // e.g. `\0`, `\70` (`'8'`), `\101` (`'A'`)
                                            // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Octal

                                            const MAX_OCTAL_DIGITS: usize = 3; // max 3 octal digits

                                            // Octal escape sequence buffer
                                            let mut buffer = String::new();
                                            buffer.push(current_char2);

                                            while let Some(next_char) = self.peek_char(0) {
                                                match next_char {
                                                    '0'..='7' => {
                                                        buffer.push(*next_char);
                                                        self.next_char(); // consume digit

                                                        if buffer.len() >= MAX_OCTAL_DIGITS {
                                                            break;
                                                        }
                                                    }
                                                    '8' | '9' => {
                                                        // invalid octal digit
                                                        return Err(
                                                            PreprocessError::MessageWithRange(
                                                                format!(
                                                                    "Invalid octal escape sequence '\\{}'. Expected octal digit.",
                                                                    buffer
                                                                ),
                                                                Range::new(
                                                                    &self.pop_position_from_stack(),
                                                                    &self.last_position,
                                                                ),
                                                            ),
                                                        );
                                                    }
                                                    _ => {
                                                        break;
                                                    }
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
                                                            &self.pop_position_from_stack(),
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

                                            // Unicode escape sequence buffer
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
                                                        &self.pop_position_from_stack(),
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
                                                                    &self.pop_position_from_stack(),
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
                                                            &self.pop_position_from_stack(),
                                                            &self.last_position,
                                                        ),
                                                    ));
                                                }
                                            }
                                        }
                                        'x' | 'X' => {
                                            // Hexadecimal escape sequence,
                                            // format: `\xh...`.
                                            // e.g. `\x38` (`'8'`), `\x41` (`'A'`)
                                            // see: https://en.wikipedia.org/wiki/Escape_sequences_in_C#Hex

                                            const HEX_DIGITS: usize = 2; // only format `\xhh` is supported

                                            // Hexadecimal escape sequence buffer
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
                                                        &self.pop_position_from_stack(),
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
                                                            &self.pop_position_from_stack(),
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
                            self.pop_position_from_stack();

                            string_buffer.push(escaped_char);
                        }
                        '"' => {
                            // encounter the closing double quote, which
                            // means the end of the string literal.
                            break;
                        }
                        _ => {
                            // ordinary char
                            string_buffer.push(current_char);
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

        let string_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        Ok(TokenWithRange::new(
            Token::String(string_buffer, string_encoding),
            string_range,
        ))
    }

    fn lex_quoted_file_path(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // "path"?  //
        // ^    ^___// to here
        // |________// current char, validated
        // ```

        // save the start position of the file path (i.e. the first '"')
        self.push_peek_position_into_stack();

        let mut file_path_buffer = String::new();

        self.next_char(); // Consumes '"'

        loop {
            if let Some(current_char) = self.next_char() {
                match current_char {
                    '"' => {
                        break;
                    }
                    '\n' | '\r' => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Incomplete file path, expected a closing double quote '\"' but found a newline character."
                                .to_owned(),
                            self.last_position,
                        ));
                    }
                    _ => {
                        file_path_buffer.push(current_char);
                    }
                }
            } else {
                // Incomplete file path (`<...EOF` or `"...EOF`).
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Incomplete file path.".to_owned(),
                ));
            }
        }

        let file_path_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        Ok(TokenWithRange::new(
            Token::FilePath(file_path_buffer, false),
            file_path_range,
        ))
    }

    fn lex_angle_bracket_file_path(&mut self) -> Result<TokenWithRange, PreprocessError> {
        // ```diagram
        // <path>?  //
        // ^    ^___// to here
        // |________// current char, validated
        // ```

        // save the start position of the file path (i.e. the first '<')
        self.push_peek_position_into_stack();

        let mut file_path_buffer = String::new();

        self.next_char(); // Consumes '<'

        loop {
            if let Some(current_char) = self.next_char() {
                match current_char {
                    '>' => {
                        break;
                    }
                    '\n' | '\r' => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Incomplete file path, expected a closing angle bracket '>' but found a newline character."
                                .to_owned(),
                            self.last_position,
                        ));
                    }
                    _ => {
                        file_path_buffer.push(current_char);
                    }
                }
            } else {
                // Incomplete file path (`<...EOF`).
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Incomplete file path.".to_owned(),
                ));
            }
        }

        let file_path_range = Range::new(&self.pop_position_from_stack(), &self.last_position);

        Ok(TokenWithRange::new(
            Token::FilePath(file_path_buffer, true),
            file_path_range,
        ))
    }

    /// Checks whether the last token in the provided token list is a directive `define`.
    ///
    /// This is used to identify function-like macro definitions, which require the macro name to be
    /// immediately followed by an opening parenthesis with no intervening whitespace.
    fn is_directive_define(&self, token_with_ranges: &[TokenWithRange]) -> bool {
        // Check for patterns like:
        // `# define`

        let length = token_with_ranges.len();
        length >= 2
            && matches!(
                token_with_ranges.get(length - 2),
                Some(TokenWithRange {
                    token: Token::DirectiveStart,
                    ..
                })
            )
            && matches!(
                token_with_ranges.last(),
                Some(TokenWithRange { token: Token::Identifier(id), ..}) if id == "define"
            )
    }

    /// Checks whether we are currently in a directive line.
    fn in_directive_line(&self) -> bool {
        // If the last directive line index matches the current line,
        // then we are in a directive line.
        matches!(self.last_directive_line_index, Some(idx) if idx == self.last_position.line)
    }

    /// Checks whether the last token in the provided token list is a directive `include` or `embed`.
    fn in_inclusion_directive(&self, token_with_ranges: &[TokenWithRange]) -> bool {
        // To check for patterns like:
        // - `# include ...`
        // - `# embed ...`

        let length = token_with_ranges.len();
        length >= 2
            && matches!(
                token_with_ranges.get(length - 2),
                Some(TokenWithRange {
                    token: Token::DirectiveStart,
                    ..
                })
            )
            && matches!(
                token_with_ranges.last(),
                Some(TokenWithRange { token: Token::Identifier(id), ..}) if id == "include" || id == "embed"
            )
    }

    /// Checks whether the last token in the provided token list is a operator `__has_include` or `__has_embed`.
    fn in_inclusion_operator(&self, token_with_ranges: &[TokenWithRange]) -> bool {
        // To check for patterns like:
        // - `#... __has_include (`
        // - `#... __has_embed (`

        if !self.in_directive_line() {
            return false;
        }

        let length = token_with_ranges.len();
        length >= 2
            && matches!(
                token_with_ranges.get(length - 2),
                Some(TokenWithRange { token: Token::Identifier(id), ..}) if id == "__has_include" || id == "__has_embed"
            )
            && matches!(
                token_with_ranges.last(),
                Some(TokenWithRange {
                    token: Token::Punctuator(Punctuator::ParenthesisOpen),
                    ..
                })
            )
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        error::PreprocessError,
        lexer::{TokenWithRange, lex_from_str},
        position::Position,
        range::Range,
        token::{
            CharEncoding, FloatingPointNumber, FloatingPointNumberWidth, IntegerNumber,
            IntegerNumberWidth, Number, Punctuator, StringEncoding, Token,
        },
    };

    // Helper functions to create tokens for testing
    impl Token {
        fn new_identifier(s: &str) -> Self {
            Token::Identifier(s.to_owned())
        }

        fn new_char(c: char) -> Self {
            Token::Char(c, CharEncoding::Default)
        }

        fn new_string(s: &str) -> Self {
            Token::String(s.to_owned(), StringEncoding::Default)
        }

        fn new_integer_number(i: i32) -> Self {
            Token::Number(Number::Integer(IntegerNumber::new(
                i.to_string(),
                false,
                IntegerNumberWidth::Default,
            )))
        }
    }

    fn lex_from_str_with_range_strip(s: &str) -> Result<Vec<Token>, PreprocessError> {
        let tokens = lex_from_str(s)?
            .into_iter()
            .map(|e| e.token)
            .collect::<Vec<Token>>();
        Ok(tokens)
    }

    #[test]
    // https://en.cppreference.com/w/c/language/operator_alternative.html
    fn test_trigraphs() {
        let source_text0 = "do ??< 123";
        // "do ??< 123"
        //  0123456789  // index

        let result0 = lex_from_str(source_text0);
        assert!(matches!(
            result0,
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 3,
                        line: 0,
                        column: 3
                    },
                    end_included: Position {
                        index: 5,
                        line: 0,
                        column: 5
                    },
                }
            ))
        ));

        let source_text1 = "val ?? 123";
        // chars index:           0123456789

        let result1 = lex_from_str(source_text1);
        assert!(matches!(
            result1,
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 4,
                        line: 0,
                        column: 4
                    },
                    end_included: Position {
                        index: 5,
                        line: 0,
                        column: 5
                    },
                }
            ))
        ));

        let trigraphs = vec![
            "??<", "??>", "??(", "??)", "??=", "??/", "??'", "??!", "??-",
        ];

        for trigraph in trigraphs {
            let result = lex_from_str(trigraph);
            assert!(result.is_err());
        }
    }

    #[test]
    // https://en.cppreference.com/w/c/language/operator_alternative.html
    fn test_digraphs() {
        let source = "if 1 <% return";

        // "if 1 <% return"
        //  01234567890123  // index

        let result = lex_from_str_with_range_strip(source);
        assert!(matches!(
            result,
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 5,
                        line: 0,
                        column: 5
                    },
                    end_included: Position {
                        index: 6,
                        line: 0,
                        column: 6
                    },
                }
            ))
        ));

        let digraphs = vec!["<%", "%>", "<:", ":>", "%:", "%:%:"];

        for digraph in digraphs {
            let result = lex_from_str(digraph);
            assert!(result.is_err());
        }
    }

    #[test]
    fn test_lex_punctuator() {
        assert_eq!(
            lex_from_str_with_range_strip("+-*/%++--==!=><>=<=&&||!&|^~<<>>").unwrap(),
            vec![
                // Arithmetic Operators
                Token::Punctuator(Punctuator::Add),
                Token::Punctuator(Punctuator::Subtract),
                Token::Punctuator(Punctuator::Multiply),
                Token::Punctuator(Punctuator::Divide),
                Token::Punctuator(Punctuator::Modulo),
                Token::Punctuator(Punctuator::Increase),
                Token::Punctuator(Punctuator::Decrease),
                // Relational Operators
                Token::Punctuator(Punctuator::Equal,),
                Token::Punctuator(Punctuator::NotEqual,),
                Token::Punctuator(Punctuator::GreaterThan,),
                Token::Punctuator(Punctuator::LessThan,),
                Token::Punctuator(Punctuator::GreaterThanOrEqual,),
                Token::Punctuator(Punctuator::LessThanOrEqual,),
                // Logical Operators
                Token::Punctuator(Punctuator::And,),
                Token::Punctuator(Punctuator::Or,),
                Token::Punctuator(Punctuator::Not,),
                // Bitwise Operators
                Token::Punctuator(Punctuator::BitwiseAnd,),
                Token::Punctuator(Punctuator::BitwiseOr,),
                Token::Punctuator(Punctuator::BitwiseXor,),
                Token::Punctuator(Punctuator::BitwiseNot,),
                Token::Punctuator(Punctuator::ShiftLeft),
                Token::Punctuator(Punctuator::ShiftRight),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("=+=-=*=/=%=&=|=^=<<=>>=").unwrap(),
            vec![
                // Assignment Operators
                Token::Punctuator(Punctuator::Assign),
                Token::Punctuator(Punctuator::AddAssign),
                Token::Punctuator(Punctuator::SubtractAssign),
                Token::Punctuator(Punctuator::MultiplyAssign),
                Token::Punctuator(Punctuator::DivideAssign),
                Token::Punctuator(Punctuator::ModulusAssign),
                Token::Punctuator(Punctuator::BitwiseAndAssign),
                Token::Punctuator(Punctuator::BitwiseOrAssign),
                Token::Punctuator(Punctuator::BitwiseXorAssign),
                Token::Punctuator(Punctuator::ShiftLeftAssign),
                Token::Punctuator(Punctuator::ShiftRightAssign),
            ]
        );

        // Brackets and Delimiters
        assert_eq!(
            lex_from_str_with_range_strip("{}[]()[[]];,").unwrap(),
            vec![
                Token::Punctuator(Punctuator::BraceOpen),
                Token::Punctuator(Punctuator::BraceClose),
                Token::Punctuator(Punctuator::BracketOpen),
                Token::Punctuator(Punctuator::BracketClose),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::Punctuator(Punctuator::ParenthesisClose),
                Token::Punctuator(Punctuator::AttributeOpen),
                Token::Punctuator(Punctuator::AttributeClose),
                Token::Punctuator(Punctuator::Semicolon),
                Token::Punctuator(Punctuator::Comma),
            ]
        );

        // Other Operators
        assert_eq!(
            lex_from_str_with_range_strip(".->?:...").unwrap(),
            vec![
                Token::Punctuator(Punctuator::Dot),
                Token::Punctuator(Punctuator::Arrow),
                Token::Punctuator(Punctuator::QuestionMark),
                Token::Punctuator(Punctuator::Colon),
                Token::Punctuator(Punctuator::Ellipsis),
            ]
        );

        // Test location

        // Test punctuations `>>>=`,
        // it should be divided into  `>>` and `>=`.
        assert_eq!(
            lex_from_str(">>>=").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ShiftRight),
                    Range::from_detail(0, 0, 0, 2),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::GreaterThanOrEqual),
                    Range::from_detail(2, 0, 2, 2),
                ),
            ]
        );

        // Test punctuations `++++=`,
        // it should be divided into  `++`, `++` and `=`.
        assert_eq!(
            lex_from_str("++++=").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Increase),
                    Range::from_detail(0, 0, 0, 2),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Increase),
                    Range::from_detail(2, 0, 2, 2),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Assign),
                    Range::from_detail(4, 0, 4, 1),
                ),
            ]
        );

        // Test punctuations `>====`,
        // it should be divided into  `>=`, `==`, and `=`.
        assert_eq!(
            lex_from_str(">====").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::GreaterThanOrEqual),
                    Range::from_detail(0, 0, 0, 2),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Equal),
                    Range::from_detail(2, 0, 2, 2),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Assign),
                    Range::from_detail(4, 0, 4, 1),
                ),
            ]
        );

        // Test punctuations `.....`,
        // it should be divided into  `...`, `.`, and `.`.
        assert_eq!(
            lex_from_str(".....").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Ellipsis),
                    Range::from_detail(0, 0, 0, 3),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Dot),
                    Range::from_detail(3, 0, 3, 1),
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::Dot),
                    Range::from_detail(4, 0, 4, 1),
                ),
            ]
        );
    }

    #[test]
    fn test_lex_whitespaces() {
        assert_eq!(lex_from_str_with_range_strip("  ").unwrap(), vec![]);

        assert_eq!(
            lex_from_str_with_range_strip("()").unwrap(),
            vec![
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::Punctuator(Punctuator::ParenthesisClose)
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("(  )").unwrap(),
            vec![
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::Punctuator(Punctuator::ParenthesisClose)
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("(\t\r\n\n\n)").unwrap(),
            vec![
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::Punctuator(Punctuator::ParenthesisClose)
            ]
        );

        // Test location

        assert_eq!(
            lex_from_str("()").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ParenthesisOpen),
                    Range::from_detail(0, 0, 0, 1)
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ParenthesisClose),
                    Range::from_detail(1, 0, 1, 1)
                ),
            ]
        );

        assert_eq!(
            lex_from_str("(  )").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ParenthesisOpen),
                    Range::from_detail(0, 0, 0, 1)
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ParenthesisClose),
                    Range::from_detail(3, 0, 3, 1)
                ),
            ]
        );
    }

    #[test]
    fn test_lex_decimal_number() {
        assert_eq!(
            lex_from_str_with_range_strip("211").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                211.to_string(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        assert_eq!(
            lex_from_str_with_range_strip("20'25").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                2025.to_string(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        // Test location

        assert_eq!(
            lex_from_str("223 211").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        223.to_string(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(0, 0, 0, 3)
                ),
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        211.to_string(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(4, 0, 4, 3)
                ),
            ]
        );

        // err: invalid char for decimal number
        assert!(matches!(
            lex_from_str_with_range_strip("1234x"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_decimal_number_with_suffix() {
        assert_eq!(
            lex_from_str_with_range_strip("1u 2U").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "1".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "2".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                )))
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("1l 2L 3ll 4LL 5wb 6WB").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "1".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "2".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "3".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "4".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "5".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "6".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("1ul 2UL 3ull 4ULL 5uwb 6UWB").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "1".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "2".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "3".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "4".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "5".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "6".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        // Test `lu`, `LU`, `llu`, `LLU`, `wbu`, `WBU` suffixes.

        assert_eq!(
            lex_from_str_with_range_strip("1lu 2LU 3llu 4LLU 5wbu 6WBU").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "1".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "2".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "3".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "4".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "5".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "6".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        // err: invalid suffix for integer number
        assert!(matches!(
            lex_from_str_with_range_strip("13f"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 2,
                    line: 0,
                    column: 2,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_floating_point_decimal_number() {
        assert_eq!(
            lex_from_str_with_range_strip("3.14").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "3.14".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        assert_eq!(
            lex_from_str_with_range_strip("1.4'14").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "1.414".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // floating-point number with exponent
        assert_eq!(
            lex_from_str_with_range_strip("2.998e8").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "2.998e8".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // floating-point number with exponent and sign
        assert_eq!(
            lex_from_str_with_range_strip("2.998e+8").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "2.998e+8".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // floating-point number with exponent and negative sign
        assert_eq!(
            lex_from_str_with_range_strip("6.626e-34").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "6.626e-34".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // automatically complete the floating-point number
        // by insert '0' before the point '.'
        assert_eq!(
            lex_from_str_with_range_strip(".5").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0.5".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // automatically complete the floating-point number
        // by appending '0' after the point '.'
        assert_eq!(
            lex_from_str_with_range_strip("5.").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "5.0".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // Test location

        assert_eq!(
            lex_from_str("3.14 6.022e23").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                        "3.14".to_owned(),
                        false,
                        FloatingPointNumberWidth::Default
                    ))),
                    Range::from_detail(0, 0, 0, 4)
                ),
                TokenWithRange::new(
                    Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                        "6.022e23".to_owned(),
                        false,
                        FloatingPointNumberWidth::Default
                    ))),
                    Range::from_detail(5, 0, 5, 8)
                ),
            ]
        );

        // err: incomplete floating point number since it ends with 'e' (empty exponent)
        assert!(matches!(
            lex_from_str_with_range_strip("123e"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 0,
                        line: 0,
                        column: 0,
                    },
                    end_included: Position {
                        index: 3,
                        line: 0,
                        column: 3,
                    }
                }
            ))
        ));

        // err: multiple '.' (point)
        assert!(matches!(
            lex_from_str_with_range_strip("1.23.456"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));

        // err: multiple 'e' (exponent)
        assert!(matches!(
            lex_from_str_with_range_strip("1e23e456"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));

        // err: invalid char for floating-point decimal number
        // err: multiple 'e' (exponent)
        assert!(matches!(
            lex_from_str_with_range_strip("2.718x"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 5,
                    line: 0,
                    column: 5,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_floating_point_decimal_number_with_suffix() {
        assert_eq!(
            lex_from_str_with_range_strip("1.0f 2.0F 3.0l 4.0L").unwrap(),
            vec![
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "1.0".to_owned(),
                    false,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "2.0".to_owned(),
                    false,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "3.0".to_owned(),
                    false,
                    FloatingPointNumberWidth::LongDouble
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "4.0".to_owned(),
                    false,
                    FloatingPointNumberWidth::LongDouble
                )))
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("1.0dd 2.0DD 3.0df 4.0DF 5.0dl 6.0DL").unwrap(),
            vec![
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "1.0".to_owned(),
                    true,
                    FloatingPointNumberWidth::Default
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "2.0".to_owned(),
                    true,
                    FloatingPointNumberWidth::Default
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "3.0".to_owned(),
                    true,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "4.0".to_owned(),
                    true,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "5.0".to_owned(),
                    true,
                    FloatingPointNumberWidth::LongDouble
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "6.0".to_owned(),
                    true,
                    FloatingPointNumberWidth::LongDouble
                )))
            ]
        );

        // err: invalid suffix for floating-point number
        assert!(matches!(
            lex_from_str_with_range_strip("1.23ll"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_binary_number() {
        assert_eq!(
            lex_from_str_with_range_strip("0b0100").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "0b0100".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        assert_eq!(
            lex_from_str_with_range_strip("0b0110'1001").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "0b01101001".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        // Test location

        assert_eq!(
            lex_from_str("0b0110 0b1000").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "0b0110".to_owned(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(0, 0, 0, 6)
                ),
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "0b1000".to_owned(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(7, 0, 7, 6)
                ),
            ]
        );

        // err: does not support binary floating point
        assert!(matches!(
            lex_from_str_with_range_strip("0b10.10"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));

        // err: invalid digit for binary number
        assert!(matches!(
            lex_from_str_with_range_strip("0b123"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 3,
                    line: 0,
                    column: 3,
                }
            ))
        ));

        // err: invalid char for binary number
        assert!(matches!(
            lex_from_str_with_range_strip("0b1x"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 3,
                    line: 0,
                    column: 3,
                }
            ))
        ));

        // err: empty binary number
        assert!(matches!(
            lex_from_str_with_range_strip("0b"),
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
    }

    #[test]
    fn test_lex_binary_number_with_suffix() {
        assert_eq!(
            lex_from_str_with_range_strip("0b01u 0b10U").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b01".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b10".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                )))
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("0b001l 0b010L 0b011ll 0b100LL 0b101wb 0b110WB").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b001".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b010".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b011".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b100".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b101".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b110".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("0b001ul 0b010UL 0b011ull 0b100ULL 0b101uwb 0b110UWB")
                .unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b001".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b010".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b011".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b100".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b101".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0b110".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        // err: invalid suffix for integer number
        assert!(matches!(
            lex_from_str_with_range_strip("0b01f"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_octal_number() {
        assert_eq!(
            lex_from_str_with_range_strip("0").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "0".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        assert_eq!(
            lex_from_str_with_range_strip("01'100").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "01100".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        assert_eq!(
            lex_from_str_with_range_strip("0775").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "0775".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        // Test location

        assert_eq!(
            lex_from_str("01 0600").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "01".to_owned(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(0, 0, 0, 2)
                ),
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "0600".to_owned(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(3, 0, 3, 4)
                ),
            ]
        );

        // err: does not support octal floating point
        assert!(matches!(
            lex_from_str_with_range_strip("0100.10"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 4,
                    line: 0,
                    column: 4,
                }
            ))
        ));

        // err: invalid digit for octal number
        assert!(matches!(
            lex_from_str_with_range_strip("0778"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 3,
                    line: 0,
                    column: 3,
                }
            ))
        ));

        // err: invalid char for octal number
        assert!(matches!(
            lex_from_str_with_range_strip("077x"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 3,
                    line: 0,
                    column: 3,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_octal_number_with_suffix() {
        assert_eq!(
            lex_from_str_with_range_strip("01u 02U").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "01".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "02".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                )))
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("01l 02L 03ll 04LL 05wb 06WB").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "01".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "02".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "03".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "04".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "05".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "06".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("01ul 02UL 03ull 04ULL 05uwb 06UWB").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "01".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "02".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "03".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "04".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "05".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "06".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        // err: invalid suffix for integer number
        assert!(matches!(
            lex_from_str_with_range_strip("01f"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 2,
                    line: 0,
                    column: 2,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_hexadecimal_number() {
        assert_eq!(
            lex_from_str_with_range_strip("0xabcdef").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "0xabcdef".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        assert_eq!(
            lex_from_str_with_range_strip("0x1234'5678").unwrap(),
            vec![Token::Number(Number::Integer(IntegerNumber::new(
                "0x12345678".to_owned(),
                false,
                IntegerNumberWidth::Default
            )))]
        );

        // Test location

        assert_eq!(
            lex_from_str("0x90ab 0xcd12").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "0x90ab".to_string(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(0, 0, 0, 6)
                ),
                TokenWithRange::new(
                    Token::Number(Number::Integer(IntegerNumber::new(
                        "0xcd12".to_string(),
                        false,
                        IntegerNumberWidth::Default
                    ))),
                    Range::from_detail(7, 0, 7, 6)
                ),
            ]
        );

        // err: invalid char for hex number
        assert!(matches!(
            lex_from_str_with_range_strip("0x1234x"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 6,
                    line: 0,
                    column: 6,
                }
            ))
        ));

        // err: empty hex number
        assert!(matches!(
            lex_from_str_with_range_strip("0x"),
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
    }

    #[test]
    fn test_lex_hexadecimal_number_with_suffix() {
        assert_eq!(
            lex_from_str_with_range_strip("0x1au 0x2bU").unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x1a".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x2b".to_owned(),
                    true,
                    IntegerNumberWidth::Default
                )))
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("0x12abl 0x34cdL 0x56efll 0x78aaLL 0x90bbwb 0xffffWB")
                .unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x12ab".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x34cd".to_owned(),
                    false,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x56ef".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x78aa".to_owned(),
                    false,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x90bb".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0xffff".to_owned(),
                    false,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(
                "0x12abul 0x34cdUL 0x56efull 0x78aaULL 0x90bbuwb 0xffffUWB"
            )
            .unwrap(),
            vec![
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x12ab".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x34cd".to_owned(),
                    true,
                    IntegerNumberWidth::Long
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x56ef".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x78aa".to_owned(),
                    true,
                    IntegerNumberWidth::LongLong
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0x90bb".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
                Token::Number(Number::Integer(IntegerNumber::new(
                    "0xffff".to_owned(),
                    true,
                    IntegerNumberWidth::BitInt
                ))),
            ]
        );
    }

    #[test]
    fn test_lex_hexadecimal_number_floating_point() {
        assert_eq!(
            lex_from_str_with_range_strip("0x1.4p3").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0x1.4p3".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // floating-point number with separator '\''
        // value is std::f32::consts::PI (3.1415927f32)
        assert_eq!(
            lex_from_str_with_range_strip("0x1.921f'b6p1").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0x1.921fb6p1".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // floating-point number with exponent and sign
        // value is std::f64::consts::E (2.718281828459045f64)
        assert_eq!(
            lex_from_str_with_range_strip("0x1.5bf0a8b145769p+1").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0x1.5bf0a8b145769p+1".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // floating-point number with exponent and negative sign
        // value is std::f64::consts::LN_2
        // https://observablehq.com/@jrus/hexfloat
        assert_eq!(
            lex_from_str_with_range_strip("0x1.62e42fefa39efp-1").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0x1.62e42fefa39efp-1".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // automatically complete the hexadecimal floating-point number
        // by insert '0' before the point '.'
        assert_eq!(
            lex_from_str_with_range_strip("0x.1234p5").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0x0.1234p5".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // automatically complete the hexadecimal floating-point number
        // by appending '0' after the point '.'
        assert_eq!(
            lex_from_str_with_range_strip("0x123.p4").unwrap(),
            vec![Token::Number(Number::FloatingPoint(
                FloatingPointNumber::new(
                    "0x123.0p4".to_owned(),
                    false,
                    FloatingPointNumberWidth::Default
                )
            ))]
        );

        // testing token's location

        assert_eq!(
            lex_from_str("0x1.2p3 0xa.bp+12").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                        "0x1.2p3".to_owned(),
                        false,
                        FloatingPointNumberWidth::Default
                    ))),
                    Range::from_detail(0, 0, 0, 7)
                ),
                TokenWithRange::new(
                    Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                        "0xa.bp+12".to_owned(),
                        false,
                        FloatingPointNumberWidth::Default
                    ))),
                    Range::from_detail(8, 0, 8, 9)
                ),
            ]
        );

        // err: missing the exponent
        assert!(matches!(
            lex_from_str_with_range_strip("0x1.23"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 0,
                        line: 0,
                        column: 0,
                    },
                    end_included: Position {
                        index: 5,
                        line: 0,
                        column: 5,
                    }
                }
            ))
        ));

        // err: empty the exponent
        assert!(matches!(
            lex_from_str_with_range_strip("0x1.23p"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 0,
                        line: 0,
                        column: 0,
                    },
                    end_included: Position {
                        index: 6,
                        line: 0,
                        column: 6,
                    }
                }
            ))
        ));

        // err: multiple '.' (point)
        assert!(matches!(
            lex_from_str_with_range_strip("0x1.2.3"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 5,
                    line: 0,
                    column: 5,
                }
            ))
        ));

        // err: multiple 'p' (exponent)
        assert!(matches!(
            lex_from_str_with_range_strip("0x1.2p3p4"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 7,
                    line: 0,
                    column: 7,
                }
            ))
        ));

        // err: invalid exponent (dot '.' after 'p')
        assert!(matches!(
            lex_from_str_with_range_strip("0x1.23p4.5"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 8,
                    line: 0,
                    column: 8,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_hexadecimal_number_floating_point_with_suffix() {
        assert_eq!(
            lex_from_str_with_range_strip("0x1.2p3f 0x2.3p4F 0x3.4p5l 0x4.5p6L").unwrap(),
            vec![
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x1.2p3".to_owned(),
                    false,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x2.3p4".to_owned(),
                    false,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x3.4p5".to_owned(),
                    false,
                    FloatingPointNumberWidth::LongDouble
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x4.5p6".to_owned(),
                    false,
                    FloatingPointNumberWidth::LongDouble
                )))
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(
                "0x1.2p3dd 0x2.3p4DD 0x3.4p5df 0x4.5p6DF 0x5.6p7dl 0x6.7p8DL"
            )
            .unwrap(),
            vec![
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x1.2p3".to_owned(),
                    true,
                    FloatingPointNumberWidth::Default
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x2.3p4".to_owned(),
                    true,
                    FloatingPointNumberWidth::Default
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x3.4p5".to_owned(),
                    true,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x4.5p6".to_owned(),
                    true,
                    FloatingPointNumberWidth::Float
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x5.6p7".to_owned(),
                    true,
                    FloatingPointNumberWidth::LongDouble
                ))),
                Token::Number(Number::FloatingPoint(FloatingPointNumber::new(
                    "0x6.7p8".to_owned(),
                    true,
                    FloatingPointNumberWidth::LongDouble
                )))
            ]
        );

        // err: invalid suffix for floating-point number
        assert!(matches!(
            lex_from_str_with_range_strip("0x1.23ll"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 6,
                    line: 0,
                    column: 6,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_char() {
        assert_eq!(
            lex_from_str_with_range_strip("'a'").unwrap(),
            vec![Token::new_char('a')]
        );

        assert_eq!(
            lex_from_str_with_range_strip("'a' 'z'").unwrap(),
            vec![Token::new_char('a'), Token::new_char('z')]
        );

        // CJK
        assert_eq!(
            lex_from_str_with_range_strip("'文'").unwrap(),
            vec![Token::new_char('文')]
        );

        // emoji
        assert_eq!(
            lex_from_str_with_range_strip("'😊'").unwrap(),
            vec![Token::new_char('😊')]
        );

        // escape char `\\`
        assert_eq!(
            lex_from_str_with_range_strip("'\\\\'").unwrap(),
            vec![Token::new_char('\\')]
        );

        // escape char `\'`
        assert_eq!(
            lex_from_str_with_range_strip("'\\\''").unwrap(),
            vec![Token::new_char('\'')]
        );

        // escape char `"`
        assert_eq!(
            lex_from_str_with_range_strip("'\\\"'").unwrap(),
            vec![Token::new_char('"')]
        );

        // escape char `\t`
        assert_eq!(
            lex_from_str_with_range_strip("'\\t'").unwrap(),
            vec![Token::new_char('\t')]
        );

        // escape char `\r`
        assert_eq!(
            lex_from_str_with_range_strip("'\\r'").unwrap(),
            vec![Token::new_char('\r')]
        );

        // escape char `\n`
        assert_eq!(
            lex_from_str_with_range_strip("'\\n'").unwrap(),
            vec![Token::new_char('\n')]
        );

        // escape char `\?`
        assert_eq!(
            lex_from_str_with_range_strip("'\\?'").unwrap(),
            vec![Token::new_char('?')]
        );

        // escape char `\0`
        assert_eq!(
            lex_from_str_with_range_strip("'\\0'").unwrap(),
            vec![Token::new_char('\0')]
        );

        // escape char, octal
        assert_eq!(
            lex_from_str_with_range_strip("'\\70'").unwrap(),
            vec![Token::new_char('8')]
        );

        // escape char, octal
        assert_eq!(
            lex_from_str_with_range_strip("'\\101'").unwrap(),
            vec![Token::new_char('A')]
        );

        // escape char, hex
        assert_eq!(
            lex_from_str_with_range_strip("'\\x38'").unwrap(),
            vec![Token::new_char('8')]
        );

        // escape char, hex
        assert_eq!(
            lex_from_str_with_range_strip("'\\x41'").unwrap(),
            vec![Token::new_char('A')]
        );

        // escape char, unicode
        assert_eq!(
            lex_from_str_with_range_strip("'\\u002d'").unwrap(),
            vec![Token::new_char('-')]
        );

        // escape char, unicode
        assert_eq!(
            lex_from_str_with_range_strip("'\\U00006587'").unwrap(),
            vec![Token::new_char('文')]
        );

        // continue lines
        assert_eq!(
            lex_from_str_with_range_strip("'\\\\\nx41'").unwrap(),
            vec![Token::new_char('A')]
        );

        // Test location

        assert_eq!(
            lex_from_str("'a' '文'").unwrap(),
            vec![
                TokenWithRange::new(Token::new_char('a'), Range::from_detail(0, 0, 0, 3)),
                TokenWithRange::new(Token::new_char('文'), Range::from_detail(4, 0, 4, 3))
            ]
        );

        assert_eq!(
            lex_from_str("'\\t'").unwrap(),
            vec![TokenWithRange::new(
                Token::new_char('\t'),
                Range::from_detail(0, 0, 0, 4)
            )]
        );

        assert_eq!(
            lex_from_str("'\\u6587'").unwrap(),
            vec![TokenWithRange::new(
                Token::new_char('文'),
                Range::from_detail(0, 0, 0, 8)
            )]
        );

        // err: empty char
        assert!(matches!(
            lex_from_str_with_range_strip("''"),
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
            lex_from_str_with_range_strip("'"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete char, missing the closing single quote, encounter EOF
        assert!(matches!(
            lex_from_str_with_range_strip("'a"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: invalid char, expect the right quote, encounter another char
        assert!(matches!(
            lex_from_str_with_range_strip("'ab"),
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
            lex_from_str_with_range_strip("'ab'"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 2,
                    line: 0,
                    column: 2,
                }
            ))
        ));

        // err: incorrect hex escape `'\x3'`
        assert!(matches!(
            lex_from_str_with_range_strip(r#"'\x3'"#),
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
            lex_from_str_with_range_strip("'\\u'"),
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
            lex_from_str_with_range_strip("'\\u123"),
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
            lex_from_str_with_range_strip("'\\U1000111'"),
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
            lex_from_str_with_range_strip("'\\U00123456'"),
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
            lex_from_str_with_range_strip("'\\u12mn''"),
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
    fn test_lex_char_with_encoding() {
        assert_eq!(
            lex_from_str_with_range_strip("'a' L'b' u'c' U'文' u8'😊'",).unwrap(),
            vec![
                Token::Char('a', CharEncoding::Default),
                Token::Char('b', CharEncoding::Wide),
                Token::Char('c', CharEncoding::UTF16),
                Token::Char('文', CharEncoding::UTF32),
                Token::Char('😊', CharEncoding::UTF8),
            ]
        );
    }

    #[test]
    fn test_lex_string() {
        assert_eq!(
            lex_from_str_with_range_strip(r#""abc""#).unwrap(),
            vec![Token::new_string("abc")]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#""abc" "xyz""#).unwrap(),
            vec![Token::new_string("abc"), Token::new_string("xyz")]
        );

        assert_eq!(
            lex_from_str_with_range_strip("\"abc\"\n\n\"xyz\"").unwrap(),
            vec![Token::new_string("abc"), Token::new_string("xyz"),]
        );

        // unicode
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                "abc文字😊"
                "#
            )
            .unwrap(),
            vec![Token::new_string("abc文字😊")]
        );

        // empty string
        assert_eq!(
            lex_from_str_with_range_strip("\"\"").unwrap(),
            vec![Token::new_string("")]
        );

        // escape chars
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                "\\\'\"\t\r\n\?\0\x2d\u6587"
                "#
            )
            .unwrap(),
            vec![Token::new_string("\\\'\"\t\r\n?\0-文")]
        );

        // continue lines
        assert_eq!(
            lex_from_str_with_range_strip("\"abc\\\\\n0xyz\"").unwrap(),
            vec![Token::new_string("abc\0xyz")]
        );

        // Test location

        // "abc" 'n' "文字😊"
        // 012345678901 2 34    // index

        assert_eq!(
            lex_from_str(r#""abc" 'n' "文字😊""#).unwrap(),
            vec![
                TokenWithRange::new(Token::new_string("abc"), Range::from_detail(0, 0, 0, 5)),
                TokenWithRange::new(Token::new_char('n'), Range::from_detail(6, 0, 6, 3)),
                TokenWithRange::new(
                    Token::new_string("文字😊"),
                    Range::from_detail(10, 0, 10, 5)
                ),
            ]
        );

        // err: incomplete string, missing the content, encounter EOF
        assert!(matches!(
            lex_from_str_with_range_strip("\""),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete string, missing the closing double quote, encounter EOF
        assert!(matches!(
            lex_from_str_with_range_strip("\"abc"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete string, missing the closing double quote, ends with '\n' and encounter EOF
        assert!(matches!(
            lex_from_str_with_range_strip("\"abc\n"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete string, missing the closing double quote, ends with whitespaces and encounter EOF
        assert!(matches!(
            lex_from_str_with_range_strip("\"abc\n   "),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incorrect hex escape `'\x3'`
        assert!(matches!(
            lex_from_str_with_range_strip(r#""abc\x3xyz""#),
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
        //
        // "abc\\u"
        // 01234 56     // index
        assert!(matches!(
            lex_from_str_with_range_strip(r#""abc\u""#),
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
            lex_from_str_with_range_strip(r#""abc\u123""#),
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
        //
        // "abc\\U1000111xyz"
        // 01234 567890234567       // index
        assert!(matches!(
            lex_from_str_with_range_strip(r#""abc\U1000111xyz""#),
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
        //
        // "abc\\U00123456xyz"
        // 01234 5678901234567      // index
        assert!(matches!(
            lex_from_str_with_range_strip(r#""abc\U00123456xyz""#),
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
            lex_from_str_with_range_strip(r#""abc\u12mnxyz""#),
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
            lex_from_str_with_range_strip(r#""abc\u1234"#),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));
    }

    #[test]
    fn test_lex_string_with_encoding() {
        assert_eq!(
            lex_from_str_with_range_strip(r#""abc" L"def" u"xyz" U"文字" u8"😊""#,).unwrap(),
            vec![
                Token::String("abc".to_owned(), StringEncoding::Default),
                Token::String("def".to_owned(), StringEncoding::Wide),
                Token::String("xyz".to_owned(), StringEncoding::UTF16),
                Token::String("文字".to_owned(), StringEncoding::UTF32),
                Token::String("😊".to_owned(), StringEncoding::UTF8),
            ]
        );
    }

    #[test]
    fn test_lex_identifier() {
        assert_eq!(
            lex_from_str_with_range_strip("name").unwrap(),
            vec![Token::new_identifier("name")]
        );

        assert_eq!(
            lex_from_str_with_range_strip("_abc").unwrap(),
            vec![Token::new_identifier("_abc")]
        );

        assert_eq!(
            lex_from_str_with_range_strip("__abc").unwrap(),
            vec![Token::new_identifier("__abc")]
        );

        // contains double colons "::"
        assert_eq!(
            lex_from_str_with_range_strip("foo::bar").unwrap(),
            vec![Token::new_identifier("foo::bar")]
        );

        // contains unicode
        assert_eq!(
            lex_from_str_with_range_strip("αβγ 文字 🍞🥛").unwrap(),
            vec![
                Token::new_identifier("αβγ"),
                Token::new_identifier("文字"),
                Token::new_identifier("🍞🥛"),
            ]
        );

        // Test location

        assert_eq!(
            lex_from_str("hello ANCC").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::new_identifier("hello"),
                    Range::from_detail(0, 0, 0, 5)
                ),
                TokenWithRange::new(
                    Token::new_identifier("ANCC"),
                    Range::from_detail(6, 0, 6, 4)
                )
            ]
        );

        // err: invalid identifier
        assert!(matches!(
            lex_from_str_with_range_strip("1abc"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 1,
                    line: 0,
                    column: 1,
                }
            ))
        ));
    }

    #[test]
    fn test_lex_directive_start_and_end() {
        assert_eq!(
            lex_from_str_with_range_strip(
                "\
abc
#define FOO
xyz
#define BAR"
            )
            .unwrap(),
            vec![
                Token::new_identifier("abc"),
                Token::DirectiveStart,
                Token::new_identifier("define"),
                Token::new_identifier("FOO"),
                Token::DirectiveEnd,
                Token::new_identifier("xyz"),
                Token::DirectiveStart,
                Token::new_identifier("define"),
                Token::new_identifier("BAR"),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(
                "\
#define FOO(x) #x
abc
#define BAR(y) y##2
xyz
"
            )
            .unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("define"),
                Token::FunctionLikeMacroIdentifier("FOO".to_owned()),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::new_identifier("x"),
                Token::Punctuator(Punctuator::ParenthesisClose),
                Token::Pound,
                Token::new_identifier("x"),
                Token::DirectiveEnd,
                //
                Token::new_identifier("abc"),
                //
                Token::DirectiveStart,
                Token::new_identifier("define"),
                Token::FunctionLikeMacroIdentifier("BAR".to_owned()),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::new_identifier("y"),
                Token::Punctuator(Punctuator::ParenthesisClose),
                Token::new_identifier("y"),
                Token::PoundPound,
                Token::new_integer_number(2),
                Token::DirectiveEnd,
                //
                Token::new_identifier("xyz"),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("#foo # bar").unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("foo"),
                Token::Pound,
                Token::new_identifier("bar")
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("#foo ###").unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("foo"),
                Token::PoundPound,
                Token::Pound
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip("#\n").unwrap(),
            vec![Token::DirectiveStart, Token::DirectiveEnd,]
        );
    }

    #[test]
    fn test_lex_file_path() {
        assert_eq!(
            lex_from_str_with_range_strip(r#"<foo.h>"#).unwrap(),
            vec![
                Token::Punctuator(Punctuator::LessThan),
                Token::new_identifier("foo"),
                Token::Punctuator(Punctuator::Dot),
                Token::new_identifier("h"),
                Token::Punctuator(Punctuator::GreaterThan),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"include <foo.h>"#).unwrap(),
            vec![
                Token::new_identifier("include"),
                Token::Punctuator(Punctuator::LessThan),
                Token::new_identifier("foo"),
                Token::Punctuator(Punctuator::Dot),
                Token::new_identifier("h"),
                Token::Punctuator(Punctuator::GreaterThan),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#include <foo.h>"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("include"),
                Token::FilePath("foo.h".to_owned(), true),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#embed <hippo.png>"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("embed"),
                Token::FilePath("hippo.png".to_owned(), true),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"__has_include(<foo.h>)"#).unwrap(),
            vec![
                Token::new_identifier("__has_include"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::Punctuator(Punctuator::LessThan),
                Token::new_identifier("foo"),
                Token::Punctuator(Punctuator::Dot),
                Token::new_identifier("h"),
                Token::Punctuator(Punctuator::GreaterThan),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#if __has_include(<foo.h>)"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("if"),
                Token::new_identifier("__has_include"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::FilePath("foo.h".to_owned(), true),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"__has_embed(<hippo.png>)"#).unwrap(),
            vec![
                Token::new_identifier("__has_embed"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::Punctuator(Punctuator::LessThan),
                Token::new_identifier("hippo"),
                Token::Punctuator(Punctuator::Dot),
                Token::new_identifier("png"),
                Token::Punctuator(Punctuator::GreaterThan),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#if __has_embed(<hippo.png>)"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("if"),
                Token::new_identifier("__has_embed"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::FilePath("hippo.png".to_owned(), true),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#""bar.h""#).unwrap(),
            vec![Token::new_string("bar.h"),]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"include "bar.h""#).unwrap(),
            vec![Token::new_identifier("include"), Token::new_string("bar.h"),]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#include "bar.h""#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("include"),
                Token::FilePath("bar.h".to_owned(), false),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#embed "spark.png""#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("embed"),
                Token::FilePath("spark.png".to_owned(), false),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"__has_include("spark.png")"#).unwrap(),
            vec![
                Token::new_identifier("__has_include"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::new_string("spark.png"),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#if __has_include("spark.png")"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("if"),
                Token::new_identifier("__has_include"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::FilePath("spark.png".to_owned(), false),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"__has_embed("spark.png")"#).unwrap(),
            vec![
                Token::new_identifier("__has_embed"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::new_string("spark.png"),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#if __has_embed("spark.png")"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("if"),
                Token::new_identifier("__has_embed"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::FilePath("spark.png".to_owned(), false),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        // unescaped path
        assert_eq!(
            lex_from_str_with_range_strip(r#"#include <path\to\header.h>"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("include"),
                Token::FilePath("path\\to\\header.h".to_owned(), true),
            ]
        );

        // Test location
        assert_eq!(
            lex_from_str("#include <path/to/header.h>").unwrap(),
            vec![
                TokenWithRange::new(Token::DirectiveStart, Range::from_detail(0, 0, 0, 1)),
                TokenWithRange::new(
                    Token::new_identifier("include"),
                    Range::from_detail(1, 0, 1, 7)
                ),
                TokenWithRange::new(
                    Token::FilePath("path/to/header.h".to_owned(), true),
                    Range::from_detail(9, 0, 9, 18)
                )
            ]
        );

        // err: invalid character `\n` in filepath
        assert!(matches!(
            lex_from_str_with_range_strip("#include <foo\nbar>"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 13,
                    line: 0,
                    column: 13,
                }
            ))
        ));

        // err: invalid character `\n` in filepath
        assert!(matches!(
            lex_from_str_with_range_strip("#include \"foo\nbar\""),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 13,
                    line: 0,
                    column: 13,
                }
            ))
        ));

        // err: incomplete filepath, missing the closing '>'
        assert!(matches!(
            lex_from_str_with_range_strip("#include <foo.h"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete filepath, missing the closing '"'
        assert!(matches!(
            lex_from_str_with_range_strip("#include \"foo.h"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));
    }

    #[test]
    fn test_lex_function_like_macro_identifier() {
        assert_eq!(
            lex_from_str_with_range_strip(r#"#define foo (a)"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("define"),
                Token::new_identifier("foo"),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::new_identifier("a"),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );

        assert_eq!(
            lex_from_str_with_range_strip(r#"#define foo(a)"#).unwrap(),
            vec![
                Token::DirectiveStart,
                Token::new_identifier("define"),
                Token::FunctionLikeMacroIdentifier("foo".to_owned()),
                Token::Punctuator(Punctuator::ParenthesisOpen),
                Token::new_identifier("a"),
                Token::Punctuator(Punctuator::ParenthesisClose),
            ]
        );
    }

    #[test]
    fn test_lex_newlines() {
        // "(\t\r\n\n\n)"
        //  0  2   4 5 6    // index
        //  0  0   1 2 3    // line
        //  0  2   0 0 0    // column
        //  1  2   1 1 1    // length

        assert_eq!(
            lex_from_str("(\t\r\n\n\n)").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ParenthesisOpen),
                    Range::from_detail(0, 0, 0, 1)
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::ParenthesisClose),
                    Range::from_detail(6, 3, 0, 1)
                ),
            ]
        );

        // "[\n  pub\n    data\n]"
        //  01 234567 890123456 7   // index
        //  00 111111 222222222 3   // line
        //  01 012345 012345678 0   // column
        //  11   3  1     4   1 1   // length

        assert_eq!(
            lex_from_str("[\n  pub\n    data\n]").unwrap(),
            vec![
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::BracketOpen),
                    Range::from_detail(0, 0, 0, 1)
                ),
                TokenWithRange::new(Token::new_identifier("pub"), Range::from_detail(4, 1, 2, 3)),
                TokenWithRange::new(
                    Token::new_identifier("data"),
                    Range::from_detail(12, 2, 4, 4)
                ),
                TokenWithRange::new(
                    Token::Punctuator(Punctuator::BracketClose),
                    Range::from_detail(17, 3, 0, 1)
                ),
            ]
        )
    }

    #[test]
    fn test_lex_line_comment() {
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 //11
                13 17// 19 23
                //  29
                31//    37
                "#
            )
            .unwrap(),
            vec![
                Token::new_integer_number(7),
                Token::new_integer_number(13),
                Token::new_integer_number(17),
                Token::new_integer_number(31),
            ]
        );

        // continue lines within line comments
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 // 11 \
                13 17 \
                19
                23
                "#
            )
            .unwrap(),
            vec![Token::new_integer_number(7), Token::new_integer_number(23),]
        );

        // Test location

        assert_eq!(
            lex_from_str("abc // def\n// uvw\nxyz").unwrap(),
            vec![
                TokenWithRange::new(Token::new_identifier("abc"), Range::from_detail(0, 0, 0, 3)),
                TokenWithRange::new(
                    Token::new_identifier("xyz"),
                    Range::from_detail(18, 2, 0, 3)
                ),
            ]
        );
    }

    #[test]
    fn test_lex_block_comment() {
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 /* 11 13 */ 17
                "#
            )
            .unwrap(),
            vec![Token::new_integer_number(7), Token::new_integer_number(17),]
        );

        // line comment chars "//" within the block comment
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 /* 11 // 13 17 */ 19
                "#
            )
            .unwrap(),
            vec![Token::new_integer_number(7), Token::new_integer_number(19),]
        );

        // block comment within the line comment
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 // foo /*
                11
                13 // bar */
                "#
            )
            .unwrap(),
            vec![
                Token::new_integer_number(7),
                Token::new_integer_number(11),
                Token::new_integer_number(13),
            ]
        );

        // continue lines within block comments
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 /* 11 \
                13 */ 19
                "#
            )
            .unwrap(),
            vec![Token::new_integer_number(7), Token::new_integer_number(19),]
        );

        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                7 /\
* 11
*\
/ 19"#
            )
            .unwrap(),
            vec![Token::new_integer_number(7), Token::new_integer_number(19),]
        );

        // Test location

        assert_eq!(
            lex_from_str("foo /* hello */ bar").unwrap(),
            vec![
                TokenWithRange::new(Token::new_identifier("foo"), Range::from_detail(0, 0, 0, 3)),
                TokenWithRange::new(
                    Token::new_identifier("bar"),
                    Range::from_detail(16, 0, 16, 3)
                ),
            ]
        );

        // err: incomplete, missing `*/`
        assert!(matches!(
            lex_from_str_with_range_strip("7 /* 11"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete, missing `*/`, ends with newline
        assert!(matches!(
            lex_from_str_with_range_strip("7 /* 11\n"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete, missing `*/`
        assert!(matches!(
            lex_from_str_with_range_strip("a /* b */ c /* d"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // err: incomplete, missing `*/`, ends with newline
        assert!(matches!(
            lex_from_str_with_range_strip("a /* b */ c /* d\n"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));
    }

    #[test]
    fn test_lex_shebang() {
        assert_eq!(
            lex_from_str_with_range_strip(
                r#"#!/usr/bin/env ancc
                11
                "#
            )
            .unwrap(),
            vec![Token::new_integer_number(11)]
        );

        assert_eq!(
            lex_from_str_with_range_strip(
                r#"
                // this is an ANCC script file
                /* set the executable bit for this file with `chmod` command:
                 * `chmod +x sample.c`
                 */
                #!/usr/bin/env ancc
                11
                "#
            )
            .unwrap(),
            vec![Token::new_integer_number(11)]
        );
    }
}
