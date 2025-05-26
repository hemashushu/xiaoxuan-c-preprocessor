// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::range::Range;

/// Represents a token type for the C preprocessor.
/// See: https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html
#[derive(Debug, PartialEq)]
pub enum Token {
    // An identifier, as in C: any sequence of letters (including non-Latin letters such as Chinese ideograms),
    // digits, or underscores, starting with a letter or underscore.
    // C keywords are not treated specially by the preprocessor.
    Identifier(String),

    // A preprocessing number, which includes all standard integer and floating-point constants in C.
    // Such numbers may start with an optional period, must have at least one decimal digit,
    // and can contain letters, digits, underscores, periods, and exponent markers.
    // Exponents are two-character sequences like 'e+', 'e-', 'E+', 'E-', 'p+', 'p-', 'P+', or 'P-'.
    // Exponents beginning with 'p' or 'P' are used for hexadecimal floating-point constants.
    // See: https://en.cppreference.com/w/c/language/floating_constant
    Number(String),

    // A string literal, character constant, or header file name (as used in #include).
    // String and character constants are written as "..." or '...'.
    // Embedded quotes must be escaped with a backslash, e.g., '\'' represents the character constant for a single quote.
    String(String),

    // A character constant, e.g., 'a', '\t', '文', '\u6587', and '\U2005E'.
    // See: https://en.wikipedia.org/wiki/Escape_sequences_in_C
    Char(char),

    Operator(Operator),

    // A punctuator: any punctuation character meaningful in C or C++,
    // except for '@', '$', and '`', which are not considered C punctuators.
    Punctuator(Punctuator),

    // A newline token, representing either '\n' or '\r\n'.
    // Newlines are preserved in the token stream for macro expansion,
    // but are removed before AST parsing.
    Newline,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    // Arithmetic Operators
    Addition,       // '+'
    Subtraction,    // '-'
    Multiplication, // '*'
    Division,       // '/'
    Modulus,        // '%'
    Increment,      // '++'
    Decrement,      // '--'

    // Relational Operators
    Equal,              // '=='
    NotEqual,           // '!='
    GreaterThan,        // '>'
    LessThan,           // '<'
    GreaterThanOrEqual, // '>='
    LessThanOrEqual,    // '<='

    // Logical Operators
    And, // '&&'
    Or,  // '||'
    Not, // '!'

    // Bitwise Operators
    BitwiseAnd, // '&'
    BitwiseOr,  // '|'
    BitwiseXor, // '^'
    BitwiseNot, // '~'
    ShiftLeft,  // '<<'
    ShiftRight, // '>>'

    // Assignment Operators
    Assignment,           // '='
    AddAssignment,        // '+='
    SubtractAssignment,   // '-='
    MultiplyAssignment,   // '*='
    DivideAssignment,     // '/='
    ModulusAssignment,    // '%='
    BitwiseAndAssignment, // '&='
    BitwiseOrAssignment,  // '|='
    BitwiseXorAssignment, // '^='
    ShiftLeftAssignment,  // '<<='
    ShiftRightAssignment, // '>>='

    // Conditional Operator
    Conditional, // '?'

    // Miscellaneous Operators
    AddressOf,     // '&'
    Dereference,   // '*'
    Comma,         // ','
    MemberAccess,  // '.'
    PointerAccess, // '->'
}

#[derive(Debug, PartialEq)]
pub enum Punctuator {
    BraceOpen,        // '{'
    BraceClose,       // '}'
    BracketOpen,      // '['
    BracketClose,     // ']'
    ParenthesisOpen,  // '('
    ParenthesisClose, // ')'
    Semicolon,        // ';'
    Colon,            // ':'
    Variadic,         // '...'

    // used in C preprocessor directives
    Pound,      // '#'
    PoundPound, // '##'
}

#[derive(Debug, PartialEq)]
pub struct TokenWithRange {
    pub token: Token,
    pub range: Range,
}

impl TokenWithRange {
    pub fn new(token: Token, range: Range) -> Self {
        Self { token, range }
    }
}
