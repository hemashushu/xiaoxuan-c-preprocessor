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
    //
    // see: https://en.cppreference.com/w/c/language/string_literal.html
    String(String, StringType),

    // A (single) character constant, e.g., 'a', '\t', '文', '\u6587', and '\U2005E'.
    // ANCPP does not support multicharacter constants like 'abc', L'abc', '\1\2\3\4'.
    //
    // See:
    // - https://en.wikipedia.org/wiki/Escape_sequences_in_C
    // - https://en.cppreference.com/w/c/language/character_constant.html
    Char(char, CharType),

    // A punctuator: any punctuation character meaningful in C or C++,
    // except for '@', '$', and '`', which are not considered C punctuators.
    Punctuator(Punctuator),

    // A newline token, representing either '\n' or '\r\n'.
    // Newlines are preserved in the token stream for macro expansion,
    // but are removed before AST parsing.
    Newline,
}

#[derive(Debug, PartialEq)]
pub enum Punctuator {
    // Arithmetic Operators
    Addition,       // '+'
    Subtraction,    // '-'
    Multiplication, // '*', also is operator `Dereference`
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
    BitwiseAnd, // '&', also is operator `AddressOf`
    BitwiseOr,  // '|'
    BitwiseXor, // '^'
    BitwiseNot, // '~'
    ShiftLeft,  // '<<'
    ShiftRight, // '>>', note that there is no `>>>` (logical right shift) operator in C

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
    QuestionMark, // '?'

    // Miscellaneous Operators
    Comma, // ','
    Dot,   // '.', member access operator
    Arrow, // '->', pointer member access operator

    // Brackets and Delimiters
    BraceOpen,        // '{'
    BraceClose,       // '}'
    BracketOpen,      // '['
    BracketClose,     // ']'
    ParenthesisOpen,  // '('
    ParenthesisClose, // ')'
    Semicolon,        // ';'
    Colon,            // ':'
    Ellipsis,         // '...', Variadic arguments in function definitions

    // Punctuators used in C preprocessor directives
    Pound,      // '#'
    PoundPound, // '##'
}

#[derive(Debug, PartialEq)]
pub enum CharType {
    Int, // Normal character, e.g., 'a', '1', '文', note that the data type is `int` instead of `char`.
    Wide, // Wide character, e.g., L'a', L'文', data type is `wchar_t`
    UTF16, // UTF-16 character, e.g., u'a', u'文', data type is `char16_t`
    UTF32, // UTF-32 character, e.g., U'a', U'文', data type is `char32_t`
    UTF8, // UTF-8 character, e.g., u8'a', u8'文', data type is `char8_t`
}

#[derive(Debug, PartialEq)]
pub enum StringType {
    Char,  // Normal string, e.g., "hello", "文", data type is `char[]`
    Wide,  // Wide string, e.g., L"hello", L"文", data type is `wchar_t[]`
    UTF16, // UTF-16 string, e.g., u"hello", u"文", data type is `char16_t[]`
    UTF32, // UTF-32 string, e.g., U"hello", U"文", data type is `char32_t[]`
    UTF8,  // UTF-8 string, e.g., u8"hello", u8"文", data type is `char8_t[]`
}

#[derive(Debug, PartialEq)]
pub enum IntegerNumberType{
    Unsigned,
    UnsignedLong,
    UnsignedLongLong,
    Long,
    LongLong
}

#[derive(Debug, PartialEq)]
pub enum FloatingPointNumberType{
    Float,
    Double,
    LongDouble,
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
