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
    Number(Number),

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
    // Newlines are preserved in the token stream for macro expansion.
    //
    // Each directive occupies one line and has the following format:
    //
    // - '#' character.
    // - preprocessing instruction (e.g. `define`, `include`).
    // - arguments (depends on the instruction).
    // - line break (newline).
    //
    // The null directive (# followed by a line break) is allowed and has no effect.
    // see: https://en.cppreference.com/w/c/preprocessor.html
    Newline,

    // The file path of the directive `#include` or `#embed`.
    //
    // Although the file path is a string either, but it is not a string literal
    // because it does not support escape sequences like `\x40`, `\"`, etc.
    IncludingFilePath(String),

    // The identifier of a function-like macro. e.g.
    //
    // ```c
    // #define foo(x, y) ((x) + (y))
    // ```
    //
    // An identifier in the `#define` directive
    // is followed by a parenthesis `(` immediately represents a function-like macro identifier,
    // while an identifier in the `#include` directive is followed by a space representing
    // an object-like macro identifier.
    // This is an exception because the whitespaces are ignored in the C language, thus
    // a special type of token is needed to distinguish.
    FunctionLikeMacroIdentifier(String),
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
    AttributeOpen,    // '[[' for attributes in C23, e.g., [[nodiscard]]
    AttributeClose,   // ']]' for attributes in C23, e.g., [[nodiscard]]
    Namespace,        // '::', used in attribute with a namespace, such as [[foo::bar]]

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
pub enum IntegerNumberType {
    Default, // no suffix, e.g., 123, the type of the integer constant is the first type in which the value can fit.
    Long,    // suffix "l", "L", e.g., 123l, 123L
    LongLong, // suffix "ll", "LL", e.g., 123ll, 123LL
    BitInt,  // suffix "wb", "WB", e.g., 123wb, 123WB
}

#[derive(Debug, PartialEq)]
pub struct IntegerNumber {
    pub value: String,
    pub unsigned: bool, // true for unsigned integers, false for signed integers
    pub number_type: IntegerNumberType,
}

impl IntegerNumber {
    pub fn new(value: String, unsigned: bool, number_type: IntegerNumberType) -> Self {
        Self {
            value,
            unsigned,
            number_type,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum FloatingPointNumberType {
    Double,     // no suffix, e.g., 1.23, an unsuffixed floating constant has type `double`.
    Float,      // suffix "f", "F", e.g., 1.23f, 1.23F
    LongDouble, // suffix "l", "L", e.g., 1.23l, 1.23L
}

#[derive(Debug, PartialEq)]
pub struct FloatingPointNumber {
    pub value: String,

    /// true for `_Decimal{32|64|128}`.
    /// possible suffix are: `dd`, `DD`, `df`, `DF`, `dl`, `DL`
    pub decimal: bool,
    pub number_type: FloatingPointNumberType,
}

impl FloatingPointNumber {
    pub fn new(value: String, decimal: bool, number_type: FloatingPointNumberType) -> Self {
        Self {
            value,
            decimal,
            number_type,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Number {
    Integer(IntegerNumber),
    FloatingPoint(FloatingPointNumber),
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
