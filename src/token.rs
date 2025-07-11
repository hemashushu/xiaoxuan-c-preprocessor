// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

/// Represents a token type for the C preprocessor.
///
/// After preprocessing, the tokens `Newline`, `FilePath`, `FunctionLikeMacroIdentifier`
/// `Pound` and `PoundPound` will not be present in the final token stream.
///
/// See: https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // An identifier, as in C: any sequence of letters (including non-Latin letters such as Chinese ideograms),
    // digits, or underscores, starting with a letter or underscore.
    // C keywords are not treated specially by the preprocessor.
    //
    // Identifiers may contain namespace separators (`::`) in attribute names,
    // e.g., `[[foo::bar]]`, `[[some::attribute(123)]]`.
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
    // See: https://en.cppreference.com/w/c/language/string_literal.html
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

    /*
     * The following tokens are used in the C preprocessor.
     * They are not present in the final token stream after preprocessing.
     */
    // Pound sign (`#`) at the beginning of a line indicates a preprocessing directive.
    DirectiveStart,

    // A newline token, representing either '\n' or '\r\n'.
    // Newlines are special tokens in the C preprocessor,
    // and they are used to separate preprocessing directives and other tokens
    // in the token stream.
    //
    // Each directive occupies one line and has the following format:
    //
    // - '#' character.
    // - preprocessing instruction (e.g. `define`, `include`).
    // - arguments (depends on the instruction).
    // - line break (newline).
    //
    // The null directive (# followed by a line break) is allowed and has no effect.
    // See: https://en.cppreference.com/w/c/preprocessor.html
    DirectiveEnd,

    // Pound (`#`)
    Pound,

    // PoundPound (`##`)
    PoundPound,

    // The file path of the directive `#include` or `#embed`.
    //
    // Although the file path is also a string, it is not a string literal
    // because it does not support escape sequences like `\x40`, `\"`, etc.
    FilePath(String, /* stick_to_system */ bool),

    // The identifier of a function-like macro. e.g.
    //
    // ```c
    // #define foo(x, y) ((x) + (y))
    // ```
    //
    // An identifier in the `#define` directive
    // that is immediately followed by a parenthesis `(` represents a function-like macro identifier,
    // while an identifier in the `#include` directive followed by a space represents
    // an object-like macro identifier.
    // This is an exception because whitespaces are typically ignored in the C language, thus
    // a special type of token is needed to make this distinction.
    FunctionLikeMacroIdentifier(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Punctuator {
    // Arithmetic Operators
    Add,      // '+'
    Subtract, // '-'
    Multiply, // '*', also is operator `Dereference`
    Divide,   // '/'
    Modulo,   // '%'
    Increase, // '++'
    Decrease, // '--'

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
    Assign,           // '='
    AddAssign,        // '+='
    SubtractAssign,   // '-='
    MultiplyAssign,   // '*='
    DivideAssign,     // '/='
    ModulusAssign,    // '%='
    BitwiseAndAssign, // '&='
    BitwiseOrAssign,  // '|='
    BitwiseXorAssign, // '^='
    ShiftLeftAssign,  // '<<='
    ShiftRightAssign, // '>>='

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
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CharType {
    Default, // Default character, e.g., 'a', '1', '文', note that the data type is `int` instead of `char`.
    Wide,    // Wide character, e.g., L'a', L'文', data type is `wchar_t`
    UTF16,   // UTF-16 character, e.g., u'a', u'文', data type is `char16_t`
    UTF32,   // UTF-32 character, e.g., U'a', U'文', data type is `char32_t`
    UTF8,    // UTF-8 character, e.g., u8'a', u8'文', data type is `char8_t`
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StringType {
    Default, // Default string, e.g., "hello", "文", data type is `char[]`
    Wide,    // Wide string, e.g., L"hello", L"文", data type is `wchar_t[]`
    UTF16,   // UTF-16 string, e.g., u"hello", u"文", data type is `char16_t[]`
    UTF32,   // UTF-32 string, e.g., U"hello", U"文", data type is `char32_t[]`
    UTF8,    // UTF-8 string, e.g., u8"hello", u8"文", data type is `char8_t[]`
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Integer(IntegerNumber),
    FloatingPoint(FloatingPointNumber),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerNumber {
    pub value: String,
    pub unsigned: bool, // true for unsigned integers, false for signed integers
    pub number_type: IntegerNumberType,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntegerNumberType {
    Default, // no suffix, e.g., 123, the type of the integer constant is the first type in which the value can fit.
    Long,    // suffix "l", "L", e.g., 123l, 123L
    LongLong, // suffix "ll", "LL", e.g., 123ll, 123LL
    BitInt,  // suffix "wb", "WB", e.g., 123wb, 123WB
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatingPointNumber {
    pub value: String,

    /// true for `_Decimal{32|64|128}`.
    /// possible suffix are: `dd`, `DD`, `df`, `DF`, `dl`, `DL`
    pub decimal: bool,
    pub number_type: FloatingPointNumberType,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FloatingPointNumberType {
    Default,    // no suffix, e.g., 1.23, an unsuffixed floating constant has type `double`.
    Float,      // suffix "f", "F", e.g., 1.23f, 1.23F
    LongDouble, // suffix "l", "L", e.g., 1.23l, 1.23L
}

impl IntegerNumber {
    pub fn new(value: String, unsigned: bool, number_type: IntegerNumberType) -> Self {
        Self {
            value,
            unsigned,
            number_type,
        }
    }

    pub fn as_usize(&self) -> Result<usize, std::num::ParseIntError> {
        let (src, radix) = if self.value.starts_with("0x") {
            // Hexadecimal number
            (&self.value[2..], 16)
        } else if self.value.starts_with("0b") {
            // Binary number
            (&self.value[2..], 2)
        } else if self.value.starts_with("0") && self.value.len() > 1 {
            // Octal number
            (&self.value[1..], 8)
        } else {
            // Decimal number
            (&self.value[..], 10)
        };

        usize::from_str_radix(src, radix)
    }
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::String(s, t) => {
                let quote = match t {
                    StringType::Default => "\"",
                    StringType::Wide => "L\"",
                    StringType::UTF16 => "u\"",
                    StringType::UTF32 => "U\"",
                    StringType::UTF8 => "u8\"",
                };

                // Escape special characters in the string
                let escaped = escape_string(s);
                write!(f, "{}{}\"", quote, escaped)
            }
            Token::Char(c, t) => {
                let prefix = match t {
                    CharType::Default => "'",
                    CharType::Wide => "L'",
                    CharType::UTF16 => "u'",
                    CharType::UTF32 => "U'",
                    CharType::UTF8 => "u8'",
                };

                // Escape special character
                let escaped = escape_char(*c);
                write!(f, "{}{}'", prefix, escaped)
            }
            Token::Punctuator(p) => write!(f, "{}", p),
            _ => unreachable!("Preprocessor tokens should not be displayed: {}", self),
        }
    }
}

pub fn escape_char(c: char) -> String {
    match c {
        '\\' => "\\\\".to_string(),
        '"' => "\\\"".to_string(),
        '\'' => "\\'".to_string(),
        '\0' => "\\0".to_string(), // Null character
        '\t' => "\\t".to_string(), // \x09, Tab
        '\n' => "\\n".to_string(), // \x0a, Newline
        '\r' => "\\r".to_string(), // \x0d, Carriage return
        '\x00'..='\x1f' | '\u{80}'..='\u{ff}' => {
            // For control characters (non-printable) characters, and extended ASCII codes,
            // use hexadecimal escape
            format!("\\x{:02x}", c as u8)
        }
        _ => c.to_string(),
    }
}

pub fn escape_string(s: &str) -> String {
    s.chars().map(escape_char).collect::<Vec<String>>().join("")
}

impl Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Integer(n) => write!(f, "{}", n),
            Number::FloatingPoint(n) => write!(f, "{}", n),
        }
    }
}

impl Display for IntegerNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let suffix = if self.unsigned {
            match self.number_type {
                IntegerNumberType::Default => "u",
                IntegerNumberType::Long => "ul",
                IntegerNumberType::LongLong => "ull",
                IntegerNumberType::BitInt => "uwb",
            }
        } else {
            match self.number_type {
                IntegerNumberType::Default => "",
                IntegerNumberType::Long => "l",
                IntegerNumberType::LongLong => "ll",
                IntegerNumberType::BitInt => "wb",
            }
        };
        write!(f, "{}{}", self.value, suffix)
    }
}

impl Display for FloatingPointNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let suffix = if self.decimal {
            match self.number_type {
                FloatingPointNumberType::Default => "dd",
                FloatingPointNumberType::Float => "df",
                FloatingPointNumberType::LongDouble => "dl",
            }
        } else {
            match self.number_type {
                FloatingPointNumberType::Default => "",
                FloatingPointNumberType::Float => "f",
                FloatingPointNumberType::LongDouble => "l",
            }
        };
        write!(f, "{}{}", self.value, suffix)
    }
}

impl Display for Punctuator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            // Arithmetic Operators
            Punctuator::Add => "+",
            Punctuator::Subtract => "-",
            Punctuator::Multiply => "*",
            Punctuator::Divide => "/",
            Punctuator::Modulo => "%",
            Punctuator::Increase => "++",
            Punctuator::Decrease => "--",

            // Relational Operators
            Punctuator::Equal => "==",
            Punctuator::NotEqual => "!=",
            Punctuator::GreaterThan => ">",
            Punctuator::LessThan => "<",
            Punctuator::GreaterThanOrEqual => ">=",
            Punctuator::LessThanOrEqual => "<=",

            // Logical Operators
            Punctuator::And => "&&",
            Punctuator::Or => "||",
            Punctuator::Not => "!",

            // Bitwise Operators
            Punctuator::BitwiseAnd => "&",
            Punctuator::BitwiseOr => "|",
            Punctuator::BitwiseXor => "^",
            Punctuator::BitwiseNot => "~",
            Punctuator::ShiftLeft => "<<",
            Punctuator::ShiftRight => ">>",

            // Assignment Operators
            Punctuator::Assign => "=",
            Punctuator::AddAssign => "+=",
            Punctuator::SubtractAssign => "-=",
            Punctuator::MultiplyAssign => "*=",
            Punctuator::DivideAssign => "/=",
            Punctuator::ModulusAssign => "%=",
            Punctuator::BitwiseAndAssign => "&=",
            Punctuator::BitwiseOrAssign => "|=",
            Punctuator::BitwiseXorAssign => "^=",
            Punctuator::ShiftLeftAssign => "<<=",
            Punctuator::ShiftRightAssign => ">>=",

            // Conditional Operator
            Punctuator::QuestionMark => "?",

            // Miscellaneous Operators
            Punctuator::Comma => ",",
            Punctuator::Dot => ".",
            Punctuator::Arrow => "->",

            // Brackets and Delimiters
            Punctuator::BraceOpen => "{",
            Punctuator::BraceClose => "}",
            Punctuator::BracketOpen => "[",
            Punctuator::BracketClose => "]",
            Punctuator::ParenthesisOpen => "(",
            Punctuator::ParenthesisClose => ")",
            Punctuator::Semicolon => ";",
            Punctuator::Colon => ":",
            Punctuator::Ellipsis => "...",
            Punctuator::AttributeOpen => "[[",
            Punctuator::AttributeClose => "]]",
        };
        write!(f, "{}", symbol)
    }
}

// C23 keywords
// This list contains all C keywords, including those introduced in C23,
// but excludes any keywords that have been deprecated or removed in C23.
// It is used to prevent keywords from being used as macro names in the preprocessor.
// Reference: https://en.cppreference.com/w/c/keyword.html
pub const C23_KEYWORDS: [&str; 53] = [
    "alignas",
    "alignof",
    "auto",
    "bool",
    "break",
    "case",
    "char",
    "const",
    "constexpr",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "false",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "nullptr",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "static_assert",
    "struct",
    "switch",
    "thread_local",
    "true",
    "typedef",
    "typeof",
    "typeof_unqual",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Atomic",
    "_BitInt",
    "_Complex",
    "_Decimal128",
    "_Decimal32",
    "_Decimal64",
    "_Generic",
    "_Imaginary",
];

#[cfg(test)]
mod tests {
    use crate::token::{CharType, StringType, Token};

    #[test]
    fn test_char_display() {
        assert_eq!(Token::Char('a', CharType::Default).to_string(), "'a'");
        assert_eq!(Token::Char('文', CharType::Wide).to_string(), "L'文'");
        assert_eq!(Token::Char('文', CharType::UTF16).to_string(), "u'文'");
        assert_eq!(Token::Char('文', CharType::UTF32).to_string(), "U'文'");
        assert_eq!(Token::Char('文', CharType::UTF8).to_string(), "u8'文'");

        // Test with escape sequences
        assert_eq!(Token::Char('\t', CharType::Default).to_string(), "'\\t'");
        assert_eq!(Token::Char('\n', CharType::Default).to_string(), "'\\n'");
        assert_eq!(Token::Char('\r', CharType::Default).to_string(), "'\\r'");
        assert_eq!(Token::Char('\\', CharType::Default).to_string(), "'\\\\'");
        assert_eq!(Token::Char('"', CharType::Default).to_string(), "'\\\"'");
        assert_eq!(Token::Char('\'', CharType::Default).to_string(), "'\\''");
        assert_eq!(Token::Char('\0', CharType::Default).to_string(), "'\\0'");
        assert_eq!(
            Token::Char('\x01', CharType::Default).to_string(),
            "'\\x01'"
        );
        assert_eq!(
            Token::Char('\x1f', CharType::Default).to_string(),
            "'\\x1f'"
        );
        assert_eq!(
            Token::Char('\u{80}', CharType::Default).to_string(),
            "'\\x80'"
        );
        assert_eq!(
            Token::Char('\u{FF}', CharType::Default).to_string(),
            "'\\xff'"
        );
    }

    #[test]
    fn test_string_display() {
        assert_eq!(
            Token::String("hello".to_string(), StringType::Default).to_string(),
            "\"hello\""
        );
        assert_eq!(
            Token::String("文✨".to_string(), StringType::Wide).to_string(),
            "L\"文✨\""
        );
        assert_eq!(
            Token::String("文✨".to_string(), StringType::UTF16).to_string(),
            "u\"文✨\""
        );
        assert_eq!(
            Token::String("文✨".to_string(), StringType::UTF32).to_string(),
            "U\"文✨\""
        );
        assert_eq!(
            Token::String("文✨".to_string(), StringType::UTF8).to_string(),
            "u8\"文✨\""
        );

        // Test with escape sequences
        assert_eq!(
            Token::String("hello\t\n\r\\\'\"\0world".to_string(), StringType::Default).to_string(),
            "\"hello\\t\\n\\r\\\\\\\'\\\"\\0world\""
        );
        assert_eq!(
            Token::String(
                "文✨\x01\x1f\u{80}\u{ff}world".to_string(),
                StringType::Default
            )
            .to_string(),
            "\"文✨\\x01\\x1f\\x80\\xffworld\""
        );
    }
}
