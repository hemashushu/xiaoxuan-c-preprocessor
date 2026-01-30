// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{error::PreprocessError, location::Location, range::Range};

/// Represents a token for the C preprocessor.
///
/// In general, C language tokens are lexed after preprocessing, but
/// since this ANCPP library is designed to be a part of ANCC (a C compiler),
/// it is reasonable to lex tokens further as close as possible to the C language,
/// This design simplifies the integration between the preprocessor and the compiler,
/// and improves the overall efficiency.
///
/// The following tokens are only used during preprocessing and
/// will not appear in the final token stream:
///
/// - `DirectiveStart`
/// - `DirectiveEnd`
/// - `FilePath`
/// - `FunctionLikeMacroIdentifier`
/// - `Pound`
/// - `PoundPound`
///
/// The C language parser should filter out these tokens before parsing.
///
/// References:
/// - https://gcc.gnu.org/onlinedocs/cpp/Tokenization.html
/// - https://en.cppreference.com/w/c/language/translation_phases.html#Phase_3
#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // A preprocessing identifier, includes:
    //
    // - C Keywords
    // - C Type, Struct, Union and Enum names
    // - C Variable names
    // - C Function names
    // - Directive names
    // - Object-like macro names
    //
    // Identifiers consist of a sequence of letters (including non-Latin letters such as Chinese ideograms),
    // digits, or underscores, starting with a letter or underscore.
    //
    // Identifiers may contain namespace separators (`::`) in attribute names, e.g.
    // - `[[foo::bar]]`
    // - `[[some::attribute(123)]]`
    Identifier(String),

    // A preprocessing number, which includes all standard integer and floating-point constants in C.
    // Such numbers may start with an optional period (`.`), must have at least one decimal digit (`0`-`9`),
    // and can contain letters, digits, underscores, periods, and exponent markers.
    // Integer constants may have suffixes like `u`, `U`, `l`, `L`, `ll`, `LL`, `wb`, or `WB`.
    // Floating-point constants may have suffixes like `f`, `F`, `l`, `L`, `dd`, `DD`, `df`, `DF`, `dl`, or `DL`.
    // A number also may have prefixes like `0x` (hexadecimal), `0b` (binary), or `0o` (octal).
    // Exponents are two-character sequences like 'e+', 'e-', 'E+', 'E-', 'p+', 'p-', 'P+', or 'P-'.
    // Exponents beginning with 'p' or 'P' are used for hexadecimal floating-point constants.
    // See: https://en.cppreference.com/w/c/language/floating_constant
    Number(Number),

    // A string literal.
    // String are enclosed in double quotes (`"`), embedded quotes must be escaped with a backslash.
    //
    // Escape sequences supported in string literals include:
    // - Simple escape sequences: `\'`, `\"`, `\?`, `\\`, `\a`, `\b`, `\f`, `\n`, `\r`, `\t`, `\v`
    // - Octal escape sequences: `\nnn` (1 to 3 octal digits)
    // - Hexadecimal escape sequences: `\xn...`, `\Xn...` (1 or more hexadecimal digits, however,
    //   only two hexadecimal digits are allowed in ANCPP, e.g., `\x41` for 'A', `\x7A` for 'z')
    // - Universal character names: `\unnnn`, `\Unnnnnnnn` (4 or 8 hexadecimal digits)
    //
    // See:
    // https://en.cppreference.com/w/c/language/escape.html
    // https://en.cppreference.com/w/c/language/string_literal.html
    String(String, StringEncoding),

    // A (single) character constant, e.g., 'a', '\t', '文', '\u6587', and '\U2005E'.
    // ANCPP does not support multicharacter constants like 'abc', L'abc', '\1\2\3\4'.
    //
    // See:
    // - https://en.wikipedia.org/wiki/Escape_sequences_in_C
    // - https://en.cppreference.com/w/c/language/character_constant.html
    Char(char, CharEncoding),

    // A punctuator: any punctuation character meaningful in C or C++,
    // except for '@', '$', and '`', which are not considered C punctuators.
    Punctuator(Punctuator),

    // Directive start token.
    // It is the pound sign (`#`) at the beginning of a line indicates a preprocessing directive.
    DirectiveStart,

    // Directive end token.
    // It is literally the `newline` (representing either '\n' or '\r\n') in the directive line.
    //
    // Newlines in C code are treated as whitespace, while newlines in
    // directive lines are special tokens.
    // They are used to separate preprocessing directives and other tokens in the token stream.
    //
    // Each directive occupies one line and has the following format:
    //
    // - (optional leading whitespace)
    // - '#' character.
    // - (optional whitespace)
    // - preprocessing instruction (e.g. `define`, `include`).
    // - arguments (depends on the instruction).
    // - line break (newline) or end of file.
    //
    // The null directive (# followed by a line break) is allowed and has no effect.
    // But ANCPP disallows empty directive lines to avoid confusion.
    // See: https://en.cppreference.com/w/c/preprocessor.html
    DirectiveEnd,

    // The stringizing operator.
    // It is pound (`#`) used in macro replacement.
    //
    // Stringizing is used to produce a string literal and can only be applied to
    // macro parameters (includes `__VA_ARGS__`).
    // For example, `#define FOO(x) #x` will expand `FOO(abc)` to `"abc"` and `FOO(123)` to `"123"`.
    Pound,

    // The token concatenation operator.
    //
    // It is pound-pound (`##`) used in macro replacement.
    //
    // Token concatenation joins two or more tokens into a single identifier.
    // The tokens are concatenated without any whitespace in between, and the tokens
    // are not expanded before concatenation.
    // Token concatenation can only be used in function-like macro replacement.
    //
    // The result of _Token concatenation_ operator (`##`) must be a valid C identifier:
    // the first token must be an identifier, and the remaining tokens must be either
    // identifiers or integer numbers. For example, `foo ## bar` and `sprite ## 2 ## b` are valid,
    // while `9 ## s` and `+ ## =` are invalid.
    PoundPound,

    // The file path of the directive `#include` and `#embed`,
    // and the operator `__has_include` and `__has_embed`.
    //
    // Note that file path is not a string literal,
    // because it does not support escape sequences like `\x40` and `\"`.
    FilePath(String, /* angle-bracket */ bool),

    // The identifier of a function-like macro definition.
    //
    // e.g.
    //
    // ```c
    // #define foo(x, y) ((x) + (y))
    // ```
    //
    // An identifier in the `#define` directive that is immediately followed by
    // a parenthesis `(` represents a function-like macro identifier.
    //
    // Note that an identifier in the `#define` directive followed by a space
    // and parenthesis `(` represents an object-like macro identifier,
    // which is a `Token::Identifier`.
    //
    // This special type of token is needed because whitespaces between the function name
    // and the opening parenthesis `(` are typically ignored in the C language, e.g.
    //
    // - `int foo (...) {...}` is equivalent to `int foo(...) {...}`.
    // - `int a = foo (...)` is equivalent to `int a = foo(...)`.
    //
    // But in macro definitions, whitespaces are significant:
    // - `#define foo(...) ...` defines a function-like macro.
    // - `#define foo (...) ...` defines an object-like macro.
    //
    // This is an inconsistency in the preprocessor design of C/C++ languages.
    FunctionLikeMacroIdentifier(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Punctuator {
    // Arithmetic Operators
    // --------------------
    Add,      // '+'
    Subtract, // '-'
    Multiply, // '*', also is operator `Dereference`
    Divide,   // '/'
    Modulo,   // '%'
    Increase, // '++'
    Decrease, // '--'

    // Relational Operators
    // --------------------
    Equal,              // '=='
    NotEqual,           // '!='
    GreaterThan,        // '>'
    LessThan,           // '<'
    GreaterThanOrEqual, // '>='
    LessThanOrEqual,    // '<='

    // Logical Operators
    // --------------------
    And, // '&&'
    Or,  // '||'
    Not, // '!'

    // Bitwise Operators
    // --------------------
    BitwiseAnd, // '&', also is operator `AddressOf`
    BitwiseOr,  // '|'
    BitwiseXor, // '^'
    BitwiseNot, // '~'
    ShiftLeft,  // '<<'
    ShiftRight, // '>>', note that there is no `>>>` (logical right shift) operator in C

    // Assignment Operators
    // --------------------
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

    // Brackets and Delimiters
    // --------------------
    BraceOpen,        // '{'
    BraceClose,       // '}'
    BracketOpen,      // '['
    BracketClose,     // ']'
    ParenthesisOpen,  // '('
    ParenthesisClose, // ')'
    AttributeOpen,    // '[[' for attributes in C23, e.g., [[nodiscard]]
    AttributeClose,   // ']]' for attributes in C23, e.g., [[nodiscard]]
    Semicolon,        // ';'
    Comma,            // ','

    // Other Operators
    // --------------------
    Dot,          // '.', member access operator
    Arrow,        // '->', pointer member access operator
    QuestionMark, // '?'
    Colon,        // ':'
    Ellipsis,     // '...', Variadic arguments in function definitions
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CharEncoding {
    Default, // Default character, e.g., 'a', '1', '文', note that the data type is `int` instead of `char`.
    Wide,    // Wide character, e.g., L'a', L'文', data type is `wchar_t`
    UTF16,   // UTF-16 character, e.g., u'a', u'文', data type is `char16_t`
    UTF32,   // UTF-32 character, e.g., U'a', U'文', data type is `char32_t`
    UTF8,    // UTF-8 character, e.g., u8'a', u8'文', data type is `char8_t`
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StringEncoding {
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

    // true for unsigned integers, false for signed integers.
    // Suffix `u` or `U` indicates an unsigned integer, e.g., `123u`, `0x1AUL`.
    // Possible suffix are: `u`, `U`, `ul`, `UL`, `ull`, `ULL`, `uwb`, `UWB`
    pub unsigned: bool,

    pub length: IntegerNumberWidth,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum IntegerNumberWidth {
    Default, // no suffix, e.g., 123, the type of the integer constant is the first type in which the value can fit.
    Long,    // suffix "l", "L", e.g., 123l, 123L
    LongLong, // suffix "ll", "LL", e.g., 123ll, 123LL
    BitInt,  // suffix "wb", "WB", e.g., 123wb, 123WB
}

#[derive(Debug, PartialEq, Clone)]
pub struct FloatingPointNumber {
    pub value: String,

    /// true for decimal floating-point numbers (`_Decimal{32|64|128}`), false for binary floating-point numbers.
    /// Suffix `d` or `D` indicates a decimal floating-point number, e.g., `1.23dd`, `4.56DF`.
    /// When it is use alone without combining with `f`, `F`, `l`, or `L`,
    /// it must be `dd` or `DD`, indicating a decimal double-precision.
    ///
    /// Possible suffix are: `dd`, `DD`, `df`, `DF`, `dl`, `DL`
    pub decimal: bool,
    pub length: FloatingPointNumberWidth,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum FloatingPointNumberWidth {
    Default,    // no suffix, e.g., 1.23, an unsuffixed floating constant has type `double`.
    Float,      // suffix "f", "F", e.g., 1.23f, 1.23F
    LongDouble, // suffix "l", "L", e.g., 1.23l, 1.23L
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenWithRange {
    pub token: Token,
    pub range: Range,
}

impl TokenWithRange {
    pub fn new(token: Token, range: Range) -> Self {
        Self { token, range }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct TokenWithLocation {
    pub token: Token,
    pub location: Location,
}

impl TokenWithLocation {
    pub fn new(token: Token, location: Location) -> Self {
        Self { token, location }
    }
}

impl IntegerNumber {
    pub fn new(value: String, unsigned: bool, length: IntegerNumberWidth) -> Self {
        Self {
            value,
            unsigned,
            length,
        }
    }

    pub fn as_u64(&self) -> Result<u64, PreprocessError> {
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

        u64::from_str_radix(src, radix).map_err(|_| {
            PreprocessError::Message(format!("Can not parse integer number: {}", self))
        })
    }
}

impl FloatingPointNumber {
    pub fn new(value: String, decimal: bool, length: FloatingPointNumberWidth) -> Self {
        Self {
            value,
            decimal,
            length,
        }
    }

    pub fn as_f64(&self) -> Result<f64, PreprocessError> {
        if self.value.starts_with("0x") {
            // Hexadecimal floating-point number
            hexfloat2::parse::<f64>(&self.value).map_err(|_| {
                PreprocessError::Message(format!("Can not parse floating-point number: {}", self))
            })
        } else {
            self.value.parse::<f64>().map_err(|_| {
                PreprocessError::Message(format!("Can not parse floating-point number: {}", self))
            })
        }
    }

    pub fn as_f32(&self) -> Result<f32, PreprocessError> {
        if self.value.starts_with("0x") {
            // Hexadecimal floating-point number
            hexfloat2::parse::<f32>(&self.value).map_err(|_| {
                PreprocessError::Message(format!("Can not parse floating-point number: {}", self))
            })
        } else {
            self.value.parse::<f32>().map_err(|_| {
                PreprocessError::Message(format!("Can not parse floating-point number: {}", self))
            })
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
                    StringEncoding::Default => "\"",
                    StringEncoding::Wide => "L\"",
                    StringEncoding::UTF16 => "u\"",
                    StringEncoding::UTF32 => "U\"",
                    StringEncoding::UTF8 => "u8\"",
                };

                // Escape special characters in the string
                let escaped = escape_string(s);
                write!(f, "{}{}\"", quote, escaped)
            }
            Token::Char(c, t) => {
                let prefix = match t {
                    CharEncoding::Default => "'",
                    CharEncoding::Wide => "L'",
                    CharEncoding::UTF16 => "u'",
                    CharEncoding::UTF32 => "U'",
                    CharEncoding::UTF8 => "u8'",
                };

                // Escape special character
                let escaped = escape_char(*c);
                write!(f, "{}{}'", prefix, escaped)
            }
            Token::Punctuator(p) => write!(f, "{}", p),
            Token::DirectiveStart => write!(f, "#"),
            Token::DirectiveEnd => writeln!(f),
            Token::Pound => write!(f, "#"),
            Token::PoundPound => write!(f, "##"),
            Token::FilePath(s, angle_bracket) => {
                if *angle_bracket {
                    write!(f, "<{}>", s)
                } else {
                    write!(f, "\"{}\"", s)
                }
            }
            Token::FunctionLikeMacroIdentifier(s) => write!(f, "{}", s),
        }
    }
}

pub fn escape_char(c: char) -> String {
    match c {
        '\\' => "\\\\".to_string(),
        '\'' => "\\'".to_string(),
        '"' => "\\\"".to_string(),
        '?' => "\\?".to_string(),
        '\x07' => "\\a".to_string(), // \x07, Alert (bell)
        '\x08' => "\\b".to_string(), // \x08, Backspace
        '\t' => "\\t".to_string(),   // \x09, Tab
        '\n' => "\\n".to_string(),   // \x0a, Newline
        '\x0b' => "\\v".to_string(), // \x0b, Vertical tab
        '\x0c' => "\\f".to_string(), // \x0c, Form feed
        '\r' => "\\r".to_string(),   // \x0d, Carriage return
        '\u{0}'..='\u{1f}' | '\u{80}'..='\u{ff}' => {
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
            match self.length {
                IntegerNumberWidth::Default => "u",
                IntegerNumberWidth::Long => "ul",
                IntegerNumberWidth::LongLong => "ull",
                IntegerNumberWidth::BitInt => "uwb",
            }
        } else {
            match self.length {
                IntegerNumberWidth::Default => "",
                IntegerNumberWidth::Long => "l",
                IntegerNumberWidth::LongLong => "ll",
                IntegerNumberWidth::BitInt => "wb",
            }
        };
        write!(f, "{}{}", self.value, suffix)
    }
}

impl Display for FloatingPointNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let suffix = if self.decimal {
            match self.length {
                FloatingPointNumberWidth::Default => "dd",
                FloatingPointNumberWidth::Float => "df",
                FloatingPointNumberWidth::LongDouble => "dl",
            }
        } else {
            match self.length {
                FloatingPointNumberWidth::Default => "",
                FloatingPointNumberWidth::Float => "f",
                FloatingPointNumberWidth::LongDouble => "l",
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

            // Brackets and Delimiters
            Punctuator::BraceOpen => "{",
            Punctuator::BraceClose => "}",
            Punctuator::BracketOpen => "[",
            Punctuator::BracketClose => "]",
            Punctuator::ParenthesisOpen => "(",
            Punctuator::ParenthesisClose => ")",
            Punctuator::AttributeOpen => "[[",
            Punctuator::AttributeClose => "]]",
            Punctuator::Semicolon => ";",
            Punctuator::Comma => ",",

            // Other Operators
            Punctuator::Dot => ".",
            Punctuator::Arrow => "->",
            Punctuator::Colon => ":",
            Punctuator::QuestionMark => "?",
            Punctuator::Ellipsis => "...",
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
    use crate::token::{
        CharEncoding, FloatingPointNumber, FloatingPointNumberWidth, IntegerNumber,
        IntegerNumberWidth, StringEncoding, Token,
    };

    #[test]
    fn test_char_display() {
        assert_eq!(Token::Char('a', CharEncoding::Default).to_string(), "'a'");
        assert_eq!(Token::Char('文', CharEncoding::Wide).to_string(), "L'文'");
        assert_eq!(Token::Char('文', CharEncoding::UTF16).to_string(), "u'文'");
        assert_eq!(Token::Char('文', CharEncoding::UTF32).to_string(), "U'文'");
        assert_eq!(Token::Char('文', CharEncoding::UTF8).to_string(), "u8'文'");

        // Test with escape sequences
        assert_eq!(
            Token::Char('\t', CharEncoding::Default).to_string(),
            "'\\t'"
        );

        assert_eq!(
            Token::Char('\n', CharEncoding::Default).to_string(),
            "'\\n'"
        );

        assert_eq!(
            Token::Char('\r', CharEncoding::Default).to_string(),
            "'\\r'"
        );

        assert_eq!(
            Token::Char('\\', CharEncoding::Default).to_string(),
            "'\\\\'"
        );

        assert_eq!(
            Token::Char('"', CharEncoding::Default).to_string(),
            "'\\\"'"
        );

        assert_eq!(
            Token::Char('\'', CharEncoding::Default).to_string(),
            "'\\''"
        );

        assert_eq!(
            Token::Char('\0', CharEncoding::Default).to_string(),
            "'\\x00'"
        );

        assert_eq!(
            Token::Char('\x01', CharEncoding::Default).to_string(),
            "'\\x01'"
        );

        assert_eq!(
            Token::Char('\x1f', CharEncoding::Default).to_string(),
            "'\\x1f'"
        );

        assert_eq!(
            Token::Char('\u{80}', CharEncoding::Default).to_string(),
            "'\\x80'"
        );

        assert_eq!(
            Token::Char('\u{FF}', CharEncoding::Default).to_string(),
            "'\\xff'"
        );
    }

    #[test]
    fn test_string_display() {
        assert_eq!(
            Token::String("hello".to_string(), StringEncoding::Default).to_string(),
            "\"hello\""
        );

        assert_eq!(
            Token::String("文✨".to_string(), StringEncoding::Wide).to_string(),
            "L\"文✨\""
        );

        assert_eq!(
            Token::String("文✨".to_string(), StringEncoding::UTF16).to_string(),
            "u\"文✨\""
        );

        assert_eq!(
            Token::String("文✨".to_string(), StringEncoding::UTF32).to_string(),
            "U\"文✨\""
        );

        assert_eq!(
            Token::String("文✨".to_string(), StringEncoding::UTF8).to_string(),
            "u8\"文✨\""
        );

        // Test with escape sequences
        assert_eq!(
            Token::String(
                "hello\t\n\r\\\'\"\0world".to_string(),
                StringEncoding::Default
            )
            .to_string(),
            "\"hello\\t\\n\\r\\\\\\\'\\\"\\x00world\""
        );

        assert_eq!(
            Token::String(
                "文✨\x01\x1f\u{80}\u{ff}world".to_string(),
                StringEncoding::Default
            )
            .to_string(),
            "\"文✨\\x01\\x1f\\x80\\xffworld\""
        );
    }

    #[test]
    fn test_integer_number_display() {
        assert_eq!(
            IntegerNumber::new("123".to_string(), false, IntegerNumberWidth::Default).to_string(),
            "123"
        );

        assert_eq!(
            IntegerNumber::new("123".to_string(), true, IntegerNumberWidth::Default).to_string(),
            "123u"
        );

        assert_eq!(
            IntegerNumber::new("123".to_string(), false, IntegerNumberWidth::Long).to_string(),
            "123l"
        );

        assert_eq!(
            IntegerNumber::new("123".to_string(), true, IntegerNumberWidth::Long).to_string(),
            "123ul"
        );

        assert_eq!(
            IntegerNumber::new("123".to_string(), true, IntegerNumberWidth::LongLong).to_string(),
            "123ull"
        );

        assert_eq!(
            IntegerNumber::new("123".to_string(), false, IntegerNumberWidth::BitInt).to_string(),
            "123wb"
        );

        assert_eq!(
            IntegerNumber::new("123".to_string(), true, IntegerNumberWidth::BitInt).to_string(),
            "123uwb"
        );
    }

    #[test]
    fn test_floating_point_number_display() {
        assert_eq!(
            FloatingPointNumber::new("1.23".to_string(), false, FloatingPointNumberWidth::Default)
                .to_string(),
            "1.23"
        );

        assert_eq!(
            FloatingPointNumber::new("1.23".to_string(), true, FloatingPointNumberWidth::Default)
                .to_string(),
            "1.23dd"
        );

        assert_eq!(
            FloatingPointNumber::new("1.23".to_string(), false, FloatingPointNumberWidth::Float)
                .to_string(),
            "1.23f"
        );

        assert_eq!(
            FloatingPointNumber::new("1.23".to_string(), true, FloatingPointNumberWidth::Float)
                .to_string(),
            "1.23df"
        );

        assert_eq!(
            FloatingPointNumber::new(
                "1.23".to_string(),
                false,
                FloatingPointNumberWidth::LongDouble
            )
            .to_string(),
            "1.23l"
        );

        assert_eq!(
            FloatingPointNumber::new(
                "1.23".to_string(),
                true,
                FloatingPointNumberWidth::LongDouble
            )
            .to_string(),
            "1.23dl"
        );
    }
}
