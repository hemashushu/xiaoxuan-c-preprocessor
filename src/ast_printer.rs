// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::io::Write;

use crate::{
    ast::{Condition, Define, If, Program, Statement},
    token::TokenWithRange,
};

const DEFAULT_INDENT_CHARS: &str = "    ";

/// Join tokens with spaces
pub fn join_tokens(tokens: &[TokenWithRange]) -> String {
    tokens
        .iter()
        .map(|TokenWithRange { token, .. }| token.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}

/// Join tokens without spaces
/// This function is used for generating file paths
pub fn join_tokens_without_spaces(tokens: &[TokenWithRange]) -> String {
    tokens
        .iter()
        .map(|TokenWithRange { token, .. }| token.to_string())
        .collect::<Vec<_>>()
        .join("")
}

fn print_define<W: Write>(
    writer: &mut W,
    define: &Define,
    indent_level: usize,
) -> std::io::Result<()> {
    // Join parameter names with commas
    let join_params = |params: &[String]| {
        params
            .iter()
            .map(|p| p.to_owned())
            .collect::<Vec<_>>()
            .join(", ")
    };

    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);

    match define {
        Define::ObjectLike {
            identifier: (name, _),
            definition,
        } => {
            if definition.is_empty() {
                writeln!(writer, "{}#define {}", indent, name)
            } else {
                writeln!(
                    writer,
                    "{}#define {} {}",
                    indent,
                    name,
                    join_tokens(definition)
                )
            }
        }
        Define::FunctionLike {
            identifier: (name, _),
            parameters,
            definition,
        } => {
            let params_text = join_params(parameters);
            writeln!(
                writer,
                "{}#define {}({}) {}",
                indent,
                name,
                params_text,
                join_tokens(definition)
            )
        }
    }
}

fn print_if<W: Write>(writer: &mut W, if_: &If, indent_level: usize) -> std::io::Result<()> {
    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);

    let mut first_branch = true;

    for branch in &if_.branches {
        let condition_text = match &branch.condition {
            Condition::Expression(tokens) => tokens
                .iter()
                .map(|TokenWithRange { token, .. }| token.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            Condition::Defined(id, _) => id.to_owned(),
            Condition::NotDefined(id, _) => id.to_owned(),
        };

        let directive_text = match &branch.condition {
            Condition::Expression(_) => {
                if first_branch {
                    "#if"
                } else {
                    "#elif"
                }
            }
            Condition::Defined(_, _) => {
                if first_branch {
                    "#ifdef"
                } else {
                    "#elifdef"
                }
            }
            Condition::NotDefined(_, _) => {
                if first_branch {
                    "#ifndef"
                } else {
                    "#elifndef"
                }
            }
        };

        writeln!(writer, "{}{} {}", indent, directive_text, condition_text)?;

        for statement in &branch.consequence {
            print_statement(writer, statement, indent_level + 1)?;
        }

        first_branch = false;
    }

    if let Some(alternative) = &if_.alternative {
        writeln!(writer, "{}#else", indent)?;
        for statement in alternative {
            print_statement(writer, statement, indent_level + 1)?;
        }
    }

    writeln!(writer, "{}#endif", indent)
}

fn print_code<W: Write>(
    writer: &mut W,
    tokens: &[TokenWithRange],
    indent_level: usize,
) -> std::io::Result<()> {
    let code_text = tokens
        .iter()
        .map(|TokenWithRange { token, .. }| token.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);
    writeln!(writer, "{}{}", indent, code_text)
}

/// Print a single statement to the given writer
pub fn print_statement<W: Write>(
    writer: &mut W,
    statement: &Statement,
    indent_level: usize,
) -> std::io::Result<()> {
    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);

    match statement {
        Statement::Pragma(pragma) => {
            // print_pragma(writer, pragma, indent_level),
            writeln!(
                writer,
                "{}#pragma {}",
                indent,
                join_tokens(&pragma.components)
            )
        }
        Statement::Define(define) => print_define(writer, define, indent_level),
        Statement::Undef(name, _) => {
            writeln!(writer, "{}#undef {}", indent, name)
        }
        Statement::Include(components) => {
            // print_include(writer, include, indent_level),
            writeln!(writer, "{}#include {}", indent, join_tokens(components))
        }
        Statement::Embed(components) => {
            // print_embed(writer, embed, indent_level),
            writeln!(writer, "{}#embed {}", indent, join_tokens(components))
        }
        Statement::If(if_) => print_if(writer, if_, indent_level),
        Statement::Error(msg, _) => {
            writeln!(writer, "{}#error \"{}\"", indent, msg)
        }
        Statement::Warning(msg, _) => {
            writeln!(writer, "{}#warning \"{}\"", indent, msg)
        }
        Statement::Code(code) => print_code(writer, code, indent_level),
    }
}

/// Print the entire program to the given writer
pub fn print_program<W: Write>(writer: &mut W, program: &Program) -> std::io::Result<()> {
    for statement in &program.statements {
        print_statement(writer, statement, 0)?;
    }
    Ok(())
}

/// Print the entire program to a string
#[allow(dead_code)]
pub fn print_program_to_string(program: &Program) -> String {
    let mut output = Vec::new();
    print_program(&mut output, program).unwrap();
    String::from_utf8(output).unwrap()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Branch, Condition, Define, If, Pragma, Program, Statement},
        ast_printer::print_program_to_string,
        range::Range,
        token::{
            CharEncoding, IntegerNumber, IntegerNumberWidth, Number, Punctuator, StringEncoding,
            Token, TokenWithRange,
        },
    };

    // Helper functions to create tokens for testing
    impl TokenWithRange {
        fn new_identifier(s: &str) -> Self {
            TokenWithRange::new(Token::Identifier(s.to_owned()), Range::default())
        }

        fn new_char(c: char) -> Self {
            TokenWithRange::new(Token::Char(c, CharEncoding::Default), Range::default())
        }

        fn new_string(s: &str) -> Self {
            TokenWithRange::new(
                Token::String(s.to_owned(), StringEncoding::Default),
                Range::default(),
            )
        }

        fn new_integer_number(i: i32) -> Self {
            TokenWithRange::new(
                Token::Number(Number::Integer(IntegerNumber::new(
                    i.to_string(),
                    false,
                    IntegerNumberWidth::Default,
                ))),
                Range::default(),
            )
        }

        fn new_punctuator(p: Punctuator) -> Self {
            TokenWithRange::new(Token::Punctuator(p), Range::default())
        }
    }

    #[test]
    fn test_print_define() {
        // Test for an object-like macro

        let define_object_like = Define::ObjectLike {
            identifier: ("MAX".to_string(), Range::default()),
            definition: vec![TokenWithRange::new_integer_number(100)],
        };

        let program_object_like = Program {
            statements: vec![Statement::Define(define_object_like)],
        };

        let output_object_like = print_program_to_string(&program_object_like);
        assert_eq!(output_object_like, "#define MAX 100\n");

        // Test for an empty object-like macro

        let define_object_like_empty = Define::ObjectLike {
            identifier: ("EMPTY".to_string(), Range::default()),
            definition: vec![],
        };

        let program_object_like_empty = Program {
            statements: vec![Statement::Define(define_object_like_empty)],
        };

        let output_object_like_empty = print_program_to_string(&program_object_like_empty);
        assert_eq!(output_object_like_empty, "#define EMPTY\n");

        // Test for a function-like macro

        let define_function_like = Define::FunctionLike {
            identifier: ("SQUARE".to_string(), Range::default()),
            parameters: vec!["X".to_string(), "Y".to_string()],
            definition: vec![
                // omit lots of parentheses for simplicity
                TokenWithRange::new_identifier("X"),
                TokenWithRange::new_punctuator(Punctuator::Multiply),
                TokenWithRange::new_identifier("Y"),
            ],
        };

        let program_function_like = Program {
            statements: vec![Statement::Define(define_function_like)],
        };
        let output_function_like = print_program_to_string(&program_function_like);
        assert_eq!(output_function_like, "#define SQUARE(X, Y) X * Y\n");
    }

    #[test]
    fn test_print_undef() {
        let name = "FOO";
        let range = Range::default();
        let statement = Statement::Undef(name.to_string(), range);

        let program = Program {
            statements: vec![statement],
        };

        let output = print_program_to_string(&program);
        assert_eq!(output, "#undef FOO\n");
    }

    #[test]
    fn test_print_include() {
        // Test for including with an identifier
        let include_with_identifier = vec![TokenWithRange::new_identifier("HEADER")];
        let program_with_identifier = Program {
            statements: vec![Statement::Include(include_with_identifier)],
        };

        let output_with_identifier = print_program_to_string(&program_with_identifier);
        assert_eq!(output_with_identifier, "#include HEADER\n");

        // Test for including with a system file path
        let include_with_system_header = vec![TokenWithRange::new(
            Token::FilePath("stdio.h".to_owned(), true),
            Range::default(),
        )];

        let output_with_system_header = Program {
            statements: vec![Statement::Include(include_with_system_header)],
        };
        let output_with_system_header_str = print_program_to_string(&output_with_system_header);
        assert_eq!(output_with_system_header_str, "#include <stdio.h>\n");

        // Test for including with a quoted file path
        let include_with_quoted_header = vec![TokenWithRange::new(
            Token::FilePath("my_header.h".to_owned(), false),
            Range::default(),
        )];

        let program_with_quoted_header = Program {
            statements: vec![Statement::Include(include_with_quoted_header)],
        };
        let output_with_quoted_header = print_program_to_string(&program_with_quoted_header);
        assert_eq!(output_with_quoted_header, "#include \"my_header.h\"\n");
    }

    #[test]
    fn test_print_embed() {
        // Test for embedding with an identifier
        let embed_with_identifier = vec![TokenWithRange::new_identifier("HEMASHUSHU_JPEG_FILE")];
        let program_with_identifier = Program {
            statements: vec![Statement::Embed(embed_with_identifier)],
        };

        let output_with_identifier = print_program_to_string(&program_with_identifier);
        assert_eq!(output_with_identifier, "#embed HEMASHUSHU_JPEG_FILE\n");

        // Test for embedding with a system file path
        let embed_with_system_header = vec![TokenWithRange::new(
            Token::FilePath("hippo.bin".to_owned(), true),
            Range::default(),
        )];

        let output_with_system_header = Program {
            statements: vec![Statement::Embed(embed_with_system_header)],
        };
        let output_with_system_header_str = print_program_to_string(&output_with_system_header);
        assert_eq!(output_with_system_header_str, "#embed <hippo.bin>\n");

        // Test for embedding with a quoted file path
        let embed_with_quoted_header = vec![TokenWithRange::new(
            Token::FilePath("spark.png".to_owned(), false),
            Range::default(),
        )];

        let program_with_quoted_header = Program {
            statements: vec![Statement::Embed(embed_with_quoted_header)],
        };
        let output_with_quoted_header = print_program_to_string(&program_with_quoted_header);
        assert_eq!(output_with_quoted_header, "#embed \"spark.png\"\n");

        // Test for embedding with `limit`, `suffix`, `prefix`, and `if_empty` parameters
        let embed_with_params = vec![
            TokenWithRange::new(
                Token::FilePath("/dev/random".to_string(), true),
                Range::default(),
            ),
            // limit(100)
            TokenWithRange::new_identifier("limit"),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisOpen),
            TokenWithRange::new_integer_number(100),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisClose),
            // prefix(1, 2, 3)
            TokenWithRange::new_identifier("prefix"),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisOpen),
            TokenWithRange::new_integer_number(1),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(2),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(3),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisClose),
            // suffix(7, 8, 9)
            TokenWithRange::new_identifier("suffix"),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisOpen),
            TokenWithRange::new_integer_number(7),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(8),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(9),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisClose),
            // if_empty(11, 13, 17, 19)
            TokenWithRange::new_identifier("if_empty"),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisOpen),
            TokenWithRange::new_integer_number(11),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(13),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(17),
            TokenWithRange::new_punctuator(Punctuator::Comma),
            TokenWithRange::new_integer_number(19),
            TokenWithRange::new_punctuator(Punctuator::ParenthesisClose),
        ];

        let program_with_params = Program {
            statements: vec![Statement::Embed(embed_with_params)],
        };
        let output_with_params = print_program_to_string(&program_with_params);

        assert_eq!(
            output_with_params,
            "#embed </dev/random> limit ( 100 ) prefix ( 1 , 2 , 3 ) suffix ( 7 , 8 , 9 ) if_empty ( 11 , 13 , 17 , 19 )\n"
        );
    }

    #[test]
    fn test_print_if() {
        // Test for a simple `#if` directive:
        // `#if CONDITION ... #endif`
        let if_simple = If {
            branches: vec![Branch {
                condition: Condition::Expression(vec![
                    TokenWithRange::new_identifier("EDITION"),
                    TokenWithRange::new_punctuator(Punctuator::Equal),
                    TokenWithRange::new_integer_number(2025),
                ]),
                consequence: vec![Statement::Code(vec![
                    TokenWithRange::new_identifier("int"),
                    TokenWithRange::new_identifier("x"),
                    TokenWithRange::new_punctuator(Punctuator::Semicolon),
                ])],
            }],
            alternative: None,
        };

        let program_simple = Program {
            statements: vec![Statement::If(if_simple)],
        };
        let output_simple = print_program_to_string(&program_simple);

        assert_eq!(output_simple, "#if EDITION == 2025\n    int x ;\n#endif\n");

        // Test for `#ifdef` directive with `else`:
        // `#ifdef IDENTIFIER ... #else ... #endif`
        let ifdef_else = If {
            branches: vec![Branch {
                condition: Condition::Defined("IDENTIFIER".to_string(), Range::default()),
                consequence: vec![Statement::Code(vec![
                    TokenWithRange::new_identifier("int"),
                    TokenWithRange::new_identifier("x"),
                    TokenWithRange::new_punctuator(Punctuator::Semicolon),
                ])],
            }],
            alternative: Some(vec![Statement::Code(vec![
                TokenWithRange::new_identifier("int"),
                TokenWithRange::new_identifier("y"),
                TokenWithRange::new_punctuator(Punctuator::Semicolon),
            ])]),
        };

        let program_ifdef_else = Program {
            statements: vec![Statement::If(ifdef_else)],
        };
        let output_ifdef_else_program = print_program_to_string(&program_ifdef_else);

        assert_eq!(
            output_ifdef_else_program,
            "#ifdef IDENTIFIER\n    int x ;\n#else\n    int y ;\n#endif\n"
        );

        // Test for multiple branches with `#elif`, `#elifdef`, and `#else`:
        // `#if CONDITION1 ... #elif CONDITION2 ... #elifdef IDENTIFIER ... #else ... #endif`
        let if_multi = If {
            branches: vec![
                Branch {
                    condition: Condition::Expression(vec![
                        TokenWithRange::new_identifier("defined"),
                        TokenWithRange::new_identifier("ANCC"),
                    ]),
                    consequence: vec![Statement::Code(vec![
                        TokenWithRange::new_identifier("a"),
                        TokenWithRange::new_punctuator(Punctuator::Semicolon),
                    ])],
                },
                Branch {
                    condition: Condition::Expression(vec![
                        TokenWithRange::new_identifier("EDITION"),
                        TokenWithRange::new_punctuator(Punctuator::Equal),
                        TokenWithRange::new_integer_number(2025),
                    ]),
                    consequence: vec![Statement::Code(vec![
                        TokenWithRange::new_identifier("b"),
                        TokenWithRange::new_punctuator(Punctuator::Semicolon),
                    ])],
                },
                Branch {
                    condition: Condition::Defined("FOO".to_string(), Range::default()),
                    consequence: vec![Statement::Code(vec![
                        TokenWithRange::new_identifier("c"),
                        TokenWithRange::new_punctuator(Punctuator::Semicolon),
                    ])],
                },
            ],
            alternative: Some(vec![Statement::Code(vec![
                TokenWithRange::new_identifier("d"),
                TokenWithRange::new_punctuator(Punctuator::Semicolon),
            ])]),
        };

        let program_if_multi = Program {
            statements: vec![Statement::If(if_multi)],
        };
        let output_if_multi_program = print_program_to_string(&program_if_multi);

        assert_eq!(
            output_if_multi_program,
            "#if defined ANCC\n    a ;\n#elif EDITION == 2025\n    b ;\n#elifdef FOO\n    c ;\n#else\n    d ;\n#endif\n"
        );
    }

    #[test]
    fn test_print_error() {
        let msg = "This is an error message";
        let statement = Statement::Error(msg.to_string(), Range::default());

        let program = Program {
            statements: vec![statement],
        };
        let output = print_program_to_string(&program);

        assert_eq!(output, "#error \"This is an error message\"\n");
    }

    #[test]
    fn test_print_warning() {
        let msg = "This is a warning message";
        let statement = Statement::Warning(msg.to_string(), Range::default());

        let program = Program {
            statements: vec![statement],
        };
        let output = print_program_to_string(&program);

        assert_eq!(output, "#warning \"This is a warning message\"\n");
    }

    #[test]
    fn test_print_pragma() {
        let pragma = Pragma {
            components: vec![
                TokenWithRange::new_identifier("STDC"),
                TokenWithRange::new_identifier("FENV_ACCESS"),
                TokenWithRange::new_identifier("ON"),
            ],
        };

        let program = Program {
            statements: vec![Statement::Pragma(pragma)],
        };

        let output = print_program_to_string(&program);
        assert_eq!(output, "#pragma STDC FENV_ACCESS ON\n");
    }

    #[test]
    fn test_print_code() {
        let code = vec![
            TokenWithRange::new_identifier("int"),
            TokenWithRange::new_identifier("x"),
            TokenWithRange::new_punctuator(Punctuator::Semicolon),
        ];
        let statement = Statement::Code(code);
        let program = Program {
            statements: vec![statement],
        };

        let output = print_program_to_string(&program);
        assert_eq!(output, "int x ;\n");
    }

    #[test]
    fn test_print_program() {
        let program = Program {
            statements: vec![
                Statement::Pragma(Pragma {
                    components: vec![
                        TokenWithRange::new_identifier("STDC"),
                        TokenWithRange::new_identifier("FENV_ACCESS"),
                        TokenWithRange::new_identifier("ON"),
                    ],
                }),
                Statement::Define(Define::ObjectLike {
                    identifier: ("MAX".to_string(), Range::default()),
                    definition: vec![TokenWithRange::new_integer_number(100)],
                }),
                Statement::Include(vec![TokenWithRange::new(
                    Token::FilePath("stdio.h".to_string(), true),
                    Range::default(),
                )]),
                Statement::Code(vec![
                    TokenWithRange::new_identifier("printf"),
                    TokenWithRange::new_punctuator(Punctuator::ParenthesisOpen),
                    TokenWithRange::new_string("Hello, World!"),
                    TokenWithRange::new_punctuator(Punctuator::ParenthesisClose),
                    TokenWithRange::new_punctuator(Punctuator::Semicolon),
                ]),
            ],
        };

        let output = print_program_to_string(&program);
        assert_eq!(
            output,
            "#pragma STDC FENV_ACCESS ON\n#define MAX 100\n#include <stdio.h>\nprintf ( \"Hello, World!\" ) ;\n"
        );
    }
}
