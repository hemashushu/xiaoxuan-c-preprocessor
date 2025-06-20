// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::io::Write;

use crate::{
    TokenWithRange,
    ast::{Condition, Define, Embed, If, Include, Pragma, Program, Statement},
    token::Token,
};

const DEFAULT_INDENT_CHARS: &str = "    ";

fn print_pragma<W: Write>(
    writer: &mut W,
    pragma: &Pragma,
    indent_level: usize,
) -> std::io::Result<()> {
    let pragma_text = pragma
        .parts
        .iter()
        .map(|TokenWithRange { token, .. }| token.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);
    writeln!(writer, "{}#pragma {}", indent, pragma_text)
}

fn print_define<W: Write>(
    writer: &mut W,
    define: &Define,
    indent_level: usize,
) -> std::io::Result<()> {
    let generate_definition_text = |definition: &[TokenWithRange]| {
        definition
            .iter()
            .map(|TokenWithRange { token, .. }| match token {
                // Handle special cases for tokens that need to be escaped
                Token::Pound => "#".to_owned(),
                Token::PoundPound => "##".to_owned(),
                _ => token.to_string(),
            })
            .collect::<Vec<_>>()
            .join(" ")
    };

    let generate_params_text = |params: &[String]| {
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
                    generate_definition_text(definition)
                )
            }
        }
        Define::FunctionLike {
            identifier: (name, _),
            parameters,
            definition,
        } => {
            let params_text = generate_params_text(parameters);
            writeln!(
                writer,
                "{}#define {}({}) {}",
                indent,
                name,
                params_text,
                generate_definition_text(definition)
            )
        }
    }
}

fn print_include<W: Write>(
    writer: &mut W,
    include: &Include,
    indent_level: usize,
) -> std::io::Result<()> {
    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);

    match include {
        Include::Identifier(id, ..) => {
            writeln!(writer, "{}#include {}", indent, id)
        }
        Include::FilePath {
            file_path: (file_path_string, ..),
            is_system_header,
        } => {
            if *is_system_header {
                writeln!(writer, "{}#include <{}>", indent, file_path_string)
            } else {
                writeln!(writer, "{}#include \"{}\"", indent, file_path_string)
            }
        }
    }
}

fn print_embed<W: Write>(
    writer: &mut W,
    embed: &Embed,
    indent_level: usize,
) -> std::io::Result<()> {
    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);

    match embed {
        Embed::Identifier(id, ..) => {
            writeln!(writer, "{}#embed {}", indent, id)
        }
        Embed::FilePath {
            file_path: (file_path_string, ..),
            is_system_file,
            limit,
            suffix,
            prefix,
            if_empty,
        } => {
            if *is_system_file {
                write!(writer, "{}#embed <{}>", indent, file_path_string)?;
            } else {
                write!(writer, "{}#embed \"{}\"", indent, file_path_string)?;
            }

            if let Some(limit_value) = limit {
                write!(writer, " limit({})", limit_value)?;
            }

            let print_binary_data = |data: &[u8]| -> String {
                if data.is_empty() {
                    return String::new();
                }
                data.iter()
                    .map(|byte| format!("0x{:02x}", byte))
                    .collect::<Vec<_>>()
                    .join(", ")
            };

            if !prefix.is_empty() {
                write!(writer, " prefix({})", print_binary_data(prefix))?;
            }

            if !suffix.is_empty() {
                write!(writer, " suffix({})", print_binary_data(suffix))?;
            }

            if let Some(if_empty_data) = if_empty {
                write!(writer, " if_empty({})", print_binary_data(if_empty_data))?;
            }

            writeln!(writer)
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
    code: &[TokenWithRange],
    indent_level: usize,
) -> std::io::Result<()> {
    let code_text = code
        .iter()
        .map(|TokenWithRange { token, .. }| token.to_string())
        .collect::<Vec<_>>()
        .join(" ");

    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);
    writeln!(writer, "{}{}", indent, code_text)
}

fn print_statement<W: Write>(
    writer: &mut W,
    statement: &Statement,
    indent_level: usize,
) -> std::io::Result<()> {
    let indent = DEFAULT_INDENT_CHARS.repeat(indent_level);

    match statement {
        Statement::Pragma(pragma) => print_pragma(writer, pragma, indent_level),
        Statement::Define(define) => print_define(writer, define, indent_level),
        Statement::Undef(name, _) => {
            writeln!(writer, "{}#undef {}", indent, name)
        }
        Statement::Include(include) => print_include(writer, include, indent_level),
        Statement::Embed(embed) => print_embed(writer, embed, indent_level),
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
/// This function is used for debugging and testing purposes.
pub fn print_program<W: Write>(writer: &mut W, program: &Program) -> std::io::Result<()> {
    for statement in &program.statements {
        print_statement(writer, statement, 0)?;
    }
    Ok(())
}

/// Print the entire program to a string
/// This function is used for debugging and testing purposes.
#[allow(dead_code)]
pub fn print_to_string(program: &Program) -> String {
    let mut output = Vec::new();
    print_program(&mut output, program).unwrap();
    String::from_utf8(output).unwrap()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        TokenWithRange,
        ast::{Branch, Condition, Define, Embed, If, Include, Pragma, Program, Statement},
        ast_printer::{
            print_define, print_embed, print_if, print_include, print_pragma, print_program,
            print_statement, print_to_string,
        },
        range::Range,
        token::{IntegerNumber, IntegerNumberType, Number, Punctuator, StringType, Token},
    };

    #[test]
    fn test_print_pragma() {
        let pragma = Pragma {
            parts: vec![
                TokenWithRange {
                    token: Token::Identifier("STDC".to_string()),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Identifier("FENV_ACCESS".to_string()),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Identifier("ON".to_string()),
                    range: Range::default(),
                },
            ],
        };

        let mut output = Vec::new();
        print_pragma(&mut output, &pragma, 0).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "#pragma STDC FENV_ACCESS ON\n"
        );
    }

    #[test]
    fn test_print_define() {
        // Test for an object-like macro

        let define = Define::ObjectLike {
            identifier: ("MAX".to_string(), Range::default()),
            definition: vec![TokenWithRange {
                token: Token::Number(Number::Integer(IntegerNumber::new(
                    "100".to_string(),
                    false,
                    IntegerNumberType::Default,
                ))),
                range: Range::default(),
            }],
        };

        let mut output = Vec::new();
        print_define(&mut output, &define, 0).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "#define MAX 100\n");

        // Test for an empty object-like macro

        let define_empty = Define::ObjectLike {
            identifier: ("EMPTY".to_string(), Range::default()),
            definition: vec![],
        };

        let mut output_empty = Vec::new();
        print_define(&mut output_empty, &define_empty, 0).unwrap();
        assert_eq!(String::from_utf8(output_empty).unwrap(), "#define EMPTY\n");

        // Test for a function-like macro

        let define_function_like = Define::FunctionLike {
            identifier: ("SQUARE".to_string(), Range::default()),
            parameters: vec!["X".to_string(), "Y".to_string()],
            definition: vec![
                // omit lots of parentheses for simplicity
                TokenWithRange {
                    token: Token::Identifier("X".to_string()),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Punctuator(Punctuator::Multiply),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Identifier("Y".to_string()),
                    range: Range::default(),
                },
            ],
        };

        let mut output_function_like = Vec::new();
        print_define(&mut output_function_like, &define_function_like, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_function_like).unwrap(),
            "#define SQUARE(X, Y) X * Y\n"
        );
    }

    #[test]
    fn test_print_undef() {
        let name = "FOO";
        let range = Range::default();
        let statement = Statement::Undef(name.to_string(), range);

        let mut output = Vec::new();
        print_statement(&mut output, &statement, 0).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "#undef FOO\n");
    }

    #[test]
    fn test_print_include() {
        // Test for including with an identifier
        let include = Include::Identifier("HEADER".to_string(), Range::default());

        let mut output = Vec::new();
        print_include(&mut output, &include, 0).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "#include HEADER\n");

        // Test for including with a system file path

        let include_system = Include::FilePath {
            file_path: ("stdio.h".to_string(), Range::default()),
            is_system_header: true,
        };

        let mut output_system = Vec::new();
        print_include(&mut output_system, &include_system, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_system).unwrap(),
            "#include <stdio.h>\n"
        );

        // Test for including with a local file path

        let include_local = Include::FilePath {
            file_path: ("my_header.h".to_string(), Range::default()),
            is_system_header: false,
        };
        let mut output_local = Vec::new();
        print_include(&mut output_local, &include_local, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_local).unwrap(),
            "#include \"my_header.h\"\n"
        );
    }

    #[test]
    fn test_print_embed() {
        // Test for embedding with an identifier

        let embed = Embed::Identifier("HEMASHUSHU_JPEG_FILE".to_string(), Range::default());
        let mut output = Vec::new();
        print_embed(&mut output, &embed, 0).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "#embed HEMASHUSHU_JPEG_FILE\n"
        );

        // Test for embedding with a system file path
        let embed_system = Embed::FilePath {
            file_path: ("data.bin".to_string(), Range::default()),
            is_system_file: true,
            limit: None,
            suffix: vec![],
            prefix: vec![],
            if_empty: None,
        };
        let mut output_system = Vec::new();
        print_embed(&mut output_system, &embed_system, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_system).unwrap(),
            "#embed <data.bin>\n"
        );

        // Test for embedding with a local file path

        let embed_local = Embed::FilePath {
            file_path: ("hippo.png".to_string(), Range::default()),
            is_system_file: false,
            limit: None,
            suffix: vec![],
            prefix: vec![],
            if_empty: None,
        };
        let mut output_local = Vec::new();
        print_embed(&mut output_local, &embed_local, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_local).unwrap(),
            "#embed \"hippo.png\"\n"
        );

        // Test for embedding with `limit`, `suffix`, `prefix`, and `if_empty` parameters
        let embed_with_params = Embed::FilePath {
            file_path: ("/dev/random".to_string(), Range::default()),
            is_system_file: true,
            limit: Some(100),
            prefix: vec![0x01, 0x02, 0x03],
            suffix: vec![0x07, 0x08, 0x09],
            if_empty: Some(vec![0x11, 0x13, 0x17, 0x19]),
        };
        let mut output_with_params = Vec::new();
        print_embed(&mut output_with_params, &embed_with_params, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_with_params).unwrap(),
            "#embed </dev/random> limit(100) prefix(0x01, 0x02, 0x03) suffix(0x07, 0x08, 0x09) if_empty(0x11, 0x13, 0x17, 0x19)\n"
        );
    }

    #[test]
    fn test_print_if() {
        // Test for a simple `#if` directive:
        // `#if CONDITION ... #endif`
        let if_simple = If {
            branches: vec![Branch {
                condition: Condition::Expression(vec![
                    TokenWithRange {
                        token: Token::Identifier("EDITION".to_string()),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Punctuator(Punctuator::Equal),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Number(Number::Integer(IntegerNumber::new(
                            "2025".to_string(),
                            false,
                            IntegerNumberType::Default,
                        ))),
                        range: Range::default(),
                    },
                ]),
                consequence: vec![Statement::Code(vec![
                    TokenWithRange {
                        token: Token::Identifier("int".to_string()),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Identifier("x".to_string()),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Punctuator(Punctuator::Semicolon),
                        range: Range::default(),
                    },
                ])],
            }],
            alternative: None,
        };

        let mut output_simple = Vec::new();
        print_if(&mut output_simple, &if_simple, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_simple).unwrap(),
            "#if EDITION == 2025\n    int x ;\n#endif\n"
        );

        // Test for `#ifdef` directive with `else`:
        // `#ifdef IDENTIFIER ... #else ... #endif`
        let ifdef_else = If {
            branches: vec![Branch {
                condition: Condition::Defined("IDENTIFIER".to_string(), Range::default()),
                consequence: vec![Statement::Code(vec![
                    TokenWithRange {
                        token: Token::Identifier("int".to_string()),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Identifier("x".to_string()),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Punctuator(Punctuator::Semicolon),
                        range: Range::default(),
                    },
                ])],
            }],
            alternative: Some(vec![Statement::Code(vec![
                TokenWithRange {
                    token: Token::Identifier("int".to_string()),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Identifier("y".to_string()),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Punctuator(Punctuator::Semicolon),
                    range: Range::default(),
                },
            ])]),
        };
        let mut output_ifdef_else = Vec::new();
        print_if(&mut output_ifdef_else, &ifdef_else, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_ifdef_else).unwrap(),
            "#ifdef IDENTIFIER\n    int x ;\n#else\n    int y ;\n#endif\n"
        );

        // Test for multiple branches with `#elif`, `#elifdef`, and `#else`:
        // `#if CONDITION1 ... #elif CONDITION2 ... #elifdef IDENTIFIER ... #else ... #endif`
        let if_multi = If {
            branches: vec![
                Branch {
                    condition: Condition::Expression(vec![
                        TokenWithRange {
                            token: Token::Identifier("defined".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Identifier("ANCC".to_string()),
                            range: Range::default(),
                        },
                    ]),
                    consequence: vec![Statement::Code(vec![
                        TokenWithRange {
                            token: Token::Identifier("a".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Punctuator(Punctuator::Semicolon),
                            range: Range::default(),
                        },
                    ])],
                },
                Branch {
                    condition: Condition::Expression(vec![
                        TokenWithRange {
                            token: Token::Identifier("EDITION".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Punctuator(Punctuator::Equal),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Number(Number::Integer(IntegerNumber::new(
                                "2025".to_string(),
                                false,
                                IntegerNumberType::Default,
                            ))),
                            range: Range::default(),
                        },
                    ]),
                    consequence: vec![Statement::Code(vec![
                        TokenWithRange {
                            token: Token::Identifier("b".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Punctuator(Punctuator::Semicolon),
                            range: Range::default(),
                        },
                    ])],
                },
                Branch {
                    condition: Condition::Defined("FOO".to_string(), Range::default()),
                    consequence: vec![Statement::Code(vec![
                        TokenWithRange {
                            token: Token::Identifier("c".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Punctuator(Punctuator::Semicolon),
                            range: Range::default(),
                        },
                    ])],
                },
            ],
            alternative: Some(vec![Statement::Code(vec![
                TokenWithRange {
                    token: Token::Identifier("d".to_string()),
                    range: Range::default(),
                },
                TokenWithRange {
                    token: Token::Punctuator(Punctuator::Semicolon),
                    range: Range::default(),
                },
            ])]),
        };
        let mut output_if_multi = Vec::new();
        print_if(&mut output_if_multi, &if_multi, 0).unwrap();
        assert_eq!(
            String::from_utf8(output_if_multi).unwrap(),
            "#if defined ANCC\n    a ;\n#elif EDITION == 2025\n    b ;\n#elifdef FOO\n    c ;\n#else\n    d ;\n#endif\n"
        );
    }

    #[test]
    fn test_print_error() {
        let msg = "This is an error message";
        let statement = Statement::Error(msg.to_string(), Range::default());

        let mut output = Vec::new();
        print_statement(&mut output, &statement, 0).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "#error \"This is an error message\"\n"
        );
    }

    #[test]
    fn test_print_warning() {
        let msg = "This is a warning message";
        let statement = Statement::Warning(msg.to_string(), Range::default());

        let mut output = Vec::new();
        print_statement(&mut output, &statement, 0).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "#warning \"This is a warning message\"\n"
        );
    }

    #[test]
    fn test_print_code() {
        let code = vec![
            TokenWithRange {
                token: Token::Identifier("int".to_string()),
                range: Range::default(),
            },
            TokenWithRange {
                token: Token::Identifier("x".to_string()),
                range: Range::default(),
            },
            TokenWithRange {
                token: Token::Punctuator(Punctuator::Semicolon),
                range: Range::default(),
            },
        ];
        let statement = Statement::Code(code);

        let mut output = Vec::new();
        print_statement(&mut output, &statement, 0).unwrap();
        assert_eq!(String::from_utf8(output).unwrap(), "int x ;\n");
    }

    #[test]
    fn test_print_program() {
        let program = Program {
            statements: vec![
                Statement::Pragma(Pragma {
                    parts: vec![
                        TokenWithRange {
                            token: Token::Identifier("STDC".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Identifier("FENV_ACCESS".to_string()),
                            range: Range::default(),
                        },
                        TokenWithRange {
                            token: Token::Identifier("ON".to_string()),
                            range: Range::default(),
                        },
                    ],
                }),
                Statement::Define(Define::ObjectLike {
                    identifier: ("MAX".to_string(), Range::default()),
                    definition: vec![TokenWithRange {
                        token: Token::Number(Number::Integer(IntegerNumber::new(
                            "100".to_string(),
                            false,
                            IntegerNumberType::Default,
                        ))),
                        range: Range::default(),
                    }],
                }),
                Statement::Include(Include::FilePath {
                    file_path: ("stdio.h".to_string(), Range::default()),
                    is_system_header: true,
                }),
                Statement::Code(vec![
                    TokenWithRange {
                        token: Token::Identifier("printf".to_string()),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Punctuator(Punctuator::ParenthesisOpen),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::String("Hello, World!".to_string(), StringType::Default),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Punctuator(Punctuator::ParenthesisClose),
                        range: Range::default(),
                    },
                    TokenWithRange {
                        token: Token::Punctuator(Punctuator::Semicolon),
                        range: Range::default(),
                    },
                ]),
            ],
        };

        let mut output = Vec::new();
        print_program(&mut output, &program).unwrap();
        assert_eq!(
            String::from_utf8(output).unwrap(),
            "#pragma STDC FENV_ACCESS ON\n#define MAX 100\n#include <stdio.h>\nprintf ( \"Hello, World!\" ) ;\n"
        );
    }

    #[test]
    fn test_print_to_string() {
        let program = Program {
            statements: vec![
                Statement::Pragma(Pragma {
                    parts: vec![TokenWithRange {
                        token: Token::Identifier("once".to_string()),
                        range: Range::default(),
                    }],
                }),
                Statement::Define(Define::ObjectLike {
                    identifier: ("MAX".to_string(), Range::default()),
                    definition: vec![TokenWithRange {
                        token: Token::Number(Number::Integer(IntegerNumber::new(
                            "100".to_string(),
                            false,
                            IntegerNumberType::Default,
                        ))),
                        range: Range::default(),
                    }],
                }),
            ],
        };

        let output = print_to_string(&program);
        assert_eq!(output, "#pragma once\n#define MAX 100\n");
    }
}
