// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{range::Range, token::TokenWithRange};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Define {
        define: Define,
        directive_range: Range,
    },

    Undef {
        identifier: String,
        directive_range: Range,
        identifier_range: Range,
    },

    // Source file inclusion
    //
    // Replaces the directive with the included file's processed tokens.
    //
    // Syntax:
    //
    // - `#include < h-char-sequence > new-line`    (1)
    // - `#include " q-char-sequence " new-line`    (2)
    // - `#include pp-tokens new-line`              (3)
    // - `__has_include ( " q-char-sequence " )`
    // - `__has_include ( < h-char-sequence > )`    (4) (since C23)
    // - `__has_include ( string-literal )`
    // - `__has_include ( < h-pp-tokens > )`        (5) (since C23)
    //
    // See also:
    // https://en.cppreference.com/w/c/preprocessor/include.html
    Include {
        components: Vec<TokenWithRange>,
        directive_range: Range,
    },

    // Binary resource inclusion (since C23)
    //
    // The #embed directive allows inclusion of binary resources in the build.
    // A resource is defined as a source of data accessible from the translation environment.
    //
    // Syntax:
    //
    // - `#embed < h-char-sequence > embed-parameter-sequence (optional) new-line`  (1)
    // - `#embed " q-char-sequence " embed-parameter-sequence (optional) new-line`  (2)
    // - `#embed pp-tokens new-line`                                                (3)
    // - `__has_embed ( " q-char-sequence " embed-parameter-sequence (optional) )`
    // - `__has_embed ( < h-char-sequence > embed-parameter-sequence (optional) )`  (4)
    // - `__has_embed ( string-literal pp-balanced-token-sequence (optional) )`
    // - `__has_embed ( < h-pp-tokens > pp-balanced-token-sequence (optional) )`    (5)
    //
    // Optional embed parameters can be specified after the required filename argument:
    //
    // - `limit`: Specifies the maximum number of bytes to include from the resource.
    // - `prefix`: A token sequence to prepend to the integer literal sequence, if not empty.
    // - `suffix`: A token sequence to append to the integer literal sequence, if not empty.
    // - `if_empty`: A token sequence to use as the expansion if the resource is empty.
    //
    // Standard C23 also permits `__limit__`, `__prefix__`, `__suffix__`, and
    // `__if_empty__`; ANCPP accepts only the unadorned names. ANCPP rejects
    // parameters in `__has_embed`, despite the standard syntax shown above.
    //
    // ANCPP converts comma-separated character/integer literals to bytes and
    // inserts separator commas itself, rather than inserting arbitrary tokens.
    // e.g.,
    //
    // - `#embed "file.txt" prefix(0xEF, 0xBB, 0xBF) suffix('\0')`      // valid,
    // - `#embed "file.txt" prefix(a b c)`                              // invalid.
    //
    // See also:
    // https://en.cppreference.com/w/c/preprocessor/embed.html
    //
    // The components can be parsed into a struct like:
    //
    // ```rust
    // Embed {
    //     file_path: String,
    //     file_path_range: Range,
    //
    //     /* If true, the file path is enclosed in angle brackets `<...>`, */
    //     /* otherwise in double quotes `"..."`. */
    //     is_system_file: bool,
    //
    //     /* Specifies the maximum number of bytes to output from the resource. */
    //     /* This limit does not include the prefix or suffix, and does not limit the number of bytes of `if_empty`. */
    //     limit: Option<usize>,
    //
    //     /* Sequence to append to the output if the resource is not empty. */
    //     suffix: Option<Vec<u8>>,
    //
    //     /* Sequence to prepend to the output if the resource is not empty. */
    //     prefix: Option<Vec<u8>>,
    //
    //     /* Sequence to output if the resource is empty. */
    //     if_empty: Option<Vec<u8>>,
    // }
    // ```
    Embed {
        components: Vec<TokenWithRange>,
        directive_range: Range,
    },

    If(If),
    Error {
        message: String,
        directive_range: Range,
        message_range: Range,
    },
    Warning {
        message: String,
        directive_range: Range,
        message_range: Range,
    },
    Pragma {
        pragma: Pragma,
        directive_range: Range,
    },

    // Regular C code (non-directive)
    Code(Vec<TokenWithRange>),
}

// Macro replacement
//
// The preprocessor supports both object-like and function-like macro replacement.
//
// Syntax:
//
// - `#define identifier replacement-list (optional)`           (1)
// - `#define identifier( parameters ) replacement-list`        (2)
// - `#define identifier( parameters, ... ) replacement-list`   (3) (since C99)
// - `#define identifier( ... ) replacement-list`               (4) (since C99)
// - `#undef identifier`                                        (5)
//
// ANCPP requires the replacement to be a balanced token sequence; standard C
// permits unmatched parentheses, brackets, and braces in replacement lists.
// "Balanced" means that all parentheses, brackets, and braces are properly matched.
// e.g.,
//
// - `#define FOO(x) (x + 1)`       // balanced
// - `#define FOO(x) (x + 1`        // unbalanced
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/replace.html
#[derive(Debug, PartialEq, Clone)]
pub enum Define {
    ObjectLike {
        // Macro name; `defined` cannot be used as a macro name.
        identifier: String,
        identifier_range: Range,

        // Can be empty, e.g., `#define FOO`
        definition: Vec<TokenWithRange>,
    },
    FunctionLike {
        // Macro name; `defined` cannot be used as a macro name.
        identifier: String,
        identifier_range: Range,

        // Parameters; the final entry is `Variadic` for `...`.
        parameters: Vec<FunctionParameter>,

        // Can be empty, e.g., `#define FOO(x, y)`
        definition: Vec<TokenWithRange>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionParameter {
    Identifier(String),
    Variadic,
}

// Conditional compilation
//
// The preprocessor supports conditional compilation of source code sections.
// This is controlled by the `#if`, `#else`, `#elif`, `#ifdef`, `#ifndef`, `#elifdef`,
// `#elifndef`, and `#endif` directives.
//
// Syntax:
//
// - `#if expression`
// - `#ifdef identifier`
// - `#ifndef identifier`
// - `#elif expression`
// - `#elifdef identifier`  (since C23)
// - `#elifndef identifier` (since C23)
// - `#else`
// - `#endif`
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/conditional.html
#[derive(Debug, PartialEq, Clone)]
pub struct If {
    pub branches: Vec<Branch>,
    pub alternative: Option<AlternativeBranch>, // `#else` branch
}

#[derive(Debug, PartialEq, Clone)]
pub struct Branch {
    pub condition: Condition,
    pub consequence: Vec<Statement>,

    // Directive range (`#if`, `#ifdef`, `#ifndef`, `#elif`, `#elifdef`, and `#elifndef`)
    pub directive_range: Range,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Condition {
    // `#if expression`
    // A nonempty expression token sequence.
    Expression(/* expression */ Vec<TokenWithRange>),

    // `#ifdef identifier`
    Defined {
        identifier: String,
        directive_range: Range,
    },

    // `#ifndef identifier`
    NotDefined {
        identifier: String,
        directive_range: Range,
    },
}

// `#else` branch of conditional compilation
#[derive(Debug, PartialEq, Clone)]
pub struct AlternativeBranch {
    pub statements: Vec<Statement>,

    // `#else` directive range
    pub directive_range: Range,
}

// Implementation-defined behavior control
//
// Implementation-defined behavior is controlled by the #pragma directive.
//
// Syntax:
//
//  - `#pragma pragma_params`       (1)
//  - `_Pragma ( string-literal )`  (2) (since C99)
//
// Standard pragmas:
//
// - `#pragma STDC FENV_ACCESS arg`
// - `#pragma STDC FP_CONTRACT arg`
// - `#pragma STDC CX_LIMITED_RANGE arg`
//
// where `arg` is either ON, OFF, or DEFAULT.
//
// ANCPP ignores pragma semantics and does not forward pragmas to the compiler.
// The header-guard linter separately recognizes the non-standard directive:
//
// - `#pragma once`
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/impl.html
#[derive(Debug, PartialEq, Clone)]
pub struct Pragma {
    pub components: Vec<TokenWithRange>,
}
