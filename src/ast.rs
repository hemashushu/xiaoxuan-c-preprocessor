// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{range::Range, token::TokenWithRange};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Pragma(Pragma),
    Define(Define),
    Undef(String, Range),
    Include(Include),
    Embed(Embed),
    If(If),
    Error(String),
    Warning(String),
    Code(Vec<TokenWithRange>),
}

// Implementation-defined behavior control
//
// Implementation-defined behavior is controlled by the #pragma directive.
//
// Syntax:
//
//  - `#pragma pragma_params` (1)
//  - `_Pragma ( string-literal )` (2) (since C99)
//
// Standard pragmas:
//
// - `#pragma STDC FENV_ACCESS arg`
// - `#pragma STDC FP_CONTRACT arg`
// - `#pragma STDC CX_LIMITED_RANGE arg`
//
// where `arg` is either ON, OFF, or DEFAULT.
//
// Non-standard pragmas (but supported by ANCPP):
//
// - `#pragma once`
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/impl.html
#[derive(Debug, PartialEq)]
pub struct Pragma {
    pub parts: Vec<TokenWithRange>,
}

// Macro replacement
//
// The preprocessor supports both object-like and function-like macro replacement.
//
// Syntax:
//
// - `#define identifier replacement-list (optional)` (1)
// - `#define identifier ( parameters ) replacement-list` (2)
// - `#define identifier ( parameters, ... ) replacement-list` (3) (since C99)
// - `#define identifier ( ... ) replacement-list` (4) (since C99)
// - `#undef identifier` (5)
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/replace.html
#[derive(Debug, PartialEq)]
pub enum Define {
    ObjectLikeMacro {
        name: String, // The macro name, string "defined" is treated as a "keyword" and cannot be used as a macro name
        definition: Vec<TokenWithRange>, // Can be empty, e.g., `#define FOO`
    },
    FunctionLikeMacro {
        name: String,
        parameters: Vec<String>,
        definition: Vec<TokenWithRange>, // Can be empty, e.g., `#define FOO(x, y)`
    },
}

// Source file inclusion
//
// Includes another source file into the current file at the line immediately following the directive.
//
// Syntax:
//
// - `#include < h-char-sequence > new-line` (1)
// - `#include " q-char-sequence " new-line` (2)
// - `#include pp-tokens new-line` (3)
// - `__has_include ( " q-char-sequence " )`
// - `__has_include ( < h-char-sequence > )` (4) (since C23)
// - `__has_include ( string-literal )`
// - `__has_include ( < h-pp-tokens > )` (5) (since C23)
//
// ANCPP supports `#include MACRO_NAME`, but does not support forms where
// the macro name is enclosed in angle brackets or quotes.
// For example, `#include <MACRO_NAME>` and `#include "MACRO_NAME"` are not supported.
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/include.html
#[derive(Debug, PartialEq)]
pub enum Include {
    Identifier(String, Range),
    FilePath {
        file_path: (String, Range),
        is_system_header: bool,
    },
}

// Binary resource inclusion (since C23)
//
// The #embed directive allows inclusion of binary resources in the build.
// A resource is defined as a source of data accessible from the translation environment.
//
// Syntax:
//
// - `#embed < h-char-sequence > embed-parameter-sequence (optional) new-line` (1)
// - `#embed " q-char-sequence " embed-parameter-sequence (optional) new-line` (2)
// - `#embed pp-tokens new-line` (3)
// - `__has_embed ( " q-char-sequence " embed-parameter-sequence (optional) )`
// - `__has_embed ( < h-char-sequence > embed-parameter-sequence (optional) )` (4)
// - `__has_embed ( string-literal pp-balanced-token-sequence (optional) )`
// - `__has_embed ( < h-pp-tokens > pp-balanced-token-sequence (optional) )` (5)
//
// ANCPP supports `#embed MACRO_NAME`, but does not support forms where
// the macro name is enclosed in angle brackets or quotes.
// For example, `#embed <MACRO_NAME>` and `#embed "MACRO_NAME"` are not supported.
//
// Optional embed parameters can be specified after the required filename argument.
// There are four standard parameters:
// - `limit`: Specifies the maximum number of bytes to include from the resource.
// - `prefix`: A balanced token sequence to prepend to the integer literal sequence, if not empty.
// - `suffix`: A balanced token sequence to append to the integer literal sequence, if not empty.
// - `if_empty`: A balanced token sequence to use as the expansion if the resource is empty.
//
// ANCPP does not support parameter names that are both prefixed and suffixed with two underscores,
// such as `__prefix__`, `__suffix__`, and `__if_empty__`.
//
// ANCPP also does not support other vendor-specific parameters, such as `gnu::offset` and `gnu::base64`.
//
// Note: A "balanced token sequence" refers to a sequence of tokens in which
// parentheses, brackets, and braces are properly balanced.
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/embed.html
#[derive(Debug, PartialEq)]
pub enum Embed {
    Identifier(String, Range),
    FilePath {
        file_path: (String, Range),
        is_system_header: bool,
        limit: Option<usize>,
        suffix: Vec<TokenWithRange>,
        prefix: Vec<TokenWithRange>,
        if_empty: Option<Vec<TokenWithRange>>,
    },
}

// Conditional inclusion
//
// The preprocessor supports conditional compilation of source code sections.
// This is controlled by the #if, #else, #elif, #ifdef, #ifndef, #elifdef,
// #elifndef (since C23), and #endif directives.
//
// Syntax:
//
// - `#if expression`
// - `#ifdef identifier`
// - `#ifndef identifier`
// - `#elif expression`
// - `#elifdef identifier` (since C23)
// - `#elifndef identifier` (since C23)
// - `#else`
// - `#endif`
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/conditional.html
#[derive(Debug, PartialEq)]
pub struct If {
    pub branches: Vec<Branch>,
    pub alternative: Option<Vec<Statement>>,
}

#[derive(Debug, PartialEq)]
pub struct Branch {
    pub condition: Condition,
    pub consequence: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Condition {
    Expression(Vec<TokenWithRange>),
    Defined(String, Range),
    NotDefined(String, Range),
}
