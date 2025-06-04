// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    range::Range,
    token::{Number, TokenWithRange},
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub pragmas: Vec<Pragma>,
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
// Non-standard pragmas:
//
// - `#pragma once`
// - `#pragma pack ...` (ANCCP does not intend to support this pragma)
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/impl.html
#[derive(Debug, PartialEq)]
pub struct Pragma {
    pub names: (String, Range),
    pub arguments: Vec<TokenWithRange>,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Define(Define),
    Undef(String, Range),
    Include(Include),
    Embed(Embed),
    Condition(Condition),
    Error(String),
    Warning(String),
    Code(Vec<TokenWithRange>),
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
    Object {
        name: String,
        code: Vec<TokenWithRange>, // This is optional
    },
    Function {
        name: String,
        parameters: Vec<String>,
        code: Vec<TokenWithRange>,
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
// ANCPP does not intend to support `#include "IDENTIFIER"` or `#include <IDENTIFIER>` syntax.
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/include.html
#[derive(Debug, PartialEq)]
pub enum Include {
    Identifier(String, Range),
    Detail {
        file_path: (String, Range),
        is_system: bool,
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
// ANCPP does not intend to support `#embed "IDENTIFIER"` or `#embed <IDENTIFIER>` syntax.
//
// See also:
// https://en.cppreference.com/w/c/preprocessor/embed.html
#[derive(Debug, PartialEq)]
pub enum Embed {
    Identifier(String, Range),
    Detail {
        file_path: (String, Range),
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
pub enum Condition {
    If(Box<Expression>),
    IfDef(String, Range),  // If the identifier is defined
    IfNDef(String, Range), // If the identifier is not defined
    Elif(Box<Expression>),
    ElifDef(String, Range),  // If the identifier is defined
    ElifNDef(String, Range), // If the identifier is not defined
    Else,
    EndIf,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Number((Number, Range)),
    Identifier((String, Range)),
    Bool(bool),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    FunctionCall(/* function name */ String, Vec<Expression>),
    Group(Vec<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic operators
    Add,      // '+'
    Subtract, // '-'
    Multiply, // '*'
    Divide,   // '/'
    Modulo,   // '%'

    // Relational operators
    Equal,              // '=='
    NotEqual,           // '!='
    GreaterThan,        // '>'
    LessThan,           // '<'
    GreaterThanOrEqual, // '>='
    LessThanOrEqual,    // '<='

    // Logical operators
    And, // '&&'
    Or,  // '||'

    // Bitwise operators
    BitwiseAnd, // '&', also used as the `AddressOf` operator
    BitwiseOr,  // '|'
    BitwiseXor, // '^'
    ShiftLeft,  // '<<'
    ShiftRight, // '>>' (note: C does not have a logical right shift `>>>`)
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Plus,       // '+', does not change the operand's value (included for symmetry with `Minus`)
    Minus,      // '-', negation
    LogicalNot, // '!'
    BitwiseNot, // '~'
}
