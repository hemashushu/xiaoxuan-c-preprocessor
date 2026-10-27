// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

// ANCPP reserves file number 0 for predefined macros.
pub const FILE_NUMBER_PREDEFINED: usize = 0;

// Header file numbers are assigned automatically, starting at 1; 0 is reserved
// for predefined macros. Callers assign main source file numbers through
// `process_source_file::source_file_number`, starting at 65536 to keep them
// separate from header file numbers.
pub const FILE_NUMBER_SOURCE_FILE_BEGIN: usize = 2_usize.pow(16);

// A diagnostic emitted by `#warning`.
// Lint level: Warn
pub const LINT_SUPPRESS_DIRECTIVE_WARN: &str = "directive_warn";

// A header has neither `#pragma once` nor an include guard.
// Lint level: Warn
pub const LINT_SUPPRESS_INCLUDE_GUARD_ABSENT: &str = "include_guard_absent";

// A header uses a traditional include guard rather than `#pragma once`.
// ANCPP recommends the latter, although it is not standardized by C23.
// Lint level: Info
pub const LINT_SUPPRESS_TRADITIONAL_INCLUDE_GUARD: &str = "traditional_include_guard";

// The include guard name does not end with the uppercase header filename,
// with non-alphanumeric characters replaced by underscores (e.g., `FILE_NAME_H`).
// Lint level: Info
pub const LINT_SUPPRESS_UNCONVENTIONAL_INCLUDE_GUARD: &str = "unconventional_include_guard";

// A macro argument contains multiple tokens, which may introduce macro pitfalls.
// Lint level: Info
pub const LINT_SUPPRESS_MACRO_INVOCATION_SINGLE_ARGUMENT_MULTIPLE_TOKENS: &str =
    "macro_invocation_single_argument_multiple_tokens";

// A macro is redefined with an identical replacement, which is redundant and may indicate a mistake.
// Lint level: Info
pub const LINT_SUPPRESS_IDENTICAL_REDEFINITION: &str = "identical_redefinition";

// `#undef` names an undefined macro, which may indicate a typo.
// Lint level: Warn
pub const LINT_SUPPRESS_UNDEF_NONEXISTENT_MACRO: &str = "undef_nonexistent_macro";

// Whether quoted header and resource paths are searched for relative to the
// file containing the directive before searching configured directories.
// This runtime option is not a Cargo feature flag or a filesystem sandbox.
//
// For example, suppose the configured include directories are:
//
// - `./include`
// - `./src/headers`
// - `/usr/include`
//
// By default, `#include "foo.h"` in `src/main.c` searches in this order:
//
// - `./include/foo.h`
// - `./src/headers/foo.h`
// - `/usr/include/foo.h`
//
// When enabled, `src/foo.h` is checked first. GCC and Clang normally use this
// relative-first search for quoted inclusions. Angle-bracket paths still search
// only system directories.
pub const COMPILE_FEATURE_RESOLVE_HEADER_FILE_RELATIVE_TO_CURRENT_FILE: &str =
    "resolve_header_file_relative_to_current_file";

// By default, ANCPP requires each macro invocation argument to be a single token
// (identifier, number, string literal, character literal, or punctuator).
// Enabling this runtime option permits multi-token arguments, including
// expressions and nested macro invocations. This allows conventional C macro
// usage but also pitfalls such as repeated evaluation of side effects.
// Other argument restrictions still apply.
pub const COMPILE_FEATURE_MACRO_INVOCATION_SINGLE_ARGUMENT_MULTIPLE_TOKENS: &str =
    "macro_invocation_single_argument_multiple_tokens";

// Keyword spellings callers can reserve against macro definitions.
// This is ANCPP's selected list, not a complete list of C23 keyword spellings:
// deprecated spellings are omitted, and `_Imaginary` is retained even though
// C23 removed imaginary types. C preprocessing does not generally reserve
// language keywords against macro definitions.
//
// - `_Alignas` (C11)(deprecated in C23) -> `alignas` (C23)
// - `_Alignof` (C11)(deprecated in C23) -> `alignof` (C23)
// - `_Bool` (C99)(deprecated in C23) -> `bool` (C23)
// - `_Static_assert` (C11)(deprecated in C23) -> `static_assert` (C23)
// - `_Thread_local` (C11)(deprecated in C23) -> `thread_local` (C23)
// - `_Noreturn` (C11)(deprecated in C23)
//
// Reference:
// - https://cppreference.com/c/keyword
// - https://en.cppreference.com/w/c/keyword.html
pub const C23_KEYWORD_STRS: [&str; 53] = [
    /* storage class specifier */
    "typedef",
    "extern",
    "static",
    "thread_local",
    "auto",
    "register",
    "constexpr",
    /* type specifier */
    "void",
    "char",
    "short",
    "int",
    "long",
    "float",
    "double",
    "signed",
    "unsigned",
    "bool",
    "_Complex",
    "_Imaginary",
    "_Decimal32",
    "_Decimal64",
    "_Decimal128",
    "_BitInt",
    "typeof",
    "typeof_unqual",
    "_Atomic",
    "struct",
    "union",
    "enum",
    /* type qualifier */
    "const",
    "restrict",
    "volatile",
    /* function specifier */
    "inline",
    /* alignment specifier */
    "alignas",
    /* other keywords */
    "if",
    "else",
    "switch",
    "case",
    "default",
    "break",
    "continue",
    "for",
    "do",
    "while",
    "false",
    "true",
    "goto",
    "return",
    "alignof",
    "sizeof",
    "nullptr",
    "static_assert",
    "_Generic",
];

#[repr(usize)]
pub enum C23Keyword {
    /* storage class specifier */
    Typedef,
    Extern,
    Static,
    ThreadLocal,
    Auto,
    Register,
    Constexpr,
    /* type specifier */
    Void,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    Bool,
    Complex,
    Imaginary,
    Decimal32,
    Decimal64,
    Decimal128,
    BitInt,
    Typeof,
    TypeofUnqual,
    Atomic,
    Struct,
    Union,
    Enum,
    /* type qualifier */
    Const,
    Restrict,
    Volatile,
    /* function specifier */
    Inline,
    /* alignment specifier */
    Alignas,
    /* other keywords */
    If,
    Else,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    For,
    Do,
    While,
    False,
    True,
    Goto,
    Return,
    Alignof,
    Sizeof,
    Nullptr,
    StaticAssert,
    Generic,
}
