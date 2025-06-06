# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

Unlike standalone tools such as GNU `cpp` or `gcc -E`, ANCPP is designed to be used as a library. It takes C source code as input and produces a stream of tokens, which can be consumed directly by a parser.

## Features

- Accurately tracks token positions in the source code, enabling precise and helpful error messages.
- Fully adheres to the C23 standard, focusing on modern and portable C code.
- Do not introduce any non-standard directives, extensions or behaviors.

## Directives

ANCPP implements almost all of the C23 standard preprocessor directives, including:

- Macro definition and expansion (`#define`, `#undef`)
- Conditional compilation (`#if`, `#ifdef`, `#ifndef`, `#else`, `#elif`, `#elifdef`, `#elifndef`, `#endif`).
- Source file inclusion (`#include`).
- Binary resource inclusion (`#embed`).
- Error and warning (`#error`, `#warning`).
- Behavior control (`#pragma`).

Some directives are not supported by ANCPP:

- Rarely used or obsolete directives, such as `#line` and `#assert`
- Directives supported by certain compilers but not part of the standard, such as `#ident` and `#include_next` (the operator `__has_include_next` is not supported either).

## Differences from Other Preprocessors

ANCPP is designed to be a modern, strictly standards-compliant C preprocessor. As a result, certain permissive syntax and behaviors accepted by other preprocessors are not supported:

- Empty macro arguments are not allowed. For example, `FOO(1,)` is invalid.
- Null directives are not allowed. For example, a line containing only `#` is considered invalid.
- Macros cannot be redefined (even with identical definitions) unless they are undefined first. For example, `#define FOO 123` followed by `#define FOO ...` is not allowed.
- Conditional expressions must evaluate to an integer value; using a character, string, floating-point number, function-like macro, or undefined macro will result in an error.
- Unsupported pragmas will stop processing rather than being ignored, For example, `#pragma GCC dependency` will result in an error.
- Trigraphs (e.g. `??<`, `??>`) and digraphs (e.g. `<:`, `:>`, and `%:%:`) are not supported.
