# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

Unlike standalone tools such as GNU `cpp` or `gcc -E`, ANCPP is designed to be used as a library. It takes C source code as input and produces a stream of tokens, which can be consumed directly by a parser.

## Features

- Accurately tracks token positions in the source code, enabling precise and helpful error messages.
- Fully adheres to the C23 standard, focusing on modern and portable C code.

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
- Directives supported by certain compilers but not part of the standard, such as `#ident` and `#include_next`
