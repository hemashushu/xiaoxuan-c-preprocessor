# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

Unlike standalone tools such as GNU `cpp` or `gcc -E`, ANCPP is designed to be used as a library. It takes C source code as input and produces a stream of tokens, which can be consumed directly by a parser.

## Features

- Accurately tracks token positions in the source code, enabling precise and helpful error messages.
- Fully adheres to the C23 standard, ensuring compatibility with modern and portable C code.
- Implements all standard preprocessor features, including:
  - Macro definition and expansion (`#define`, `#undef`).
  - Conditional compilation (`#if`, `#ifdef`, `#ifndef`, `#else`, `#elif`, `#elifdef`, `#elifndef`, `#endif`).
  - File inclusion (`#include`).
  - Error and warning directives (`#error`, `#warning`).
  - New directives for C23 (`#embed`, `#pragma`).
- Pre-validates literals to ensure they conform to C23 syntax.
