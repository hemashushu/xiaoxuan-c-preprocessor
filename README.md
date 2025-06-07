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

## Differences from Other Preprocessors

ANCPP is designed to be a modern, portable, strictly standards-compliant C preprocessor. As a result, certain permissive syntax and features provided by other preprocessors are not supported:

- Omitting arguments in macro invocations is not allowed. For example, `FOO(1,,3)` is invalid.
- Macros cannot be redefined (even with identical definitions) unless they are undefined first. For example, `#define FOO 123` followed by `#define FOO ...` is not allowed.
- In variadic macros, the parameter name must not precede the ellipsis. For example, `#define FOO(args...)` is invalid. Additionally, using `##` before `__VA_ARGS__` is not supported.
- Null directives are disallowed. For example, a line containing only `#` is considered invalid.
- Conditional expressions must evaluate to an integer value. If the expression is a character, string, floating-point number, function-like macro, or undefined macro, the preprocessor will report an error.
- Unsupported pragmas will halt processing rather than being ignored. For example, `#pragma GCC dependency` will result in an error.
- Non-standard directives such as `#assert`, `unassert`, `#ident`, `sccs`, and `#include_next`, as well as the operator `__has_include_next()`, are not supported.
- The rarely used `#line` directive is not supported.
- Trigraphs (e.g., `??<`, `??>`) and digraphs (e.g., `<:`, `:>`, and `%:%:`) are not supported.
