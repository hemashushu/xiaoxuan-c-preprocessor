# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

Unlike standalone tools such as GNU `cpp` or `gcc -E`, ANCPP is designed to be used as a library. It takes C source code as input and produces a stream of tokens, which can be consumed directly by a parser.

## Features

- Accurately tracks token positions in the source code, enabling precise and helpful error messages.
- Fully adheres to the C23 standard, focusing on modern and portable C code.
- Does not introduce any non-standard directives, extensions, or behaviors.
- Limits some "flex" features to make the preprocessor more predictable and less error-prone (see below for details).

## Supporting

**Directives**

ANCPP implements almost all of the C23 standard preprocessor directives, including:

- Macro definition and expansion (`#define`, `#undef`).
- Function-like macros and variadic macros.
- Conditional compilation (`#if`, `#ifdef`, `#ifndef`, `#else`, `#elif`, `#elifdef`, `#elifndef`, `#endif`).
- Source file inclusion (`#include`).
- Binary resource inclusion (`#embed`).
- Error and warning (`#error`, `#warning`).
- Behavior control (`#pragma`).

**Operators**

- _Stringizing_ (the `#` operator).
- _Token concatenation_ (the `##` operator).
- `__VA_OPT__( content )`

TODO

**Built-in Macros**

- `__FILE__`
- `__LINE__`
- `__DATE__`
- `__TIME__`
- `__VA_ARGS__`

TODO

## Limitations

ANCPP is designed to be a modern, portable, and strictly standards-compliant C preprocessor. As such, it does not support certain extensions and features found in other preprocessors:

**Macro Definitions**

- A macro cannot be redefined unless it is first undefined. For example, `#define FOO 123` followed by another `#define FOO ...` is not allowed. Some preprocessors permit redefining a macro if the new definition is identical, but ANCPP does not support this behavior.
- Undefining a macro that does not exist is not allowed.
- Macro definitions must have balanced brackets. For example, `#define FOO if (` is invalid.

**Macro Invocations**

- Macro invocation arguments are limited to a single identifier, string (or adjacent string literals), character, or number. Expressions and punctuation are not allowed as arguments. For example, `FOO(1+2, bar())` is invalid.
- Omitting arguments in macro invocations is not permitted. For example, `FOO(1,,3)` is invalid.
- _Token concatenation_ (the `##` operator) must produce a valid C identifier. Specifically, the first token must be an identifier, and the second token must be either an identifier or a number. For example, `foo##bar` and `abc##123` are valid, while `123##abc` and `+##=` are invalid. Additionally, using `##` before `__VA_ARGS__` is not supported.
- Parameter name must not precede the ellipsis in _variadic macros_. For example, `#define FOO(...)` is valid, but `#define FOO(args...)` is invalid.
- Conditional expressions must evaluate to an integer value. If the expression is a character, string, floating-point number, function-like macro, or undefined macro, the preprocessor will report an error.
- In conditional expressions, only integer numbers or macros that expand to integer numbers may be used as operands of arithmetic, logical, bitwise, and comparison operators. For example, `#if 1 + 2` is valid, but `#if FOO == "abc"` is invalid.

**Directives**

- Non-standard directives such as `#assert`, `#unassert`, `#ident`, `#sccs`, and `#include_next`, as well as the operator `__has_include_next()`, are not supported.
- The rarely used `#line` directive is not supported.
- Null directives are not allowed. For example, a line containing only `#` is considered invalid.

**Other Limitations**

- Unsupported pragmas will halt processing rather than being ignored. For example, `#pragma GCC dependency` will result in an error.
- Trigraphs (e.g., `??<`, `??>`) and digraphs (e.g., `<:`, `:>`, and `%:%:`) are not supported.
- Each file is included only once, even if it does not have header guards.

These limitations are intended to make object-like macros behave more like C variables, function-like macros behave more like C functions, and conditional expressions within `#if` structures behave like standard C expressions. As a result, ANCPP is more consistent, predictable, clear, and less error-prone.
