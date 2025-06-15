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

- _Stringizing_ (the `#` operator). Stringizing is used to produce a string literal and can only be applied to function-like macro parameters (includes `__VA_ARGS__`). For example, `#define FOO(x) #x` will expand `FOO(abc)` to `"abc"` and `FOO(123)` to `"123"`.
- _Token concatenation_ (the `##` operator). Token concatenation joins two or more tokens into a single token. For example, `#define FOO(x) x##123` will expand `FOO(bar)` to `bar123`. The tokens are concatenated without any whitespace in between, and the tokens are not expanded before concatenation. Token concatenation can only be used in macro definitions (both object-like and function-like).
- `__VA_OPT__( content )`

TODO

**Operators in conditional expressions**

- Arithmetic operators: `+`, `-`, `*`, `/`, `%`
- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical operators: `&&`, `||`
- Bitwise operators: `&`, `|`, `^`, `<<`, `>>`
- Unary operators: `+`, `-`, `!`, `~`
- Grouping operator: `(...)`

Other operators

- `defined`, `defined(...)`
- `__has_include(...)`, `__has_embed(...)`

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
- Parameter name must not precede the ellipsis in _variadic macros_. For example, `#define FOO(...)` is valid, but `#define FOO(args...)` is invalid.

**Condition**

- Conditional expressions must evaluate to an integer value: 0 means false, and any non-zero value means true. If the value is a character, string, floating-point number, function-like macro, or undefined macro, it will result in an error.
- In conditional expressions, only integer literals or macros that expand to integer values may be used as operands for arithmetic, logical, bitwise, or comparison operators. For example, `#if 1 + 2` is valid, but `#if FOO == "abc"` is invalid.

**Directives**

- Non-standard directives such as `#assert`, `#unassert`, `#ident`, `#sccs`, and `#include_next`, as well as the operator `__has_include_next()`, are not supported.
- The rarely used `#line` directive is not supported.
- Null directives are not allowed. For example, a line containing only `#` is considered invalid.

**Other Limitations**

- Stringizing (#) can only be applied to identifiers, numbers, chars, or string literals.
- The result of _Token concatenation_ operator (`##`) must be a valid C identifier: the first token must be an identifier, and the remaining tokens must be either identifiers or integer numbers. For example, `foo##bar` and `sprite##2##b` are valid, while `9##s` and `+##=` are invalid.
- Using _token concatenation_ operator (`##`) before `__VA_ARGS__` is not supported.
- Unsupported pragmas will halt processing rather than being ignored. For example, `#pragma GCC dependency` will result in an error.
- Trigraphs (e.g., `??<`, `??>`) and digraphs (e.g., `<:`, `:>`, and `%:%:`) are not supported.
- Each file is included only once, even if it does not have header guards.

These limitations are intended to make object-like macros behave more like C variables, function-like macros behave more like C functions, and conditional expressions within `#if` structures behave like standard C expressions. As a result, ANCPP is more consistent, predictable, clear, and less error-prone.
