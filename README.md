# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

Unlike standalone tools such as GNU `cpp` or `gcc -E`, ANCPP is designed to be used as a library. It takes C source code as input and produces a stream of tokens, which can be consumed directly by a parser.

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=4 orderedList=false} -->

<!-- code_chunk_output -->

- [Features](#features)
- [Supporting](#supporting)
- [Limitations](#limitations)

<!-- /code_chunk_output -->


## Features

- Fully adheres to the C23 standard, focusing on modern and portable C code.
- Accurately tracks token positions in the source code, enabling precise and helpful error messages.
- Does not introduce any non-standard directives, extensions, or behaviors.
- Limits some "flex" features to make the preprocessor more predictable and less error-prone (see below for details).

## Supporting

ANCPP supports almost all of the C23 standard preprocessor directives, operators, and built-in macros.

**Directives**

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

The C preprocessor operates as a text-based "find and replace" tool, which makes it extremely "flexible." However, this flexibility can lead to code that does not conform to C syntax or disrupts control flow, often due to unintentional mistakes by developers that are difficult to detect. Additionally, the C preprocessor can sometimes behave anti-intuitive. For example, when passing an expression as an argument to a function-like macro, the expression may be evaluated multiple times, potentially causing unexpected side effects. To avoid such issues, ANCPP intentionally restricts some of the more "flexible" features.

It is worth noting that ANCPP does not change or add features. On the contrary, ANCPP strictly adheres to the C23 standard, but restricts or disables certain features that are prone to errors.

**Macro Definitions**

- A macro cannot be redefined unless it is first undefined. For example, `#define FOO 123` followed by another `#define FOO ...` is not allowed. Some preprocessors permit redefining a macro if the new definition is identical, but ANCPP does not support this behavior.
- Undefining a macro that does not exist is not allowed.
- Macro definitions must have balanced brackets and quotes. For example, `#define FOO if (i==0` and `#define FOO char* a="bar` is invalid.

**Macro Invocations**

- Macro invocation arguments are limited to a single identifier, string (or adjacent string literals), character, or number. Expressions and punctuation are not allowed as arguments. For example, `FOO(1+2, bar())` is invalid.
- Omitting arguments in macro invocations is not permitted. For example, `FOO(1,,3)` is invalid.
- Parameter name must not precede the ellipsis in _variadic macros_. For example, `#define FOO(...)` is valid, but `#define FOO(args...)` is invalid.

**Condition**

- Conditional expressions must evaluate to an integer value: 0 means false, and any non-zero value means true. If the value is a character, string, floating-point number, function-like macro, or undefined macro, it will result in an error.
- In conditional expressions, only integer literals or macros that expand to integer values may be used as operands for arithmetic, logical, bitwise, or comparison operators. For example, `#if 1 + 2` is valid, but `#if FOO == "abc"` is invalid.

**Directives**

- Non-standard directives such as `#assert`, `#unassert`, `#ident`, `#sccs`, and `#include_next`, as well as the operator `__has_include_next()`, are not supported.
- The value of parameters `prefix`, `suffix`, and `if_empty` of directive `#embed` must be a character or integer number sequence separated by commas. For example, `#embed "file.txt" prefix(0xEF, 0xBB, 0xBF) suffix('\0')` is valid, but `#embed "file.txt" prefix(int foo=)` is invalid.
- The rarely used `#line` directive is not supported.
- Null directives are not allowed. For example, a line containing only `#` is considered invalid.

**Other Limitations**

- Stringizing (#) can only be applied to identifiers, numbers, chars, or string literals.
- The result of _Token concatenation_ operator (`##`) must be a valid C identifier: the first token must be an identifier, and the remaining tokens must be either identifiers or integer numbers. For example, `foo##bar` and `sprite##2##b` are valid, while `9##s` and `+##=` are invalid.
- Using _token concatenation_ operator (`##`) before `__VA_ARGS__` is not supported.
- Unsupported pragmas will halt processing rather than being ignored. For example, `#pragma GCC dependency` will result in an error.
- Trigraphs (e.g., `??<`, `??>`) and digraphs (e.g., `<:`, `:>`, and `%:%:`) are not supported.
- Each file is included only once, even if it does not have header guards or `#pragma once`.

These limitations are intended to make object-like macros behave more like C variables, function-like macros behave more like C functions, and conditional expressions within `#if` structures behave like standard C expressions. As a result, ANCPP is more consistent, predictable, clear, and less error-prone.
