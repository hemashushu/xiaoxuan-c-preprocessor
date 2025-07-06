# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=4 orderedList=false} -->

<!-- code_chunk_output -->

- [Features](#features)
- [Usage](#usage)
- [Get to Know Preprocessing](#get-to-know-preprocessing)
- [Directives](#directives)
- [Limitations](#limitations)

<!-- /code_chunk_output -->

## Features

- Fully adheres to the C23 standard, focusing on modern and portable C code.
- Accurately tracks token positions in the source code, enabling precise and helpful error messages.
- Does not introduce any non-standard directives, extensions, or behaviors.
- Limits some "flex" features to make the preprocessor more predictable and less error-prone (see below for details).

## Usage

ANCPP is not a standalone command-line tool like `gcc -E` or `cpp`. Instead, it is designed as a library that can be easily integrated into your Rust projects. ANCPP takes C source code as input and produces a stream of tokens, which can be consumed directly by compilers or parsers.

To use ANCPP, add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
ancpp = "0.1.0"
```

Alternatively, you can add it using the following command:

```bash
cargo add ancpp
```

You can then use ANCPP in your code as follows:

```rust
// Build the `FileProvider`, an abstract file system interface
// that provides access to user and system header files and binary resources.
//
// Typically, you should specify the paths to your project's header directories
// and system include directories.
let project_root_path = Path::new("/path/to/your/project");
let user_dirs = vec![
    project_root_path.join("include"),
    project_root_path.join("header"),
    project_root_path.join("resources"),
    project_root_path.join("src/common"),
];

let system_include_path = Path::new("/usr/include");
let system_dirs = vec![system_include_path];

let file_provider = NativeFileProvider::new(&user_dirs, &system_dirs);

// Build the predefined macros.
let mut predefinitions = HashMap::new();
predefinitions.insert("__STDC__".to_string(), "1".to_string());
predefinitions.insert("__STDC_VERSION__".to_string(), "202311L".to_string());
predefinitions.insert("__STDC_HOSTED__".to_string(), "1".to_string()); // 1 for hosted environments, 0 for freestanding
predefinitions.insert("__STDC_UTF_16__".to_string(), "1".to_string()); // 1 for UTF-16 support, e.g., `u'文'` and `u"hippo🦛"`
predefinitions.insert("__STDC_UTF_32__".to_string(), "1".to_string()); // 1 for UTF-32 support, e.g., `U'宏'` and `U"spark✨"`

// Build the `HeaderFileCache`, which caches parsed header files
// to speed up preprocessing when handling multiple source files.
let mut file_cache = HeaderFileCache::new();

let should_resolve_relative_path = false; // Set to true to resolve relative paths, such as `#include "../header.h"`.
let source_file_number = FILE_NUMBER_SOURCE_FILE_BEGIN + 1; // Used for error reporting
let source_file_relative_path = Path::new("src/main.c");    // Relative path to the source file, relative to the project root
let source_file_full_path = Path::new("/path/to/your/project/src/main.c"); // Full path to the source file

// Process the source file and get the preprocessed result
let result = process_source_file(
        file_provider,
        file_cache,
        predefinitions,
        should_resolve_relative_path,
        source_file_number,
        source_file_relative_path,
        source_file_full_path,
    )
    .unwrap();

println!("{:?}", result);
```

The function `process_source_file` is the main API provided by ANCPP. It takes a `FileProvider`, a `HeaderFileCache`, a map of predefined macros, and several parameters related to the source file, and returns the preprocessed result as a `Result<PreprocessResult, PreprocessorError>`.

A `PreprocessResult` contains a vector of `TokenWithLocation` (the preprocessed tokens, ready for consumption by compilers) and a vector of `Prompt` (warnings and hints generated during preprocessing).

By default, ANCPP only resolves header and resource files located in the user and system directories specified in the `FileProvider`. For example, if your source file `src/main.c` contains `#include "./header.h"`, ANCPP will search for `header.h` in the `include`, `header`, `resources`, and `src/common` directories under the project root, as well as in the system include directories (e.g., `/usr/include`). It will not search the current directory (`src/`) unless you enable relative path resolution.

Most C compilers, such as GCC and LLVM, resolve relative paths in `#include` directives relative to the directory of the source file being compiled. To match this behavior, set the `should_resolve_relative_path` parameter to `true`.

The `source_file_relative_path` parameter is used to generate the content of the `__FILE__` macro and does not affect file loading. The actual file loading is determined by the `source_file_full_path` parameter.

In a typical C project with multiple source and header files, preprocessing may produce errors or warnings, each associated with a specific source file number (as well as line and column). ANCPP automatically assigns file numbers to header files, starting from 1 (file number 0 is reserved for predefined macros). The source file number should be specified manually using the `source_file_number` parameter. To avoid conflicts with header files, the source file number should be greater than or equal to `FILE_NUMBER_SOURCE_FILE_BEGIN`, which is defined as 65536 in ANCPP.

The loaded header file list can be obtained by calling the `get_cache_file_list` method on the `HeaderFileCache`. This list contains all header files that were successfully loaded during preprocessing, along with their file numbers and paths.

> The following two chapters are excerpted from my book [Building a C23 Compiler from Scratch in 2025](https://leanpub.com/building-a-c23-compiler-from-scratch-in-2025). Click the link to read the full book.

## Get to Know Preprocessing

The C preprocessor uses a simple "language" to process C source code. This language consists of directives and macros, which are interleaved with regular C code.

Each directive appears on a line starting with the `#` character, followed by the directive name and optional parameters. Directives control how the preprocessor handles the source code, such as defining macros, including files, or managing conditional compilation.

For example, the following code contains a conditional compilation which is consisted of `#if`, `#else`, and `#endif` directives. These code will output line `println("Hello!")` if the `EDITION` macro (which acts like a variable) is defined as `2025`; otherwise, it will output line `printf("Hello!\n");`:

```c
#if EDITION == 2025
    println("Hello!")
#else
    printf("Hello!\n");
#endif
```

After preprocessing, all directive lines are removed, macros are expanded, and the resulting code is a standard C source file that can be compiled by a C compiler.

### Directives

ANCPP supports almost all of the C23 standard preprocessor directives, including:

- Macro definition and expansion (`#define`, `#undef`).
- Function-like macros and variadic macros.
- Conditional compilation (`#if`, `#ifdef`, `#ifndef`, `#else`, `#elif`, `#elifdef`, `#elifndef`, `#endif`).
- Source file inclusion (`#include`).
- Binary resource inclusion (`#embed`).
- Error and warning (`#error`, `#warning`).
- Behavior control (`#pragma`).

#### Defining and Undefining Macros

Macro is a text-substitution mechanism that allows you to define reusable constants or code snippets, each macro has a name and an optional value or a piece of code.

Macro is defined using the `#define` directive, e.g.:

```c
#define PI 3.14
```

This statement defines a macro named `PI` with the value `3.14`. Whenever the preprocessor encounters `PI` in the code, it replaces it with `3.14`. Such as:

```c
printf("The value of PI is: %f\n", PI);
```

produces the following code after preprocessing:

```c
printf("The value of PI is: %f\n", 3.14);
```

When you someday want to increase the precision of `PI`, you can simply change the definition, for example, `#define PI 3.1415926535`, and the change will be applied throughout the codebase without needing to modify each occurrence of `PI`.

In this case the macro is something like a global constant, but it does not generate any storage in the compiled program, while a global constant usually generates a read-only data in the `.rodata` section of the compiled binary if it have no optimization applied.
ddd
The most difference between a macro and a global constant is that the definition of a macro can be a piece of code, not just a value, for example:

```c
#define CHECK_RESULT if result != 0 { \
    printf("Error: %d\n", result); \
    return result; \
}
```

> The backslashes (`\`) at the end of each line allow the macro to span multiple lines (since a directive must be on a single line). The preprocessor will remove these backslashes and concatenate the lines into a single line.

The code above defines a macro named `CHECK_RESULT` with a C language `if` structure. This macro can be used in your code like this:

```c
int result = some_function();
CHECK_RESULT
result = another_function();
CHECK_RESULT
```

This will expand to:

```c
int result = some_function();
if result != 0 {
    printf("Error: %d\n", result);
    return result;
}
result = another_function();
if result != 0 {
    printf("Error: %d\n", result);
    return result;
}
```

#### Function-like Macros

What makes macros even more powerful is that they can be defined to take arguments, similar to functions. These are called function-like macros (macros without parameters are called object-like macros). They are defined using the same `#define` directive, but with parameters enclosed in parentheses:

```c
#define BRANCH(code, func_name) \
    case code: { \
        result = handle_##func_name(left, right); \
        break; \
    }
```

> Note that there is no space between the macro name (`BRANCH`) and the opening parenthesis (`(`). This is important because it distinguishes a function-like macro from an object-like macro.

Now let's invoke this macro in a `switch` statement:

```c
switch (opcode) {
    BRANCH(0x0001, add)
    BRANCH(0x0002, sub)
    BRANCH(0x0003, mul)
    BRANCH(0x0004, div)
    default: {
        fprintf(stderr, "Unknown opcode: %d\n", opcode);
        break;
    }
}
```

This will expand to:

```c
switch (opcode) {
    case 0x0001: {
        result = handle_add(left, right);
        break;
    }
    case 0x0002: {
        result = handle_sub(left, right);
        break;
    }
    case 0x0003: {
        result = handle_mul(left, right);
        break;
    }
    case 0x0004: {
        result = handle_div(left, right);
        break;
    }
    default: {
        fprintf(stderr, "Unknown opcode: %d\n", opcode);
        break;
    }
}
```

In this case, the macro is far from being just a simple text-substitution, it is "code-generation"! Function-like macros can generate large amounts of repetitive, regular code based on the parameters, which make our lives easier.

> The operator `##` is used to concatenate tokens. In the example above, `handle_##func_name` will expand to `handle_add`, `handle_sub`, etc., depending on the value of argument `func_name`.

Furthermore, function-like macros can generate any code, even if functions. So why not generate `handle_add`, `handle_sub` functions with macros?

```c
#define HANDLE_FUNC(func_name, operator) \
    int handle_##func_name(int left, int right) { \
        return left operator right; \
    }
```c

Now we can use it to generate the handler functions:

```c
HANDLE_FUNC(add, +)
HANDLE_FUNC(sub, -)
HANDLE_FUNC(mul, *)
HANDLE_FUNC(div, /)
```

This will expand to:

```c
int handle_add(int left, int right) {
    return left + right;
}

int handle_sub(int left, int right) {
    return left - right;
}

int handle_mul(int left, int right) {
    return left * right;
}

int handle_div(int left, int right) {
    return left / right;
}
```

#### When to Use Function-like Macros



Macros are a powerful feature of the C preprocessor, but they should be used judiciously. Here are some guidelines on when to use macros:
- **Constants**: Use object-like macros for constants that are used throughout the codebase, such as mathematical constants or configuration values. This allows you to change the value in one place without modifying every occurrence.
- **Code Generation**: Use function-like macros to generate repetitive code patterns, such as switch statements or function definitions. This can reduce boilerplate code and make the codebase more maintainable.
- **Debugging**: Use macros to generate debug code that can be easily enabled or disabled. For example, you can define a macro that prints debug information only when a certain flag is set:

#define PI 3.14
#define SQUARE(x) ((x) * (x))
#define AREA(r) (PI * SQUARE(r))
printf("The value of PI is: %f\n", PI);
printf("The area of a circle with radius 5 is: %f\n", AREA(5));
```

The text `PI` and `AREA(5)` in the `printf` statements will be replaced with their respective definitions during preprocessing, resulting in the following code:

```c
printf("The value of PI is: %f\n", 3.14);
printf("The area of a circle with radius 5 is: %f\n", (3.14 * ((5) * (5))));
```

It is important to note that the preprocessor does not evaluate expressions or perform type checking; it simply replaces text based on the definitions.


**Operators**

- _Stringizing_ (the `#` operator). Stringizing is used to produce a string literal and can only be applied to function-like macro parameters (includes `__VA_ARGS__`). For example, `#define FOO(x) #x` will expand `FOO(abc)` to `"abc"` and `FOO(123)` to `"123"`.
- _Token concatenation_ (the `##` operator). Token concatenation joins two or more tokens into a single token. For example, `#define FOO(x) x##123` will expand `FOO(bar)` to `bar123`. The tokens are concatenated without any whitespace in between, and the tokens are not expanded before concatenation. Token concatenation can only be used in macro definitions (both object-like and function-like).

**Operators in conditional expressions**

- Arithmetic operators: `+`, `-`, `*`, `/`, `%`
- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical operators: `&&`, `||`
- Bitwise operators: `&`, `|`, `^`, `<<`, `>>`
- Unary operators: `+`, `-`, `!`, `~`

Grouping `(...)`

Other operators

- `defined`, `defined(...)`
- `__has_include(...)`,
- `__has_embed(...)`
- `__has_c_attribute(...)`
- `__VA_OPT__( content )`

TODO

**Built-in Macros**

- `__FILE__`
- `__LINE__`
- `__DATE__`
- `__TIME__`
- `__VA_ARGS__`

- `__STDC_EMBED_NOT_FOUND__`
- `__STDC_EMBED_FOUND__`
- `__STDC_EMBED_EMPTY__`

Other macros such as `__STDC__`, `__STDC_VERSION__`, `__STDC_HOSTED__`, they are provided by the compiler and are not defined by ANCPP.

TODO

## Limitations

The C preprocessor operates as a text-based "find and replace" tool, which makes it extremely "flexible." However, this flexibility can result in code that does not conform to C syntax or disrupts control flow, often due to unintentional mistakes by developers that are difficult to detect. Additionally, the C preprocessor can sometimes behave counterintuitive. For instance, when an expression is passed as an argument to a function-like macro, it may be evaluated multiple times, potentially causing unexpected side effects. To avoid such issues, ANCPP intentionally restricts some of the more "flexible" features.

It is worth noting that ANCPP does not change or add features. On the contrary, ANCPP strictly adheres to the C standard, but restricts or disables certain features that are prone to errors.

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

- Stringizing (`#`) can only be applied to identifiers, numbers, chars, or string literals.
- The result of _Token concatenation_ operator (`##`) must be a valid C identifier: the first token must be an identifier, and the remaining tokens must be either identifiers or integer numbers. For example, `foo##bar` and `sprite##2##b` are valid, while `9##s` and `+##=` are invalid.
- Using _token concatenation_ operator (`##`) before `__VA_ARGS__` is not supported.
- Trigraphs (e.g., `??<`, `??>`) and digraphs (e.g., `<:`, `:>`, and `%:%:`) are not supported.
- Each file is included only once, even if it does not have header guards or `#pragma once`.

These limitations are intended to make object-like macros behave more like C variables, function-like macros behave more like C functions, and conditional expressions within `#if` structures behave like standard C expressions. As a result, ANCPP is more clear, consistent, predictable, intuitive, and less error-prone.
