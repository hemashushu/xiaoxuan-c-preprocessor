# XiaoXuan C Preprocessor

**ANCPP** (XiaoXuan C Preprocessor) is a C preprocessor implemented in Rust. It supports C23 preprocessing features, including macro expansion, conditional compilation, file and resource inclusion, predefined macros, and diagnostics, with the intentional restrictions documented below.

ANCPP is provided primarily as a library for the ANCC project (XiaoXuan C Compiler). It processes C source files and emits a stream of tokens (with locations) that can be consumed by a C AST parser.

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=4 orderedList=false} -->

<!-- code_chunk_output -->

- [1. Features](#1-features)
- [2. Usage](#2-usage)
  - [2.1 Compile features](#21-compile-features)
  - [2.2 Suppress lints](#22-suppress-lints)
- [3. How the preprocessor works](#3-how-the-preprocessor-works)
  - [3.1 Macro definition (`#define`)](#31-macro-definition-define)
    - [3.2 Undefining macros (`#undef`)](#32-undefining-macros-undef)
  - [3.3 Function-like macros](#33-function-like-macros)
  - [3.4 Variadic macros](#34-variadic-macros)
  - [3.5 Predefined Macros](#35-predefined-macros)
  - [3.6 Conditional compilation (`#if`, `#else` and `#endif`)](#36-conditional-compilation-if-else-and-endif)
  - [3.7 Source file inclusion (`#include`)](#37-source-file-inclusion-include)
  - [3.8 Resource file inclusion (`#embed`)](#38-resource-file-inclusion-embed)
  - [3.9 Error and warning (`#error`, `#warning`)](#39-error-and-warning-error-warning)
  - [3.10 Behavior control (`#pragma`)](#310-behavior-control-pragma)
- [4. Unsupported Directives](#4-unsupported-directives)
- [5. Unsupported Features](#5-unsupported-features)
- [6. Token Types](#6-token-types)
  - [6.1 Identifiers](#61-identifiers)
  - [6.2 String and Character Literals](#62-string-and-character-literals)
  - [6.3 Numbers](#63-numbers)
  - [6.4 Punctuators](#64-punctuators)
- [7. Links](#7-links)
- [8. License](#8-license)
- [9. References](#9-references)

<!-- /code_chunk_output -->

## 1. Features

- C23 preprocessing support for modern, portable C projects, with intentional deviations from the standard.
- Most vendor-specific preprocessing extensions are unsupported; known implementation-specific behavior is documented below.
- Restrictions on some standard features to make preprocessing more predictable and help catch mistakes early; some restrictions can be disabled.
- Precise and helpful error messages.

## 2. Usage

To use ANCPP, add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
ancpp = "2.1.0"
```

Alternatively, you can add it using the following command:

```bash
cargo add ancpp
```

The following example preprocesses two C source files:

```rust
use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use ancpp::{
    consts::FILE_NUMBER_SOURCE_FILE_BEGIN,
    context::HeaderFileCache,
    native_file_provider::NativeFileProvider,
    process_source_file,
};

// Build the `FileProvider`, an abstract file system interface
// that provides access to the user and system header files and binary resources.
let project_root_path = Path::new("/path/to/my/c/project");
let user_include_dirs = vec![
    project_root_path.join("include"),
    project_root_path.join("src/header")
];

let system_include_path = Path::new("/usr/include").to_path_buf();
let system_include_dirs = vec![system_include_path];

let file_provider = NativeFileProvider::new(&user_include_dirs, &system_include_dirs);

// Build the `HeaderFileCache`, which caches parsed header files
// to speed up preprocessing when handling multiple source files.
let mut file_cache = HeaderFileCache::new();

// Runtime preprocessing options; these are not Cargo feature flags.
let compile_features = HashMap::<String, bool>::new();

// Add lint identifiers here to suppress specific messages.
let suppress_lints = HashSet::new();

// Add the predefined macros you need
let mut predefinitions = HashMap::new();
predefinitions.insert("__STDC__".to_string(), "1".to_string());
predefinitions.insert("__STDC_VERSION__".to_string(), "202311L".to_string());
// Set `__STDC_HOSTED__` to 1 for hosted implementations, 0 for freestanding ones.
predefinitions.insert("__STDC_HOSTED__".to_string(), "1".to_string());
// Set `__STDC_UTF_16__` to 1 for UTF-16 support, e.g., `u'文'` and `u"hippo🦛"`
predefinitions.insert("__STDC_UTF_16__".to_string(), "1".to_string());
// Set `__STDC_UTF_32__` to 1 for UTF-32 support, e.g., `U'宏'` and `U"spark✨"`
predefinitions.insert("__STDC_UTF_32__".to_string(), "1".to_string());

// Process one source file and get the preprocessed result

// The number of the source file, used for error reporting.
// Start at `FILE_NUMBER_SOURCE_FILE_BEGIN` (65536)
// and increment by 1 for each source file.
//
// Here, "source file" means a C translation unit's main file (e.g., `main.c`),
// not a header file (e.g., `header.h`).
let source_file_number = FILE_NUMBER_SOURCE_FILE_BEGIN;

// The path name of the source file.
// This sets `__FILE__` for the main file; it is not used to load the file.
// It is usually a path relative to the project root.
let source_file_path_name = Path::new("src/main.c");

// The canonical full path of the source file.
// This path is used to load the source file.
let source_file_full_path = Path::new("/path/to/my/c/project/src/main.c");

let result = process_source_file(
        &file_provider,
        &mut file_cache,
        &ancpp::consts::C23_KEYWORD_STRS,
        &compile_features,
        &suppress_lints,
        &predefinitions,
        source_file_number,
        source_file_path_name,
        source_file_full_path,
    )
    .unwrap();

println!("{:?}", result);

// Process another source file
// Reuse the provider, cache, and configuration with a new file number and paths.
let source_file_number = FILE_NUMBER_SOURCE_FILE_BEGIN + 1;
let source_file_path_name = Path::new("src/utils.c");
let source_file_full_path = Path::new("/path/to/my/c/project/src/utils.c");
let result = process_source_file(
        &file_provider,
        &mut file_cache,
        &ancpp::consts::C23_KEYWORD_STRS,
        &compile_features,
        &suppress_lints,
        &predefinitions,
        source_file_number,
        source_file_path_name,
        source_file_full_path,
    ).unwrap();

println!("{:?}", result);
```

`process_source_file` is ANCPP's main API. It borrows a `FileProvider` and a mutable `HeaderFileCache`, and accepts reserved identifiers, preprocessing options, lint suppression settings, predefined macros, and source file metadata. It returns `Result<PreprocessResult, PreprocessFileError>`. A `PreprocessResult` contains a vector of `TokenWithLocation` for a compiler's parser and a vector of `Lint` messages generated during preprocessing.

In a typical C project with multiple source and header files, preprocessing may produce errors or warnings, each associated with a specific source file number (as well as line and column). ANCPP automatically assigns file numbers to header files, starting from 1 (file number 0 is reserved for predefined macros). The source file number should be specified manually using the `source_file_number` parameter. To avoid conflicts with header files, the source file number should be greater than or equal to `FILE_NUMBER_SOURCE_FILE_BEGIN`, which is defined as 65536 in ANCPP.

Call `HeaderFileCache::get_cache_file_list` to inspect the cached headers, including their file numbers and paths. If you reuse the cache, this list also includes headers cached by earlier preprocessing runs.

### 2.1 Compile features

The `compile_features` map controls preprocessing behavior at runtime. These options are not Cargo feature flags. An omitted option is equivalent to `false`.

**`resolve_header_file_relative_to_current_file`**

This flag controls whether to resolve file paths that are relative to the current file.

By default, ANCPP searches for headers and resources only in the user and system directories configured in the `FileProvider`. This makes the search order explicit; it does not provide a filesystem security boundary.

For example, suppose the configured include directories are:

- `./include`
- `./src/headers`
- `/usr/include`

The statement `#include "foo.h"` in the source file `src/main.c` will try to match `foo.h` in the following order:

- `./include/foo.h`
- `./src/headers/foo.h`
- `/usr/include/foo.h`

If this flag is `true`, ANCPP first checks `src/foo.h`, relative to the file containing the directive, before searching the configured directories.

GCC and Clang normally search relative to the file containing a quoted inclusion directive. Set this flag to `true` to enable that search in ANCPP.

**`macro_invocation_single_argument_multiple_tokens`**

By default, ANCPP requires each macro argument to be a single token: an identifier, number, string literal, character literal, or punctuator. Set this flag to `true` to allow multi-token arguments, such as expressions and nested macro invocations. This enables more conventional C macro usage but also permits pitfalls such as repeated evaluation of arguments with side effects. Other restrictions documented below still apply.

### 2.2 Suppress lints

ANCPP's built-in linter reports potential issues and suggestions during preprocessing. Suppress a lint by adding its identifier to the `suppress_lints` set.

- `directive_warn` (warn): A message generated by `#warning`.
- `include_guard_absent` (warn): A header file has neither `#pragma once` nor an include guard.
- `traditional_include_guard` (info): A header file uses a traditional include guard rather than `#pragma once`, which ANCPP recommends. Note that `#pragma once` is not standardized by C23.
- `unconventional_include_guard` (info): The guard name does not end with the uppercase header filename with non-alphanumeric characters replaced by underscores, such as `FILE_NAME_H`. A prefix is allowed.
- `macro_invocation_single_argument_multiple_tokens` (info): A macro invocation argument contains multiple tokens, which may lead to unexpected behaviors.
- `identical_redefinition` (info): A macro is redefined with an identical replacement, which is redundant and may indicate a mistake.
- `undef_nonexistent_macro` (warn): A macro that has not been defined is undefined using `#undef`, which may indicate a mistake.

## 3. How the preprocessor works

Preprocessing is a translation phase before C syntax and semantics are analyzed. **Preprocessing directives** control macro definitions, conditional compilation, and file inclusion. A directive starts with `#`, possibly preceded by whitespace, and ends at the next newline after backslash-newline splicing. For example:

```c
#include <stdio.h>
#define PI 3.14159

int main() {
    #ifdef FP
    float r = 1.0f;
        float c = 2 * PI * r;
        printf("Circumference: %f\n", c);
        return 0;
    #else
        printf("Floating point not supported.\n");
        return 1;
    #endif
}
```

The preprocessor interprets directives and processes the remaining source as tokens rather than C statements. Conceptually, the example above has this structure:

```c
#include <stdio.h>
#define PI 3.14159
"..."
#ifdef FP
    "..."
#else
    "..."
#endif
"..."
```

Directives control which tokens appear in the output, and macro invocations are expanded. If `FP` is undefined, the result is equivalent to the following code, with the contents of `stdio.h` omitted:

```c
// ... declarations from stdio.h ...
int main() {
    printf("Floating point not supported.\n");
    return 1;
}
```

ANCPP returns the preprocessed tokens rather than generating a C source file. The examples below show equivalent source code for readability.

> Macro expansion substitutes preprocessing tokens, not arbitrary substrings: it does not replace text inside string literals or parts of identifiers. Because it does not enforce C syntax or semantics, expansion can produce invalid code or unintended control flow. ANCPP intentionally restricts some features to help catch mistakes. These deviations from standard C are marked `[ANCPP RESTRICTION]` below.

### 3.1 Macro definition (`#define`)

Syntax:

`#define IDENTIFIER REPLACEMENT`

`#define` defines an object-like macro. Subsequent occurrences of `IDENTIFIER` are expanded to the replacement tokens where macro expansion applies.

Examples:

```c
#define PI 3.14159
float c = 2 * PI * r;

#define MESSAGE "Hello, World!\n"
puts(MESSAGE);
```

The example expands to:

```c
float c = 2 * 3.14159 * r;
puts("Hello, World!\n");
```

> A macro is not a typed C object. Its replacement tokens are substituted during preprocessing, before the compiler analyzes the resulting C code.

The replacement can contain a value, an expression, statements, or an initializer:

```c
#define SAY_HELLO puts("Hello, World!\n");
SAY_HELLO                       // Expands to: `puts("Hello, World!\n");`

#define DATA {1, 2, 3, 4, 5}
int arr[] = DATA;               // Expands to: `int arr[] = {1, 2, 3, 4, 5};`
```

To continue a definition on the next physical line, place a backslash `\` immediately before the newline:

```c
#define ASSERT_ARG if (arg == 0) { \
        puts("Assertion failed.\n"); \
        exit(1); \
    }

int myfunc(int arg) {
    ASSERT_ARG                      // Expands to the if statement above.
    // ... rest of the function
}
```

The replacement can also contain other macros, which will be expanded.

```c
#define FOO 42
#define BAR (FOO) + 1
#define BUZZ (BAR) * 2
int x = BUZZ;                   // Expands to: `int x = ((42) + 1) * 2;`
```

`[ANCPP RESTRICTION]`: Macro replacements must have balanced parentheses, brackets, and braces, and properly terminated string and character literals.

```c
#define A if (i==0              // Disabled: Unbalanced parentheses.
#define B if (i==0) {           // Disabled: Balanced parentheses but unbalanced braces.
#define D int nums[             // Disabled: Unbalanced brackets.
#define C int nums[] = {        // Disabled: Balanced brackets but unbalanced braces.
#define E do {                  // Disabled: Unbalanced braces.
#define F } else {              // Disabled: Unbalanced braces.
#define G char* s="string       // Disabled: Unbalanced quotes.
#define H char c='a             // Disabled: Unbalanced quotes.
```

Standard C allows unmatched parentheses, brackets, and braces in replacement lists, which can be completed by surrounding code. ANCPP rejects them to catch potentially unintended syntax or control flow. Unterminated string and character literals are already invalid in standard C.

> Prefer typed C facilities where possible. C23 `constexpr` objects and enumeration constants can replace many constant macros. A `const` object is read-only but is not, by itself, an integer constant expression in C.

**Empty replacement**

The replacement can also be empty:

```c
#define FOO
puts("Hello" FOO "World!\n");   // Expands to: `puts("Hello" "World!\n");`
```

Empty macros are often used as feature flags for conditional compilation.

**Redefinition of macros**

ANCPP rejects a macro redefinition that changes its definition:

```c
#define VERSION 1
#define VERSION 2               // Error: Conflicting macro redefinition.
```

`[ANCPP RESTRICTION]`: Standard C requires a diagnostic for a conflicting macro redefinition. Some implementations warn and continue; ANCPP reports an error.

To redefine an existing macro, you must first undefine it using the `#undef` directive (see the next section).

```c
#define VERSION 1
// ... some code that uses VERSION
#undef VERSION
#define VERSION 2
// Now VERSION expands to 2
```

You can redefine a macro if the new replacement is identical, but a lint message will still be generated to indicate the redundant redefinition.

```c
#define BUZZ 789
#define BUZZ 789                // Info: Identical Redefinition
```

**Reserved identifiers**

You cannot define a macro named `defined`, which is a special operator in conditional expressions (described below).

```c
#define defined 123             // Error: "defined" is a reserved identifier.
```

The `reserved_identifiers` argument of `process_source_file` is a slice of identifiers that ANCPP prevents users from defining as macros.

The example above passes `ancpp::consts::C23_KEYWORD_STRS` to reserve the keywords listed by ANCPP. This is a caller-selected restriction, not a general C preprocessing rule.

**Macros that haven't been defined**

An identifier that is not defined as a macro remains unchanged in ordinary source code. If you intended it to be a macro, this may cause a compilation error:

```c
puts("Hello" BUZZ "World!\n");  // Expands to: `puts("Hello" BUZZ "World!\n");`
                                // Which causes compilation error because of invalid syntax.
```

The preprocessor cannot distinguish an undefined macro name from an ordinary C identifier. Uppercase macro names are a common convention that helps readers distinguish them from other identifiers.

**Self-referential macros**

A self-referential macro contains its own name in its replacement. It does **not** expand recursively without limit: while a macro's replacement is being rescanned, that macro is disabled for expansion. A self-reference encountered during that rescan remains unexpanded.

```c
#define FOO (4 + FOO)
int x = FOO;                    // Expands to: `int x = (4 + FOO);`.
                                // Not `(4 + (4 + (4 + ... )))`.

#define EACCES EACCES
int err = EACCES;               // Expands to: `int err = EACCES;`.
                                // No infinite recursion.
```

This rule also applies to indirect self-reference (cross-reference) between multiple macros.

```c
#define FOO (BAR + 1)
#define BAR (FOO + 1)

int x = FOO;                    // Expands to: `int x = ((FOO + 1) + 1);`.
int y = BAR;                    // Expands to: `int y = ((BAR + 1) + 1);`.
```

### 3.2 Undefining macros (`#undef`)

Syntax:

`#undef IDENTIFIER`

`#undef` removes a macro definition, leaving the identifier available for ordinary use or a new definition.

For example, undefine a macro before giving it a different replacement:

```c
#define VERSION 1
// ... some code that uses VERSION
#undef VERSION
#define VERSION 2
// Now VERSION expands to 2
```

If the identifier does not have an associated macro, the processor will generate a warning message.

```c
#undef FOO                      // Warning: Macro "FOO" has not been defined.
```

Standard C allows `#undef` on an undefined identifier with no effect. ANCPP additionally emits a suppressible warning to help catch typos.

### 3.3 Function-like macros

The macros above are called object-like macros because they resemble data objects, and the preprocessor simply replaces occurrences of the macro identifier with its replacement.

Function-like macros accept arguments. On invocation, the preprocessor substitutes the arguments for parameters in the replacement list, then rescans the result for further macro expansion.

Syntax:

```c
// Define a function-like macro
#define IDENTIFIER(PARAMETERS) REPLACEMENT
// Invoke the function-like macro
IDENTIFIER(ARGUMENTS)
```

Examples:

```c
#define CHECK_RESULT(x) if ((x) != 0) { \
        puts("Error occurred.\n"); \
        exit(1); \
    }

CHECK_RESULT(foo);
```

After preprocessing, you get:

```c
if ((foo) != 0) {
    puts("Error occurred.\n");
    exit(1);
};
```

> Function-like macros act as code templates and can reduce repetition, but they do not have the type checking or argument evaluation rules of C functions.

Parenthesize each parameter in an expression macro and the entire replacement expression to preserve operator precedence. For example:

```c
#define ADD(a, b) a + b         // Pitfall: Missing parentheses can change precedence.
int x = ADD(1, 2) * 3;          // Expands to: `int x = 1 + 2 * 3;` which is evaluated as `1 + (2 * 3) = 7`, not `(1 + 2) * 3 = 9`
```

Wrap a statement macro in `do { ... } while (0)` so that an invocation followed by a semicolon behaves as a single statement.

Let's look at an example that demonstrates the issue:

```c
#define CHECK_RESULT(x) if ((x) != 0) { \
        puts("Error occurred.\n"); \
        exit(1); \
    }

if (is_cli)
    CHECK_RESULT(pass);
else run();

// The above expands to:
//
// if (is_cli)
//     if ((pass) != 0) {
//         puts("Error occurred.\n");
//         exit(1);
//     };
// else run();
//
// Invalid `else` because there is no matching `if`.
```

An improved version:

```c
#define CHECK_RESULT(x) do { \
    if ((x) != 0) { \
            puts("Error occurred.\n"); \
            exit(1); \
        } \
    } while (0)

if (is_cli)
    CHECK_RESULT(pass);
else run();

// The above expands to:
//
// if (is_cli)
//     do {
//         if ((pass) != 0) {
//             puts("Error occurred.\n");
//             exit(1);
//         }
//     } while (0);
// else run();
//
// Now the `else` matches the outer `if`.

// It is good practice to wrap the `if...else...` statement in braces as well:
//
// if (is_cli) {
//     CHECK_RESULT(pass);
// } else {
//     run();
// }
```

`[ANCPP RESTRICTION]`: Function-like macro replacements must also have balanced parentheses, brackets, and braces, and properly terminated literals.

**Space between macro name and opening parenthesis**

In a function-like macro definition, there must be no whitespace between the macro name and `(`. Otherwise, the definition creates an object-like macro:

```c
#define ADD (a, b) (a) + (b)    // It just defines an object-like macro named "ADD" because of the space.
int sum = ADD(3, 4);            // Expands to: `int sum = (a, b) (a) + (b) (3, 4);` which is invalid syntax.
```

Whitespace between the name and `(` is allowed in an invocation:

```c
#define INC(x) (x) + 1          // Ok
int i = INC (5);                // Expands to: `int i = (5) + 1;`
```

**Redefinition of function-like macros**

To change a function-like macro's definition in ANCPP, first undefine it with `#undef`.

```c
#define ADD(x, y) (x) + (y)
#define ADD(x) (x) + 1          // Error: Conflicting macro redefinition.

#undef ADD
#define ADD(x) (x) + 1          // Now it is ok
```

ANCPP permits an equivalent function-like macro redefinition, even with renamed parameters, but emits a lint message. Standard C's rules for identical redefinitions require matching parameter names.

```c
#define ADD(x, y) (x) + (y)
#define ADD(left, right) (left) + (right)   // Info: Identical Redefinition
```

**Invocation**

Invoke a function-like macro by writing its name followed by arguments in parentheses. Arguments may span multiple lines. For a non-variadic macro, the argument count must match the parameter count:

```c
#define MULTIPLY(a, b) (a) * (b)
int p = MULTIPLY(2, 3);         // Expands to: `int p = (2) * (3);`
int q = MULTIPLY(4);            // Error: Missing argument.
int r = MULTIPLY(5, 6, 7);      // Error: Too many arguments.
```

Parentheses following an object-like macro are not treated as macro arguments:

```c
#define FOO 42
int x = FOO(11);                // Expands to: `int x = 42(11);`
```

A function-like macro name without a following `(` remains unexpanded:

```c
#define INC(x) (x) + 1;
int y = INC 42;                 // Expands to: `int y = INC 42;`
```

`[ANCPP RESTRICTION]`: Arguments for named parameters cannot be empty. A macro with no parameters can still be invoked as `MACRO()`.

```c
#define SUBTRACT(a, b) (a) - (b)
int d = SUBTRACT(10, );         // Disabled: The second argument is empty.
int e = SUBTRACT(, 5);          // Disabled: The first argument is empty.
int f = SUBTRACT(, );           // Disabled: Both arguments are empty.
int g = SUBTRACT((,),)          // Disabled: The first argument is `(,)`, but the second argument is empty.
```

Standard C permits empty arguments, which contribute no tokens when substituted. The resulting C code may be invalid, so ANCPP rejects empty arguments for named parameters.

`[ANCPP RESTRICTION]`: By default, each macro argument must be a single token. Multi-token expressions and nested macro invocations are rejected; a single punctuator is allowed.

Standard C permits arguments containing multiple preprocessing tokens. Commas separate arguments unless enclosed in nested parentheses; brackets and braces do not protect commas. Preprocessing directives within macro arguments have undefined behavior. Multi-token arguments also permit pitfalls involving side effects:

```c
#define DOUBLE(x) (x) * (x)

int i = 2;
int j = DOUBLE(i++);                    // Disallowed: Argument "i++" is an expression.

// With multi-token arguments enabled, this expands to `int j = (i++) * (i++);`.
// The unsequenced modifications of i cause undefined behavior in C.

int next_random_number() { /* ... */ }
int k = DOUBLE(next_random_number());   // Disallowed: Argument "next_random_number()" is an expression.

// The invocation above expands to: `int k = (next_random_number()) * (next_random_number());`, which causes unexpected behavior due to multiple function calls.

#define MIN(x, y) ((x) < (y) ? (x) : (y))

int min_value = MIN(MIN(a,b), c);       // Disallowed: Argument "MIN(a,b)" is an expression.

// The invocation above expands to:
//
// ```c
// int min_value =
// (
//   (((a) < (b) ? (a) : (b))) < (c) ?
//   (((a) < (b) ? (a) : (b))) : (c)
// );
```

The single-token restriction reduces these risks but does not guarantee that macro expansion is safe or that the resulting C code is valid.

To allow multi-token arguments, set `"macro_invocation_single_argument_multiple_tokens"` to `true` in the `compile_features` map passed to `process_source_file`.

> Prefer C functions, including inline functions where appropriate, when they can express the same operation. Reserve function-like macros for cases that need preprocessing, such as token construction or conditional code generation.

**Expansion of function-like macros**

Macro arguments are token sequences, not evaluated C expressions. The preprocessor expands eligible macros in the arguments, substitutes the resulting tokens, and rescans the replacement. Arguments used with `#` or `##` are exceptions, described below.

```c
#define ADD(a, b) (a) + (b)
int left = 1;
int x = ADD(left, 11);          // Expands to: `int x = (left) + (11);`

// The steps of expansion of `ADD(left, 11)` are:
//
// 1. Argument `left` is an identifier, so it remains unchanged.
// 2. Argument `11` is a number, so it remains unchanged as well.
// 3. Invoke `ADD(...)`.
// 4. Substitute the arguments to get `(left) + (11)`.
```

Macros in arguments are normally **expanded before substitution**:

```c
#define ADD(a, b) (a) + (b)
int left = 1;
#define RIGHT 2
int x = ADD(left, RIGHT);       // Expands to: `int x = (left) + (2);`

// The steps of expansion of `ADD(left, RIGHT)` are:
//
// 1. Argument `left` is not a macro, so it remains unchanged.
// 2. Argument `RIGHT` is a macro, so it is expanded to `2`.
// 3. Invoke `ADD(...)`.
// 4. Substitute the arguments to get `(left) + (2)`.
```

Arguments are identified **before** they are expanded. Commas introduced by that expansion do not change the argument count of the current invocation:

```c
#define PRINT(a, b) a; b

PRINT("hello", "world");    // Expands to: `"hello"; "world";`

#define FOOBAR "foo", "bar"
PRINT(FOOBAR, "buzz");      // Expands to: `"foo", "bar"; "buzz";`

// `PRINT(FOOBAR, "buzz")` has two arguments, not three.
```

Argument expansion also applies to nested macro invocations. This example requires `macro_invocation_single_argument_multiple_tokens` to be enabled:

```c
#define INC(a) (a) + 1
int x = INC(INC(3));            // Expands to: `int x = ((3) + 1) + 1;`

// The steps of expansion of `INC(INC(3))` are:
//
// 1. The argument `INC(3)` is a macro invocation, so it is expanded first.
// 2. Substitute `3` to get `(3) + 1`.
// 3. Invoke the outer `INC(...)`.
// 4. Substitute the expanded argument to get `((3) + 1) + 1`.
```

After substitution, the result is **rescanned** for further macro expansion:

```c
#define OPERATOR +
#define ADD(a, b) (a) OPERATOR (b)

int x = ADD(1, 2);              // Expands to: `int x = (1) + (2);`

// The steps of expansion of `ADD(1, 2)` are:
//
// 1. Arguments `1` and `2` are not macros, so they remain unchanged.
// 2. Substitute the arguments to get `(1) OPERATOR (2)`.
// 3. Rescan the result and expand `OPERATOR` to `+`.
// 4. The final expansion is `(1) + (2)`.
```

Function-like macro invocations produced by substitution are also expanded:

```c
#define GREETING(msg) puts(msg)
#define INVOKE(func_name, argument) func_name(argument)

INVOKE(GREETING, "Hello, World!\n");    // Expands to: `puts("Hello, World!\n");`

// The steps of expansion of `INVOKE(GREETING, "Hello, World!\n")` are:
//
// 1. Argument `GREETING` is an identifier (it's not an invocation), so it remains unchanged.
// 2. Argument `"Hello, World!\n"` is a string literal, so it remains unchanged as well.
// 3. Invoke `INVOKE(...)`.
// 4. Substitute the arguments to get `GREETING("Hello, World!\n")`.
// 5. Scan the result for macros and find `GREETING`.
// 6. Expand `GREETING("Hello, World!\n")`.
// 7. Substitute the argument to get `puts("Hello, World!\n")`.
// The semicolon comes from the original invocation.
```

Parameter substitution applies only to tokens in the current macro's replacement list. A matching identifier introduced later by another macro's expansion is not substituted retroactively:

```c
#define WRITE puts(msg);
#define LOG(msg) puts("log:"); WRITE

LOG("Hello")                    // Expands to: `puts("log:"); puts(msg);`

// The steps of expansion of `LOG("Hello")` are:
//
// 1. Argument `"Hello"` is a string literal, so it remains unchanged.
// 2. Invoke `LOG(...)`.
// 3. Substitute the argument to get `puts("log:"); WRITE`.
// 4. Scan the result for macros and find `WRITE`.
// 5. Expand `WRITE`
// 6. The result is `puts("log:"); puts(msg);`.
// 7. `msg` was introduced by `WRITE`, after parameter substitution for `LOG`.
```

With multi-token arguments enabled, commas introduced by one expansion can change the argument count of a subsequent invocation during rescanning:

```c
#define WRITE(msg) puts(msg);
#define SURROUND(text) "[", text, "]"

#define LOG(s) WRITE("log:" s)

LOG(SURROUND("Hello"))          // Error: Mismatched arguments.

// The steps of expansion of `LOG(SURROUND("Hello"))` are:
//
// 1. There is only one argument `SURROUND("Hello")` for `LOG(...)`.
// 2. `LOG(s)` accepts one argument, so it is valid so far.
// 3. Argument `SURROUND("Hello")` is a macro invocation, so it is expanded first.
// 4. Invoke `SURROUND(...)`.
// 5. Substitute "Hello" to get `"[", "Hello", "]"`.
// 6. Substitute that result into `LOG` to get `WRITE("log:" "[", "Hello", "]")`.
// 7. Rescan the result and find `WRITE`.
// 8. Error: `WRITE` expects one argument, but three are provided.
```

With the default single-token restriction, ANCPP rejects the nested invocation before reaching this argument-count mismatch.

**Self-referential function-like macros**

Function-like macros follow the same self-reference suppression rules as object-like macros:

```c
#define DEC(x) DEC(x - 1)
int n = DEC(5);                 // Expands to: `int n = DEC(5 - 1);`.

#define WRITE(x) ">" FMT(x)
#define FMT(y) WRITE(y) "!\n"

WRITE("Hello")                  // Expands to: `">" WRITE("Hello") "!\n";`
```

**Stringizing (the `#` operator)**

In a function-like macro's replacement list, placing `#` before a parameter name (spaces between `#` and the parameter are allowed) converts the argument's source spelling into a C string literal.

ANCPP stringizes its parsed tokens, separating them with spaces and normalizing literal spellings. This can differ from standard C stringizing, which preserves token spelling and normalizes only the argument's existing whitespace.

```c
#define DEBUG(var) printf("%s=%d\n", #var, var)
int foo = 42;
DEBUG(foo);                     // Expands to: `printf("%s=%d\n", "foo", foo);`
```

Identifiers, numbers, character literals, and string literals can all be stringized. Quotes and backslashes within string and character literals are escaped as needed:

```c
#define SHOW(x) puts(#x);
SHOW(42)                        // Expands to: `puts("42");`
SHOW('c')                       // Expands to: `puts("'c'");`
SHOW("Hello")                   // Expands to: `puts("\"Hello\"");`
```

Whitespace is allowed between `#` and the parameter name, so `puts(#x)` can also be written as `puts(# x)`.

By default, the single-token argument restriction also applies to stringizing. Enable `macro_invocation_single_argument_multiple_tokens` to stringize multi-token arguments without expanding them:

```c
#define FOO(x) #x
FOO(a + b);                     // With multi-token arguments enabled: `"a + b";`
FOO(foo());                     // With multi-token arguments enabled: `"foo ( )";`
```

An argument used with `#` is stringized **without prior macro expansion**. If the argument is a macro name, the name itself is used:

```c
#define SHOW(x) puts(#x);

#define FOO 42
SHOW(FOO)                       // Expands to: `puts("FOO");`, not `puts("42");`
```

If you want to stringize the expanded value of a macro argument, you need to use an additional level of indirection.

```c
#define SHOW_MACRO_VAL(x) SHOW(x)
#define SHOW(x) puts(#x);

#define FOO 42
SHOW_MACRO_VAL(FOO)             // Expands to: `puts("42");`

// The steps of expansion of `SHOW_MACRO_VAL(FOO)` are:
//
// 1. In `SHOW_MACRO_VAL`, x is not used with # or ##, so FOO expands to 42.
// 2. Substitute 42 to get `SHOW(42)`.
// 3. Rescan the result and expand `SHOW`.
// 4. In `SHOW`, x is used with #, so 42 is stringized without further expansion.
// 5. The final expansion is `puts("42");`.
```

Stringizing results:

| Argument      | Stringize Result |
|---------------|------------------|
| `identifier`  | `"identifier"`   |
| `number`      | `"number"`       |
| `'character'` | `"'character'"`  |
| `"string"`    | `"\"string\""`   |
| `MACRO_NAME`  | `"MACRO_NAME"`   |

Stringizing may only be applied to parameters of function-like macros.

**Token concatenation (the `##` operator)**

Token concatenation, also called token pasting, joins tokens without intervening whitespace. In standard C, the result must be a valid preprocessing token.

```c
#define ALT(x) x##2
ALT(foo)                        // Expands to: `foo2`

#define VAR(family, member, index) family ## member ## index
VAR(bar_, a, 1)                 // Expands to: `bar_a1`
VAR(buz_, b, 2)                 // Expands to: `buz_b2`
```

Spaces around the `##` operator are allowed, so `x##2` above can also be written as `x ## 2`, and `family ## member ## index` can also be written as `family##member##index`.

An argument adjacent to `##` is substituted without prior macro expansion:

```c
#define CONCAT(a, b) a##b
#define PREFIX foo
CONCAT(PREFIX, 1)               // Expands to: `PREFIX1`, not `foo1`
```

The pasted token is then eligible for expansion during rescanning:

```c
#define FOOBAR "hello"
#define CONCAT(a, b) a##b
CONCAT(FOO, BAR);               // Expands to: `"hello"`
```

`[ANCPP RESTRICTION]`: The pasted result must be an ASCII identifier: it must start with a letter or underscore and contain only letters, digits, and underscores. ANCPP cannot paste tokens to form a number or punctuator, even where standard C permits it.

```c
#define FOO(x) x##2
FOO(1);                         // Disabled: The result "12" is a number, not an identifier.
FOO(+);                         // Disabled: The result "+2" is not a single valid token.
```

### 3.4 Variadic macros

Variadic macros are function-like macros that accept a variable number of arguments. They are defined using an ellipsis `...` in the parameter list.

Syntax:

```c
#define IDENTIFIER(PARAMETERS, ...) REPLACEMENT
```

In the replacement list, `__VA_ARGS__` stands for the variable arguments, including the commas separating them. A macro can also have only a variadic parameter, as in `#define NAME(...) REPLACEMENT`.

Examples:

```c
#define LOG(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
LOG("Value: %d\n", 42);         // Expands to: `fprintf(stderr, "Value: %d\n", 42);`
LOG("Hello, World!\n");        // Expands to: `fprintf(stderr, "Hello, World!\n", );` (invalid C).

#define STR(...) char str[] = { __VA_ARGS__ , '\0'}
STR('f', 'o', 'o');             // Expands to: `char str[] = { 'f', 'o', 'o', '\0'};`
STR();                         // Expands to: `char str[] = { , '\0'};` (invalid C).

#define VEC(type, name, ...) type name[] = { __VA_ARGS__ }
VEC(int, numbers, 1, 2, 3);     // Expands to: `int numbers[] = { 1, 2, 3 };`
VEC(char, letters);             // Expands to: `char letters[] = {  };`
```

Use `__VA_OPT__(...)` to emit its contents only when the variable arguments contain tokens. This avoids stray commas when the variable arguments are empty:

```c
#define LOG(fmt, ...) fprintf(stderr, fmt __VA_OPT__(,) __VA_ARGS__)
LOG("Value: %d\n", 42);         // Expands to: `fprintf(stderr, "Value: %d\n", 42);`
LOG("Hello, World!\n");         // Expands to: `fprintf(stderr, "Hello, World!\n" );`

#define STR(...) char str[] = { __VA_ARGS__ __VA_OPT__(,) '\0'}
STR('f', 'o', 'o');            // Expands to: `char str[] = { 'f', 'o', 'o', '\0'};`
STR();                         // Expands to: `char str[] = { '\0'};`

#define VEC(type, name, ...) type name[] = { __VA_ARGS__ __VA_OPT__(,) 0 }
VEC(int, numbers, 1, 2, 3);     // Expands to: `int numbers[] = { 1, 2, 3, 0 };`
VEC(char, letters);             // Expands to: `char letters[] = { 0 };`
```

`__VA_ARGS__` and `__VA_OPT__(...)` may appear only in the replacement list of a variadic macro. Unlike named parameters, the variadic portion may be empty.

ANCPP does not support GNU-style named variadic parameters. Standard C uses `...` and `__VA_ARGS__`, not `args...`:

```c
#define FOO(args...)            // Disabled: Ellipsis with name is not allowed.
```

**Stringizing `__VA_ARGS__`**

You can also use the `#` operator to stringize `__VA_ARGS__` in the replacement of variadic macros.

```c
#define SHOW_ALL(...) puts(#__VA_ARGS__);
SHOW_ALL(1, 2, foo, "bar")      // Expands to: `puts("1, 2, foo, \"bar\"");`
SHOW_ALL(1 2, foo "bar")        // Requires multi-token arguments: `puts("1 2, foo \"bar\"");`
SHOW_ALL()                      // Expands to: `puts("");`
```

`[ANCPP RESTRICTION]`: Applying `##` to `__VA_ARGS__` is not supported.

GNU preprocessors additionally support `, ##__VA_ARGS__` to remove a preceding comma when variable arguments are omitted. ANCPP does not support this extension; use the standard C23 form `format __VA_OPT__(,) __VA_ARGS__` instead.

```c
// Disabled: `##` with `__VA_ARGS__` is not supported.
#define LOG(format, ...) fprintf(stderr, format, ##__VA_ARGS__)
```

### 3.5 Predefined Macros

ANCPP defines the following macros automatically:

| Macro      | Description                                      |
|------------|--------------------------------------------------|
| `__FILE__` | The current file's name as a string literal. For the main file, ANCPP uses `source_file_path_name`; inside a header, it uses that header's path. |
| `__LINE__` | The current line number as an integer literal. `__FILE__` and `__LINE__` are useful in generating an error message that indicates the location in the source code. |
| `__DATE__` | The translation date as a string literal in the format `"Mmm dd yyyy"`. Standard C pads single-digit days with a space. |
| `__TIME__` | The translation time as a string literal in the format `"hh:mm:ss"`. |
| `__STDC_EMBED_NOT_FOUND__`, `__STDC_EMBED_FOUND__` and `__STDC_EMBED_EMPTY__` | Expand to 0, 1, and 2, respectively: the possible results of `__has_embed(...)`. |

A C compiler can supply additional predefined macros describing its implementation:

| Macro           | Description                                      |
|-----------------|--------------------------------------------------|
| `__STDC__`      | Expands to the integer constant 1. This macro is intended to indicate a conforming implementation. |
| `__STDC_VERSION__` | Expands to an integer constant of type long whose value increases with each version of the C standard: `199409L` for C95, `199901L` for C99, `201112L` for C11, `201710L` for C17 and `202311L` for C23. |
| `__STDC_HOSTED__` | Expands to 1 for a hosted implementation or 0 for a freestanding implementation. This describes the C execution environment and library guarantees, not simply the presence of an operating system. |
| `__STDC_UTF_16__` | Expands to 1 if `char16_t` uses UTF-16 encoding. |
| `__STDC_UTF_32__` | Expands to 1 if `char32_t` uses UTF-32 encoding. |

ANCPP does not define these implementation macros itself. The caller supplies them through the `predefinitions` parameter of `process_source_file`. Define them only when their claims hold for the complete C implementation; ANCPP alone does not establish C23 conformance.

### 3.6 Conditional compilation (`#if`, `#else` and `#endif`)

Conditional compilation instructs the preprocessor to select whether or not to include a chunk of code in the final token stream passed to the compiler. For example, a program may need to use different code depending on the machine or operating system it is to run on or the features that are enabled.

Preprocessor conditionals can test arithmetic expressions, or whether a name (identifier) is defined as a macro, or both simultaneously using the special `defined` operator.

A preprocessing conditional selects tokens during translation; excluded branches are not compiled as C code. An ordinary C `if` selects a branch during execution, but both branches must still be valid C. A compiler may optimize away a branch, so both branches do not necessarily appear in the generated machine code.

Conditional compilation is controlled by `#if`, `#else`, `#elif`, `#ifdef`, `#ifndef`, `#elifdef`, `#elifndef`, and `#endif` directives.

Syntax:

```c
#if EXPRESSION
    // code to include if EXPRESSION is evaluated to non-zero integer
#elif EXPRESSION
    // code to include if previous EXPRESSION is evaluated to zero
    // and this EXPRESSION is evaluated to non-zero integer
#else
    // code to include if all previous EXPRESSION are evaluated to zero
#endif
```

`#elif` and `#else` are optional. A conditional group may contain any number of `#elif` branches and at most one `#else`, which must come last. Conditions must be integer constant expressions: zero is false, and any nonzero value is true.

Example:

```c
#define VERSION 2
#if VERSION == 1
    // Code for version 1
#elif VERSION == 2
    // Code for version 2
#else
    // Code for other versions
#endif
```

Generally, we define macros without values (because they are not intended to be expanded) to enable or disable features in the code via conditional compilation. These macros are called feature flags.

```c

// Feature flags are usually defined in configuration headers
// e.g. `config.h`
#define FEATURE_PNG             // A feature flag to enable PNG support
#define FEATURE_JPEG            // A feature flag to enable JPEG support

// The following code is in the source files
// e.g. `main.c`

#if defined(FEATURE_PNG)
    // Code that supports PNG images
#endif

#if defined(FEATURE_JPEG)
    // Code that supports JPEG images
#endif
```

The `defined` operator tests whether a macro is defined, regardless of its replacement. It yields 1 for a defined macro and 0 otherwise.

`defined(IDENTIFIER)` can also be written as `defined IDENTIFIER`:

```c
#define FEATURE_PNG

#if defined FEATURE_PNG
    // Code that supports PNG images
#endif
```

The preprocessor provides four shorthand directives:

- `#ifdef IDENTIFIER` is equivalent to `#if defined(IDENTIFIER)`
- `#ifndef IDENTIFIER` is equivalent to `#if !defined(IDENTIFIER)`
- `#elifdef IDENTIFIER` is equivalent to `#elif defined(IDENTIFIER)`
- `#elifndef IDENTIFIER` is equivalent to `#elif !defined(IDENTIFIER)`

Each shorthand directive accepts only one identifier, whereas `#if` and `#elif` accept expressions.

Let's rewrite the previous example using shorthand directives:

```c
#define FEATURE_PNG

#ifdef FEATURE_PNG
    // Code that supports PNG images
#endif
```

**Feature flag checking conventions**

A macro defined as zero is still defined. The result depends on whether you test its existence or its value:

The following example checks the flags using `#ifdef`:

```c
#define FEATURE_PNG 0

#ifdef FEATURE_PNG
    // CAUTION: This code is included, because FEATURE_PNG is defined,
    // though its value is 0.
#endif
```

The following example checks the flags using `#if` followed by the macro name directly:

```c
#define FEATURE_PNG 0

#if FEATURE_PNG
    // CAUTION: This code is NOT included, because FEATURE_PNG
    // expands to 0 (false), though it is defined.
#endif
```

Choose a consistent convention: use empty macros with `#ifdef`/`#ifndef`, or numeric flags with `#if`. Do not mix existence checks and value checks unintentionally. In ANCPP, numeric flags must be defined before they are used in a condition.

`[ANCPP RESTRICTION]`: ANCPP rejects character literals and undefined identifiers in conditional expressions. It also rejects empty macro expansions where an operand is required. String literals and floating-point operands are already invalid in standard C preprocessing conditions.

In standard C preprocessing conditions, character constants have integer values and remaining identifiers are replaced by zero, except for `true`, which becomes 1 in C23. ANCPP instead diagnoses character literals and undefined identifiers to catch potential mistakes.

```c
#define OS_LINUX
#define EDITION "pro"
#define GENERATION 'A'
#define PI 3.14

#if ARCH_X86                    // Disabled: ARCH_X86 is undefined.
    // ...
#endif

#if OS_LINUX                    // Disabled: OS_LINUX is an empty macro.
    // ...
#endif

#if EDITION                     // Disabled: EDITION is a string literal.
    // ...
#endif

#if GENERATION                  // Disabled: GENERATION is a character literal.
    // ...
#endif

#if PI                          // Disabled: PI is a floating-point number.
    // ...
#endif
```

**Logical operators**

In conditional expressions, you can use logical operators to combine multiple conditions:

- `&&` (logical AND): The condition is true if both operands are true.
- `||` (logical OR): The condition is true if at least one operand is true.
- `!` (logical NOT): The condition is true if the operand is false.

```c
#if defined(ARCH_X86) && defined(OS_LINUX)
    // Code for x86 architecture on Linux
#endif

#if defined(OS_LINUX) || defined(OS_BSD)
    // Code for Linux or BSD operating systems
#endif

#if !defined(OS_WINDOWS)
    // Code for non-Windows operating systems
#endif
```

**Comparison, arithmetic, bitwise, and unary operators**

In addition to logical operators, conditional expressions also support comparison, arithmetic, bitwise, and unary operators:

- Comparison operators: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Arithmetic operators: `+`, `-`, `*`, `/`, `%`
- Bitwise operators: `~`, `&`, `|`, `^`, `<<`, `>>`
- Unary operators: `+`, `-`

These operators work on integer operands and produce integer results. Increment, decrement, assignment, and function calls are not permitted in preprocessing conditions.

```c
#define VERSION 1
#define BUILD 20260404L

#if VERSION == 1 && BUILD >= 20260101L
    // Code for version 1 with build date on or after 2026-01-01
#endif
```

Parentheses can be used to control grouping:

```c
#define OS_LINUX
#define ARCH_X86

#if (defined OS_LINUX || defined OS_BSD) && defined ARCH_X86
    // Code for x86 architecture on Linux or BSD
#endif
```

`[ANCPP RESTRICTION]`: The same operand restrictions apply inside larger expressions, including operands of logical, comparison, arithmetic, and bitwise operators:

```c
#define VERSION 1
#define OS_LINUX
#define EDITION "pro"
#define GENERATION 'A'
#define PI 3.14

#if VERSION || ARCH_X86         // Disabled: ARCH_X86 is undefined
    // ...
#endif

#if VERSION == 1 && OS_LINUX    // Disabled: OS_LINUX is an empty macro.
    // ...
#endif

#if EDITION == "pro"            // Disabled: EDITION is a string literal
    // ...
#endif

#if GENERATION < 'C'            // Disabled: GENERATION is a character literal
    // ...
#endif

#if PI * 2 > 3                  // Disabled: PI is a floating-point number
    // ...
#endif
```

> Identifiers `false` and `true` in conditional expressions are expanded to 0 and 1 respectively.

`[ANCPP RESTRICTION]`: The conditional operator `?:` is unsupported. Integer literals are parsed as `u64` and then evaluated using the host's signed `isize` arithmetic. ANCPP does not implement the full C preprocessing `intmax_t`/`uintmax_t` model or preserve suffix signedness during evaluation, so large values and unsigned comparisons can differ from standard C.

**Attribute testing**

The function-like operator `__has_c_attribute(...)` tests attribute support. For a supported standard attribute, it yields a standardized year-and-month value; otherwise it yields 0. ANCPP currently uses the fixed values below rather than querying the downstream compiler, so callers must ensure that the compiler supports the attributes ANCPP reports.

| attribute-token | Attribute          | Value     |
|-----------------|--------------------|-----------|
| `deprecated`    | `[[deprecated]]`   | `201904L` |
| `fallthrough`   | `[[fallthrough]]`  | `201904L` |
| `maybe_unused`  | `[[maybe_unused]]` | `201904L` |
| `nodiscard`     | `[[nodiscard]]`    | `202003L` |
| `noreturn`      | `[[noreturn]]`     | `202202L` |
| `unsequenced`   | `[[unsequenced]]`  | `202207L` |
| `reproducible`  | `[[reproducible]]` | `202207L` |

```c
// Define a macro for the deprecated attribute
#if __has_c_attribute(deprecated)
    #define DEPRECATED [[deprecated]]
#else
    #define DEPRECATED
#endif

DEPRECATED  // Expands to `[[deprecated]]` if supported, otherwise expands to nothing.
void old_function(int n) {
    // ...
}
```

> You can use `#ifdef __has_c_attribute` to check whether the `__has_c_attribute(...)` operator is supported by the preprocessor.

ANCPP also recognizes `_Noreturn` in this test, but `[[_Noreturn]]` is not a standard C23 attribute spelling; use `[[noreturn]]`.

### 3.7 Source file inclusion (`#include`)

`#include` inserts the contents of another file, usually a header containing declarations and definitions needed by the current file.

Syntax:

```c
#include <FILE_PATH>
#include "FILE_PATH"
```

Examples:

```c
#include <stdio.h>
#include "my_header.h"
```

Conceptually, `#include` replaces the directive with the included file's contents and processes them in the current preprocessing context. Consider these two files, `main.c` and `my_header.h`:

```c
// File: my_header.h
#define ENABLE_SSL
#define BUFFER_SIZE 1024
int read(char* buffer, int size);
```

```c
// File: main.c
#include "my_header.h"

#ifdef ENABLE_SSL
    char buffer[BUFFER_SIZE];
    int len = read(buffer, BUFFER_SIZE);
#endif
```

When processing `main.c`, the preprocessor loads `my_header.h`, processes its directives, and includes its remaining tokens. The result is equivalent to:

```c
int read(char* buffer, int size);

char buffer[1024];
int len = read(buffer, 1024);
```

The definitions from `my_header.h` affect the subsequent code in `main.c`.

The `#include` directive also accepts a macro that expands to a file path:

```c
#ifdef OS_WINDOWS
    #define HEADER_FILE <windows.h>
#else
    #define HEADER_FILE <unistd.h>
#endif

#include HEADER_FILE            // Includes either <windows.h> or <unistd.h> based on the OS
```

**Difference between `<FILE_PATH>` and `"FILE_PATH"`**

The search rules are implementation-defined in C. Compilers such as GCC and Clang typically use these conventions:

- `#include <FILE_PATH>` searches configured include directories, including compiler-provided and user-specified directories.
- `#include "FILE_PATH"` first searches relative to the file containing the directive, then falls back to configured include directories.

ANCPP uses the `FileProvider` passed to `process_source_file`. Angle-bracket inclusion searches system directories only. Quoted inclusion searches user directories, then system directories. To first search relative to the file containing a quoted directive, set `"resolve_header_file_relative_to_current_file"` to `true` in `compile_features`. The same search rules apply to resources and availability tests.

**The limitations of `FILE_PATH`**

A directly written header name is not an ordinary string literal: ANCPP does not interpret escape sequences in it. Use `/` as the path separator, including on Windows, to avoid ambiguity.

```c
// ANCPP treats the backslash literally, not as an escape.
// Prefer: #include "include/my_header.h"
#include "include\my_header.h"

// The spelling contains a literal backslash followed by x5fheader.h;
// it does not mean my_header.h. Whether backslash separates directories
// depends on the platform and file provider.
#include "my\x5fheader.h"

#include "my"header.h"              // Error: Double quote terminates the string literal
#include <my>header.h>              // Error: Greater-than sign terminates the angle-bracketed file path
```

For portability, avoid these characters and sequences in header names:

- The character `'`
- The character `"`
- The character `\`
- The character sequence `//`
- The character sequence `/*`

Their treatment can be undefined or implementation-defined under the C header-name rules.

Note that if you define a string literal macro and then use it in `#include`, the string literal rules apply:

```c
#define HEADER_FILE "my\u2764header.h"  // ANCPP decodes the Unicode escape in this string literal.
#include HEADER_FILE                    // Includes the file "my❤header.h"
```

**Include guards**

In standard C, including an unguarded header more than once can duplicate definitions and cause errors. ANCPP instead includes each resolved file only once per preprocessing run.

Consider these three files under conventional C inclusion rules:

```c
// File: foo.h
struct Item { int value; };

// File: bar.h
#include "foo.h"
#define MIN_SIZE 10

// File: main.c
#include "foo.h"
#include "bar.h"                // Standard C: Redefinition of struct Item without guards.
```

Although `main.c` names `foo.h` only once, `bar.h` includes it again. Without guards, a conventional preprocessor produces:

```c
// Contents from `#include "foo.h"`
struct Item { int value; };

// Contents from `#include "bar.h"`
struct Item { int value; };
#define MIN_SIZE 10
```

The duplicate structure definition causes a compilation error. Identical macro redefinitions alone would not cause this error. An _include guard_ prevents repeated inclusion of a header's contents:

```c
#ifndef HEADER_FILENAME_H
#define HEADER_FILENAME_H
// ... contents of the header
#endif
```

When the header file is included for the first time, the macro `HEADER_FILENAME_H` is not defined, so the contents of the header are included and the macro is defined. On subsequent inclusions, the macro is already defined, so the contents are skipped.

> In conventional C programming, the macro name used in include guards is usually the uppercase version of the header file name with non-alphanumeric characters replaced by underscores (`_`).

Many C compilers also support the non-standard `#pragma once` directive:

```c
#pragma once
// ... contents of the header
```

For portability to implementations that do not support `#pragma once`, use a traditional include guard. Both forms can appear in the same header if needed.

`[ANCPP RESTRICTION]`: ANCPP includes a resolved header only once per `process_source_file` call, even without a guard or `#pragma once`. Patterns that intentionally include the same file repeatedly with different macro definitions therefore do not work.

**`__has_include(...)` operator**

In a conditional expression, `__has_include(...)` yields 1 if the specified header can be found using the include search rules, or 0 otherwise. It does not check whether the header's contents are valid C.

Syntax:

- `__has_include("file")`
- `__has_include(<file>)`
- `__has_include(MACRO)`

Examples:

```c
#include "config.h"

#if __has_include("optional_config.h")
    #include "optional_config.h"
#endif

#if __has_include(<lz4.h>)
    // Code that uses LZ4 library
#elif __has_include(<zstd.h>)
    // Code that uses Zstandard library
#endif
```

The file path in `__has_include(...)` can also be a macro that expands to a file path:

```c
#ifdef OS_WINDOWS
    #define HEADER_FILE "windows_specific.h"
#else
    #define HEADER_FILE "unix_specific.h"
#endif

#if __has_include(HEADER_FILE)
    // Code that includes OS-specific header
#endif
```

ANCPP treats a directly written `__has_include("FILE_PATH")` argument as a header name and does not decode escape sequences. A macro expanding to a string literal follows ANCPP's string-literal decoding rules, as with `#include` above.

In `__has_include(<MACRO>)`, `MACRO` is part of the header name, not a request to expand that macro. To use a macro that expands to a complete header name, write `__has_include(MACRO)`.

### 3.8 Resource file inclusion (`#embed`)

`#embed`, introduced in C23, makes resource data available as integer tokens. It is commonly used to include binary data or other resources in a program.

Unlike `#include`, it does not preprocess the resource's contents. ANCPP emits a comma-separated sequence of byte values, which can be used in an array initializer:

```c
#embed "data.bin"
#embed <data.bin>
```

Example:

```c
const uint8_t binary_data[] =
{
    #embed "data.bin"
};
```

The processing result may look like:

```c
const uint8_t binary_data[] =
{
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0,
    0, 13, 73, 72, 68, 82, 0, 0, 9, 198,
    // ... More bytes from data.bin
};
```

Directive `#embed` also supports using a macro that expands to a file path:

```c
#define BINARY_FILE "data.bin"
#embed BINARY_FILE
```

When a macro expands to a string literal, ANCPP decodes its escape sequences:

```c
#define BINARY_FILE "data\\data.bin"
#embed BINARY_FILE              // Backslash is escaped in string literal
```

> Resource files are not subject to header include guards or ANCPP's once-only inclusion rule. Each `#embed` directive reads the specified resource.

**Parameters of `#embed`**

`#embed` supports these parameters:

- `limit(NUMBER)`: Limit the maximum number of bytes to embed from the file.
- `prefix(TOKENS)`: Tokens to prepend when the selected resource data is nonempty.
- `suffix(TOKENS)`: Tokens to append when the selected resource data is nonempty.
- `if_empty(TOKENS)`: Tokens to emit when the selected resource data is empty, including when `limit(0)` is used. In this case, `prefix` and `suffix` are ignored.

```c
const char message[] =
{
    #embed "message.txt" if_empty('H', 'e', 'l', 'l', 'o', '!', '\n')
    ,'\0' // Append null terminator to `message[]`
};

const uint8_t random_data[] =
{
    #embed "random.bin" limit(1024) \
        prefix(0xCA, 0xFE, 0x03, 0x05) \
        suffix(0xBE, 0xEF, 0x07, 0x09)
};
```

`[ANCPP RESTRICTION]`: C23 also permits the spellings `__limit__`, `__prefix__`, `__suffix__`, and `__if_empty__`. ANCPP supports only the unadorned names shown above.

`[ANCPP RESTRICTION]`: `limit` must resolve to a single non-negative integer literal, not a multi-token expression, even if the expression would evaluate to a non-negative integer.

```c
const uint8_t data[] =
{
    #embed "file.txt" limit(256 + 256)  // Disabled: Expression is not allowed.
    #embed "file.txt" limit(-100)       // Disabled: Negative number is not allowed.
    #embed "file.txt" limit("1024")     // Disabled: String literal is not allowed.
};
```

`[ANCPP RESTRICTION]`: ANCPP restricts `prefix`, `suffix`, and `if_empty` to comma-separated character or integer literals that fit in a byte. It converts these to byte values and automatically inserts commas between nonempty prefix, resource, and suffix sequences. Standard C inserts parameter tokens directly, so portable code must supply its own separating commas. The prefix/suffix example above illustrates ANCPP behavior, not portable C23 syntax.

```c
#embed "file.txt" prefix(0xAA 0xBB 0xCC)    // Disabled: Commas are required
#embed "file.txt" prefix("foo", "bar")      // Disabled: String literals are not allowed
#embed "file.txt" prefix(int a = 123)       // Disabled: Only character or integer literals are allowed
```

**`__has_embed(...)` operator**

The `__has_embed(...)` operator tests resource availability in conditional expressions:

Syntax:

- `__has_embed("file")`
- `__has_embed(<file>)`
- `__has_embed(MACRO)`

There are three possible results:

- `__STDC_EMBED_NOT_FOUND__` (0): The resource could not be found or the request is unsupported.
- `__STDC_EMBED_FOUND__` (1): The resource is available and the selected data is nonempty.
- `__STDC_EMBED_EMPTY__` (2): The resource is available but the selected data is empty.

Example:

```c
#if __has_embed("patch.dat") == __STDC_EMBED_NOT_FOUND__
    #error "Required resource file patch.dat not found."
#elif __has_embed("patch.dat") == __STDC_EMBED_EMPTY__
    // Code to handle empty "patch.dat"
#else
    // Code that uses the embedded "patch.dat"
#endif
```

`[ANCPP RESTRICTION]`: C23 permits embed parameters in `__has_embed(...)`, with unsupported parameters causing a result of `__STDC_EMBED_NOT_FOUND__`. ANCPP rejects parameters in this operator, so it tests the unmodified resource only.

### 3.9 Error and warning (`#error`, `#warning`)

`#error` reports an error, while `#warning` reports a warning:

```c
#error "This is an error message."
#warning "This is a warning message."
```

On an active `#error` directive, ANCPP stops preprocessing and returns an error. Standard C requires a diagnostic for this directive.

On an active `#warning` directive, ANCPP emits the `directive_warn` lint and continues. C23 standardizes `#warning`; it was an extension in earlier C versions.

### 3.10 Behavior control (`#pragma`)

`#pragma` controls implementation-defined behavior, such as floating-point settings or structure alignment. Unrecognized pragmas are ignored.

ANCPP recognizes the following pragma for include-guard diagnostics:

```c
#pragma once
```

Many implementations use this non-standard pragma to prevent repeated inclusion of a header.

ANCPP already includes each resolved header only once per preprocessing run, so `#pragma once` does not change inclusion behavior. A leading `#pragma once` satisfies the include-guard linter. Other pragmas have no effect and are not passed on to the downstream compiler.

The standard `_Pragma(string-literal)` operator allows a pragma to be produced through macro expansion. ANCPP accepts it only in macro replacements and discards it without applying pragma semantics or forwarding it to the downstream compiler. A standalone `_Pragma(...)` in ordinary source code is rejected.

## 4. Unsupported Directives

The following non-standard directives and operator are unsupported:

```c
#assert SYSTEM(unix)                // Error: Obsolete directive
#unassert SYSTEM                    // Error: Obsolete directive
#ident "foo"                        // Error: Non-standard directive
#sccs "foo"                         // Error: Non-standard directive
#include_next <file.h>              // Error: Non-standard directive
#if __has_include_next("file.h")    // Error: Non-standard operator
```

`[ANCPP RESTRICTION]`: The standard `#line` directive is unsupported. Code generators that use it to preserve source locations therefore need another way to provide location information.

```c
#line 100 "new_file.c"              // Error: Directive `#line` is not supported.
```

`[ANCPP RESTRICTION]`: ANCPP rejects null directives (`#` followed by a newline), although standard C permits them with no effect.

```c
#                                   // Error: Null directive.
```

## 5. Unsupported Features

Trigraphs were removed in C23 and are unsupported. Digraphs remain valid in C23, but ANCPP intentionally rejects them.

```c
%:include <stdio.h>                 // Disabled: Digraphs are disabled.
if (true) <% return 1; %>           // Disabled: Digraphs are disabled.
printf("%s\n", argv<:1:>);          // Disabled: Digraphs are disabled.

??=include <stdlib.h>               // Error: Trigraphs are removed in C23.
if (true) ??< return 1; ??>         // Error: Trigraphs are removed in C23.
printf("%s??/n", argv??(1??));      // Error: Trigraphs are removed in C23.
```

## 6. Token Types

ANCPP produces identifiers, numbers, string literals, character literals, and punctuators. The token model is based on C23, with the implementation differences noted below. ANCPP also concatenates adjacent string literals before returning the token stream.

Directives and excluded conditional branches do not appear in the final token stream. Eligible macro invocations are expanded; identifiers left unexpanded, such as suppressed self-references, can still appear.

### 6.1 Identifiers

ANCPP represents C keywords as identifier tokens, just like type, function, and variable names. The downstream parser distinguishes keywords from other identifiers.

An ASCII identifier consists of letters (`A-Z`, `a-z`), digits (`0-9`), and underscores (`_`), and starts with a letter or underscore.

- `myVariable`
- `_temp123`
- `MAX_SIZE`

Identifiers cannot start with a digit:

- `2ndValue`        // invalid
- `123_abc`         // invalid

Identifiers are case-sensitive, so `myVar`, `MyVar`, and `MYVAR` are considered different identifiers.

ANCPP's lexer also accepts directly written Unicode characters in the ranges U+00A0 to U+D7FF and U+E000 to U+10FFFF, for example:

- `变量`
- `ヴァリアブル`
- `❤️`

This is broader than C23's identifier rules, which use Unicode `XID_Start`/`XID_Continue` properties. ANCPP normalizes identifiers to NFC, but not every character it accepts, such as the heart symbol above, is a valid standard C identifier.

ANCPP represents scoped attribute names such as `vendor::attribute` with a `Token::NamespaceIdentifier(scope, name)` token. In standard C, `::` is a punctuator separating the scope and name, not part of an identifier.

### 6.2 String and Character Literals

ANCPP represents a character literal as one character enclosed in single quotes (`'`) and a string literal as a sequence of characters enclosed in double quotes (`"`). Standard C calls character literals *character constants*.

- `'A'`                 // character literal
- `"Hello, World!"`     // string literal

Character and string literals can contain the following escape sequences:

| Escape Sequence | Description         |
|-----------------|---------------------|
| `\\`            | Backslash (`\`)     |
| `\'`            | Single quote (`'`)  |
| `\"`            | Double quote (`"`)  |
| `\?`            | Question mark (`?`) |
| `\a`            | Alert (bell)        |
| `\b`            | Backspace           |
| `\t`            | Horizontal tab      |
| `\n`            | Newline             |
| `\v`            | Vertical tab        |
| `\f`            | Form feed           |
| `\r`            | Carriage return     |
| `\ooo`          | Octal value (1 to 3 digits), e.g., `\101` (A), `\12` (newline) |
| `\xhh`          | Hexadecimal value: ANCPP requires exactly two hex digits, e.g., `\x41` (A), `\x0A` (newline). Standard C consumes one or more hex digits. |
| `\uXXXX`        | Universal character name with 4 hex digits, e.g., `\u03A9` (Ω) |
| `\UXXXXXXXX`    | Universal character name with 8 hex digits, e.g., `\U000003A9` (Ω) |

`\e` is a non-standard escape supported by some compilers, including GCC and Clang. ANCPP does not support it. ANCPP also does not support C23's braced escape forms, such as `\x{41}` and `\u{03A9}`, or named universal character escapes such as `\N{GREEK CAPITAL LETTER OMEGA}`.

ANCPP additionally accepts the non-standard uppercase hex-escape spelling `\Xhh`; portable C uses lowercase `\x`.

String and character literal prefixes are also supported:

| Prefix | Description                        |
|--------|------------------------------------|
| `L`    | Wide character or string literal   |
| `u`    | UTF-16 character or string literal |
| `U`    | UTF-32 character or string literal |
| `u8`   | UTF-8 character or string literal  |

- `L'A'`: wide character literal
- `u8"Hello"`: UTF-8 string literal

ANCPP concatenates adjacent string literals only when their encoding prefixes match exactly. For example, `"a" "b"` is accepted, but `u8"a" "b"` is rejected, although standard C permits the latter combination.

`[ANCPP RESTRICTION]`: ANCPP rejects multicharacter constants such as `'AB'`, which standard C permits with implementation-defined values. Prefixed constants such as `u'CD'` are already invalid in C23.

### 6.3 Numbers

Number literals include integer literals and floating-point literals.

**Integer literals**

Integer literals can be in decimal, octal, hexadecimal, or binary format:

| Format      | Prefix  | Example                 |
|-------------|---------|-------------------------|
| Decimal     | None    | `123`, `0`, `4567`      |
| Octal       | `0`     | `0123`, `0777`          |
| Hexadecimal | `0x` or `0X` | `0x123`, `0XABC`   |
| Binary      | `0b` or `0B` | `0b1010`, `0B1101` |

Integer literals can have suffixes that affect their type:

| Suffix       | Description       | Example           |
|--------------|-------------------|-------------------|
| `l` or `L`   | Long integer      | `123l`, `0b1010L` |
| `ll` or `LL` | Long long integer | `123ll`, `456LL`  |
| `wb` or `WB` | Bit-precise integer (`_BitInt`) | `123wb`, `0XFFWB` |

The unsigned suffix `u` or `U` can appear alone, as in `123u`, or with another suffix, as in `123ul`, `45ull`, or `0XFFUWB`.

In C, an integer literal's type depends on its value, base, and suffix. For example, an unsuffixed decimal literal uses the first fitting type from `int`, `long int`, and `long long int`; unsuffixed nondecimal literals also consider unsigned types. ANCPP stores a normalized value spelling and suffix information rather than choosing a target-specific C type.

**Floating-point literals**

Floating-point literals can be in decimal or hexadecimal format:

| Format      | Prefix       | Example                 |
|-------------|--------------|-------------------------|
| Decimal     | None         | `123.45`, `.667`, `1.414e10`, `1.732e+2`, `2.718e-3` |
| Hexadecimal | `0x` or `0X` | `0x1.23p4`, `0x4.5p+6`,  `0X7.5p-8` |

Binary floating-point literals can have suffixes that select their type:

| Suffix      | Description | Example                |
|-------------|-------------|------------------------|
| `f` or `F`  | `float`       | `123.45f`, `0x1.23p4F` |
| `l` or `L`  | `long double` | `123.45l`, `0X4.5P+6L` |

Decimal floating-point suffixes are `df`/`DF` for `_Decimal32`, `dd`/`DD` for `_Decimal64`, and `dl`/`DL` for `_Decimal128`, for example `123.45df`, `0.667DD`, and `1.0dl`. A single `d` or `D` is not a valid suffix. Standard C does not permit decimal floating-point suffixes on hexadecimal literals, even though ANCPP's lexer currently accepts them.

If a suffix is not specified, the type of the floating-point literal is `double`.

### 6.4 Punctuators

Punctuators include operators, brackets, delimiters, and other special symbols used in C programming.

**Arithmetic Operators**

| Punctuator | Description          |
|------------|----------------------|
| `+`        | Addition             |
| `-`        | Subtraction          |
| `*`        | Multiplication       |
| `/`        | Division             |
| `%`        | Modulus              |
| `++`       | Increment            |
| `--`       | Decrement            |

**Relational and Logical Operators**

| Punctuator | Description              |
|------------|--------------------------|
| `==`       | Equal to                 |
| `!=`       | Not equal to             |
| `<`        | Less than                |
| `<=`       | Less than or equal to    |
| `>`        | Greater than             |
| `>=`       | Greater than or equal to |
| `&&`       | Logical AND              |
| `\|\|`     | Logical OR               |
| `!`        | Logical NOT              |

**Bitwise Operators**

| Punctuator | Description          |
|------------|----------------------|
| `&`        | Bitwise AND          |
| `\|`       | Bitwise OR           |
| `^`        | Bitwise XOR          |
| `~`        | Bitwise NOT          |
| `<<`       | Left shift           |
| `>>`       | Right shift          |

**Assignment Operators**

| Punctuator | Description               |
|------------|---------------------------|
| `=`        | Assignment                |
| `+=`       | Addition assignment       |
| `-=`       | Subtraction assignment    |
| `*=`       | Multiplication assignment |
| `/=`       | Division assignment       |
| `%=`       | Modulus assignment        |
| `&=`       | Bitwise AND assignment    |
| `\|=`      | Bitwise OR assignment     |
| `^=`       | Bitwise XOR assignment    |
| `<<=`      | Left shift assignment     |
| `>>=`      | Right shift assignment    |

**Brackets and Delimiters**

| Punctuator | Description             |
|------------|-------------------------|
| `(`        | Left parenthesis        |
| `)`        | Right parenthesis       |
| `{`        | Left brace              |
| `}`        | Right brace             |
| `[`        | Left bracket            |
| `]`        | Right bracket           |
| `[[`       | Left attribute bracket  |
| `]]`       | Right attribute bracket |
| `;`        | Semicolon               |
| `,`        | Comma                   |

**Other Punctuators**

| Punctuator  | Description                   |
|-------------|-------------------------------|
| `.`         | Member access                 |
| `->`        | Member access through pointer |
| `:` and `?` | Conditional operator          |
| `...`       | Ellipsis                      |

## 7. Links

- [ANCPP source code repository](https://github.com/hemashushu/xiaoxuan-c-preprocessor)
- [XiaoXuan C Compiler](https://github.com/hemashushu/xiaoxuan-c-compiler)

## 8. License

See [LICENSE](LICENSE) for MPL-2.0 and [LICENSE.additional](LICENSE.additional) for the additional terms that apply to this project.

## 9. References

- [cppreference: C preprocessor](https://en.cppreference.com/w/c/preprocessor.html)
- [GCC CPP Manual](https://gcc.gnu.org/onlinedocs/cpp/)
- [The C standard](https://www.c-language.org/)
