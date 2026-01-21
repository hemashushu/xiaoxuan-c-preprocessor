# XiaoXuan C Preprocessor

**ANCPP** (XiaoXuan C Preprocessor) is a C preprocessor implemented in Rust. It is compliant with the C23 standard and implements standard preprocessing features — macro expansion, conditional compilation, file and resource inclusion, predefined macros, and diagnostics.

ANCPP is provided primarily as a library for the ANCC project (XiaoXuan C Compiler). It processes C source files and emits a stream of tokens (with locations) that can be consumed by a C AST parser.

<!-- @import "[TOC]" {cmd="toc" depthFrom=2 depthTo=4 orderedList=false} -->

<!-- code_chunk_output -->

- [1. Features](#1-features)
- [2. Usage](#2-usage)
- [3. How preprocessor works](#3-how-preprocessor-works)
  - [3.1 Macro definition (`#define`)](#31-macro-definition-define)
  - [3.2 Undefined macros (`#undef`)](#32-undefined-macros-undef)
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
- [7. License](#7-license)
- [8. References](#8-references)

<!-- /code_chunk_output -->

## 1. Features

- Fully C23 standard compliance, focusing on modern and portable C projects.
- No non‑standard extensions, directives or behaviors.
- Disable some "flex" features by default to make the preprocessor more predictable and less error-prone.
- Precise and helpful error messages.

## 2. Usage

To use ANCPP, add the following dependency to your `Cargo.toml`:

```toml
[dependencies]
ancpp = "0.1.0"
```

Alternatively, you can add it using the following command:

```bash
cargo add ancpp
```

The boilerplate code to preprocess a C source file:

```rust
// Build the `FileProvider`, an abstract file system interface
// that provides access to the user and system header files and binary resources.
let project_root_path = Path::new("/path/to/my/c/project");
let user_include_dirs = vec![
    project_root_path.join("include"),
    project_root_path.join("src/header")
];

let system_include_path = Path::new("/usr/include");
let system_include_dirs = vec![system_include_path];

let file_provider = NativeFileProvider::new(&user_include_dirs, &system_include_dirs);

// Build the `HeaderFileCache`, which caches parsed header files
// to speed up preprocessing when handling multiple source files.
let mut file_cache = HeaderFileCache::new();

// A flag to control whether to resolve relative paths within the current source file.
//
// Set to true to resolve relative paths to the source file,
// otherwise, preprocessor will only search in the specified including directories.
//
// For example, consider there are 3 defined include directories:
//
// - `./include`
// - `./src/headers`
// - `/usr/include`
//
// The statement `#include "foo.h"` in the source file `src/main.c` will try to
// match `foo.h` in the following order:
//
// - `./include/foo.h`
// - `./src/headers/foo.h`
// - `/usr/include/foo.h`
//
// If `resolve_relative_path_within_current_file` is true, then it will also
// try to match `src/foo.h`.
let resolve_relative_path_within_current_file = false;

// Add the predefined macros you need
let mut predefinitions = HashMap::new();
predefinitions.insert("__STDC__".to_string(), "1".to_string());
predefinitions.insert("__STDC_VERSION__".to_string(), "202311L".to_string());
// Set `__STDC_HOSTED__` to 1 for hosted environments, 0 for freestanding
predefinitions.insert("__STDC_HOSTED__".to_string(), "1".to_string());
// Set `__STDC_UTF_16__` to 1 for UTF-16 support, e.g., `u'文'` and `u"hippo🦛"`
predefinitions.insert("__STDC_UTF_16__".to_string(), "1".to_string());
// Set `__STDC_UTF_32__` to 1 for UTF-32 support, e.g., `U'宏'` and `U"spark✨"`
predefinitions.insert("__STDC_UTF_32__".to_string(), "1".to_string());

// Process one source file and get the preprocessed result

// The number of the source file, used for error reporting.
// It should begin with `FILE_NUMBER_SOURCE_FILE_BEGIN` (which default to 65536)
// and incremented by 1 for each source file.
//
// Note that the term "source file" refer only to the C source file (e.g. `main.c`),
// it does not include the header files (e.g. `header.h`).
let source_file_number = FILE_NUMBER_SOURCE_FILE_BEGIN + 1;

// The path name of the source file.
// It is used to generate the value of the `__FILE__` macro and the parser wouldn't use it to load the file.
// Usually, it's value is the relative path to the project root directory.
let source_file_path_name = Path::new("src/main.c");

// The canonical full path of the source file.
// This is used to load the source file content actually.
let source_file_full_path = Path::new("path/to/my/c/project/src/main.c");

let result = process_source_file(
        file_provider,
        file_cache,
        &ancpp::token::C23_KEYWORDS,
        predefinitions,
        resolve_relative_path_within_current_file,
        source_file_number,
        source_file_path_name,
        source_file_full_path,
    )
    .unwrap();

println!("{:?}", result);

// Process another source file
let source_file_number = FILE_NUMBER_SOURCE_FILE_BEGIN + 2;
let source_file_path_name = Path::new("src/utils.c");
let source_file_full_path = Path::new("path/to/my/c/project/src/utils.c");
let result = process_source_file(
        file_provider,
        file_cache,
        &ancpp::token::C23_KEYWORDS,
        predefinitions,
        resolve_relative_path_within_current_file,
        source_file_number,
        source_file_path_name,
        source_file_full_path,
    ).unwrap();

println!("{:?}", result);
```

The function `process_source_file` is the main API provided by ANCPP. It takes a `FileProvider`, a `HeaderFileCache`, a map of predefined macros, and several parameters related to the source file, and returns the preprocessed result as a `Result<PreprocessResult, PreprocessorError>`. A `PreprocessResult` contains a vector of `TokenWithLocation` (the preprocessed tokens, ready for consumption by the AST parser of compilers) and a vector of `Prompt` (warnings and hints generated during preprocessing).

By default, ANCPP only resolves header and resource files located in the user and system directories specified in the `FileProvider`. For example, if your source file `src/main.c` contains a statement `#include "./header.h"`, ANCPP will search for `header.h` in the user including directories (such as `include`, `src/header` directories under the project root), and the system including directories (e.g., `/usr/include`). It will not search other directories (e.g. the project's source code folder `src/`) unless you enable the parameter `resolve_relative_path_within_current_file`. Most C compilers, such as GCC and Clang (LLVM), resolve relative paths which is related to the current file being compiled. To match this behavior, set the `resolve_relative_path_within_current_file` parameter to `true`.

In a typical C project with multiple source and header files, preprocessing may produce errors or warnings, each associated with a specific source file number (as well as line and column). ANCPP automatically assigns file numbers to header files, starting from 1 (file number 0 is reserved for predefined macros). The source file number should be specified manually using the `source_file_number` parameter. To avoid conflicts with header files, the source file number should be greater than or equal to `FILE_NUMBER_SOURCE_FILE_BEGIN`, which is defined as 65536 in ANCPP.

The loaded header file list can be obtained by calling the `get_cache_file_list` method on the `HeaderFileCache`. This list contains all header files that were successfully loaded during preprocessing, along with their file numbers and paths.

## 3. How preprocessor works

The preprocessor is works as a separate phase before the actual compilation of C source code. The purpose of a preprocessor is to modify the C source code with a "tiny language", the preprocessing language, which is consists of **preprocessor directives** that are embedded in the C source code. Each directive occurs on a separate line, which begin with the `#` character (possibly preceded by whitespace), and continue until the end of the line.

The directive lines are the "code" of the preprocessor, while the rest of the C source code is treated as "data" to be processed.

Directives performs various operations such as macro expansion, conditional compilation, and file inclusion. ANCPP supports almost all of the C23 standard preprocessor directives, operators, and built-in macros.

> Preprocessor operates as a textual substitution, which makes it extremely "flexible" because it has no knowledge of C syntax or semantics. This flexibility can result in code that does not conform to C syntax or disrupts control flow, often due to unintentional mistakes by developers that are difficult to detect. ANCPP intentionally disable some features by default to make it more suitable for modern C application programming. The limitations and constraints are described with the mark `[ANCPP RESTRICTION]` in the following sections.

### 3.1 Macro definition (`#define`)

Syntax:

`#define IDENTIFIER REPLACEMENT`

The `#define` directives define the identifier as a macro, that is they instruct the preprocessor to replace all successive occurrences of `IDENTIFIER` with `REPLACEMENT`.

Examples:

```c
#define PI 3.14159
float c = 2 * PI * r;           // Expands to: `float c = 2 * 3.14159 * r;`

#define MESSAGE "Hello, World!\n"
puts(MESSAGE);                  // Expands to: `puts("Hello, World!");`
```

The replacement can be not only a single value, but also expressions or statements.

```c
#define SAY_HELLO puts("Hello, World!\n");
SAY_HELLO                       // Expands to: `puts("Hello, World!\n");`

#define DATA {1, 2, 3, 4, 5}
int arr[] = DATA;               // Expands to: `int arr[] = {1, 2, 3, 4, 5};`
```

If the replacement contains multiple lines, you can use the backslash `\` at the end of each line to indicate that the macro definition continues on the next line.

```c
#define ASSERT if (arg != 0) { \
        puts("Assertion failed.\n"); \
        exit(1); \
    }

int myfunc(int arg) {
    ASSERT                      // Expands to the if statement above.
    // ... rest of the function
}
```

> While a single value macro has no significant advantage over a `const` variable, complex macros work as "code generators" that can greatly improve code readability and maintainability by avoiding repetitive code.

The replacement can also contain other macros, which will be expanded.

```c
#define FOO 42
#define BAR (FOO) + 1
#define BUZZ (BAR) * 2
int x = BUZZ;                   // Expands to: `int x = ((42) + 1) * 2;`
```

`[ANCPP RESTRICTION]`: Macro replacement can not contain any unbalanced parentheses, brackets or quotes.

```c
#define A if (i==0              // Disabled: Unbalanced parentheses.
#define B if (i==0) {           // Disabled: Balanced parentheses but unbalanced braces.
#define D int n = nums[         // Disabled: Unbalanced brackets.
#define C int nums[] = {        // Disabled: Balanced brackets but unbalanced braces.
#define E do {                  // Disabled: Unbalanced braces.
#define F } else {              // Disabled: Unbalanced braces.
#define G char* s="string       // Disabled: Unbalanced quotes.
#define H char c='a             // Disabled: Unbalanced quotes.
```

Since macros are pure textual substitution (that is, text find-and-replace), unbalanced parentheses, brackets or quotes in macro replacements can lead to invalid C code (or disrupting control flow) after macro expansion, which may be hard to detect. This constraint helps catch such mistakes early.

> If something the C constants can do, prefer using `const` variables instead of macros. Macros should be used only in "static code generation" scenarios.

**Empty replacement**

The replacement can also be empty:

```c
#define FOO
puts("Hello" FOO "World!\n");   // Expands to: `puts("Hello" "World!\n");`
```

The empty macros are often used as a switch to enable or disable certain features in the code via conditional compilation.

**Redefinition of macros**

You can not redefine an existing macro with a new definition directly.

```c
#define VERSION 1
#define VERSION 2               // Error: Redefinition an existing macro
```

To redefine an existing macro, you must first undefine it using the `#undef` directive (see the next section).

```c
#define VERSION 1
// ... some code that uses VERSION
#undef VERSION
#define VERSION 2
// Now VERSION expands to 2
```

`[ANCPP RESTRICTION]`: You can not redefine an existing macro even if the new replacement is identical.

```c
#define BUZZ 789
#define BUZZ 789                // Disabled: Even though the new replacement is identical
```

This constraint helps catch potential mistakes where developers accidentally redefine macros.

**Reserved identifiers**

You can not define macros with name `defined`, which is a special operator in conditional expressions (will be explained later).

```c
#define defined 123              // Error: "defined" is a reserved identifier.
```

The ANCPP function `process_source_file` accepts an argument `reserved_identifiers`, which is a set of identifiers that you want to reserve and prevent users from defining them as macros.

By default ANCC pass the `ancpp::token::C23_KEYWORDS` constant to reserve all C23 keywords, this helps avoid accidental macro definitions that conflict with C language keywords.

**Macros that haven't been defined**

Because macros are textual substitution essentially, if you are using a macro that havn't been defined yet, it will not be replaced and remains unchanged in the source code. This may lead to unexpected behaviors or errors during compilation.

```c
puts("Hello" BUZZ "World!\n");  // Expands to: `puts("Hello" BUZZ "World!\n");`
                                // Which causes compilation error because of invalid syntax.
```

Unfortunately, preprocessors can not detect such issues because they do not analyze the semantics of the code. It is the responsibility of developers to ensure that all macros used in the code are properly defined. By convention, we use uppercase names for macros to distinguish them from regular identifiers, which helps reduce the likelihood of such mistakes.

**Self-referential macros**

A self-referential macro is one whose name appears in its definition. By convention, self-referential macros will **NOT** lead to infinite recursion because one macro is only expanded once per occurrence in the source code.

```c
#define FOO (4 + FOO)
int x = FOO;                    // Expands to: `int x = (4 + FOO);`.
                                // Not `(4 + (4 + (4 + ... )))`.

#define EACCES EACCES
int err = EACCES;               // Expands to: `int err = EACCES;`.
                                // Not inifinite loop.
```

This rule also applies to indirect self-reference (cross-reference) between multiple macros.

```c
#define FOO (BAR + 1)
#define BAR (FOO + 1)

int x = FOO;                    // Expands to: `int x = ((FOO + 1) + 1);`.
int y = BAR;                    // Expands to: `int y = ((BAR + 1) + 1);`.
```

### 3.2 Undefined macros (`#undef`)

Syntax:

`#undef IDENTIFIER`

The `#undef` directive undefines the identifier, that is it cancels the previous definition of the identifier by `#define` directive.

Imagine there is a macro-map in the preprocessor that stores all defined macros. When the preprocessor encounters an `#undef IDENTIFIER` directive, it removes the entry for `IDENTIFIER` from the macro-map.

The `#undef` directive is usually used to release a macro identifier or to redefine a macro with a different replacement.

```c
#define VERSION 1
// ... some code that uses VERSION
#undef VERSION
#define VERSION 2
// Now VERSION expands to 2
```

`[ANCPP RESTRICTION]`: If the identifier does not have an associated macro, the directive is failed.

```c
#undef FOO                      // Disabled: Macro "FOO" has not been defined.
```

While traditional preprocessors silently ignore attempts to undefine undefined macros, ANCPP treats it as an error. This constraint helps catch typos.

### 3.3 Function-like macros

The macros above are called object-like macros because they resemble data objects, and the preprocessor simply replaces occurrences of the macro identifier with its replacement.

Preprocessor also supports function-like macros, which resemble functions in C. When the preprocessor encounters a function-like macro invocation, it replaces the macro identifier and its arguments with the replacement text, substituting the arguments into the replacement text.

Syntax:

```c
// Define a function-like macro
#define IDENTIFIER(PARAMETERS) REPLACEMENT
// Invoke the function-like macro
IDENTIFIER(ARGUMENTS)
```

Examples:

```c
#define SQUARE(x) ((x) * (x))
int a = SQUARE(5);              // Expands to: `int a = ((5) * (5));`

#define MAX(a, b) ((a) > (b) ? (a) : (b))
int m = MAX(10, 20);            // Expands to: `int m = ((10) > (20) ? (10) : (20));`
```

You may notice that in the above examples, the parameters in the replacement text are wrapped in parentheses, sometimes the whole replacement is also wrapped in parentheses. This is a common practice to preserve correct operator precedence after macro expansion because the arguments are textually substituted. The following example illustrates the issue:

```c
#define ADD(a, b) a + b         // Warning: Missing parentheses may lead to unexpected results.
int x = ADD(1, 2) * 3;          // Expands to: `int x = 1 + 2 * 3;` which is evaluated as `1 + (2 * 3) = 7`, not `(1 + 2) * 3 = 9`
```

If the replacement is compound statements, you should also wrap the whole replacement in a `do { ... } while (0)` construct to ensure correct control flow.

```c
#define CHECK_AND_EXIT(cond) \
    if (!cond) { \
        puts("Error occurred.\n"); \
        exit(1); \
    }                           // Warning: Missing `do { ... } while (0)`

if (is_cli)
    CHECK_AND_EXIT(argc);
else run(argc, argv);

// The above expands to:

// if (is_cli)
//     if (!argc) {
//         puts("Error occurred.\n");
//         exit(1);
//     };
// else run(argc, argv);        // Invalid `else` because there is no matching `if`.
```

`[ANCPP RESTRICTION]`: Function-like macro replacement also requires balanced parentheses, brackets and quotes.

**Space between macro name and opening parenthesis**

Note that there is no space between the macro name and the opening parenthesis `(` in the definition of function-like macros, this is different from C functions where a space is allowed.

```c
#define ADD (a, b) (a) + (b)    // It just defines an object-like macro named "ADD" because of the space.
int sum = ADD(3, 4);            // Expands to: `int sum = (a, b) (a) + (b) (3, 4);` which is invalid syntax.
```

Space in invocations is allowed though, but it is not recommended because it may lead to confusion.

```c
#define INC(x) (x) + 1          // Ok
int i = INC (5);                // Expands to: `int i = (5) + 1;`
```

**Invocation**

There is no a special directive for invoking function-like macros, you just write the macro name, and arguments enclosed with parentheses `()`. The invocation looks similar to calling a C function (use uppercase names for function-like macros to distinguish them from regular C functions), including the arguments can cross multiple lines, the number of arguments must match the number of parameters defined in the macro:

```c
#define MULTIPLY(a, b) (a) * (b)
int p = MULTIPLY(2, 3);         // Expands to: `int p = (2) * (3);`
int q = MULTIPLY(4);            // Error: Missing argument.
int r = MULTIPLY(5, 6, 7);      // Error: Too many arguments.
```

Object-like macros can not be invoked like function-like macros:

```c
#define FOO 42
int x = FOO(11);                // Expands to: `int x = 42(11);`
```

Function-like macro can not be invoked without parentheses:

```c
#define INC(x) (x) + 1;
int y = INC 42;                 // Expands to: `int y = INC 42;`
```

`[ANCPP RESTRICTION]`: Arguments can not be empty.

```c
#define SUBTRACT(a, b) (a) - (b)
int d = SUBTRACT(10, );         // Disabled: The second argument is empty.
int e = SUBTRACT(, 5);          // Disabled: The first argument is empty.
int f = SUBTRACT(, );           // Disabled: Both arguments are empty.
int g = SUBTRACT((,),)          // Disabled: The first argument is `(,)`, but the second argument is empty.
```

The examples above are valid in traditional preprocessors, where empty arguments are treated as zero-length tokens. However, this can easily lead to expand to invalid C code, so ANCPP disallows empty arguments.

`[ANCPP RESTRICTION]`: Macro invocation arguments are limited to a single identifier, string (or adjacent string literals), character, or number. Expressions and punctuation are not allowed as arguments.

In the traditional preprocessors, function-like macro arguments can be anything, including punctuations (even the commas within nested parentheses), expressions, statements, code blocks and directives. However, this flexibility can lead to unexpected behaviors, especially when the arguments have side effects or involve complex expressions.

```c
#define DOUBLE(x) (x) * (x)

int i = 2;
int j = DOUBLE(i++);                    // Disallowed: Argument "i++" is an expression.

// The invocation above expands to: `int j = (i++) * (i++);`, which causes unexpected behavior due to multiple increments of i.

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

This constraint significantly weakens the flexibility of function-like macros, but it ensures that function-like macro arguments are simple and predictable. Since ANCC is designed for C beginner and modern C applications programming, this trade-off is acceptable.

> If something the C functions can do, prefer using C functions (inline functions if necessary) instead of function-like macros. Function-like macros should be used only in "static code generation" scenarios.

**Expansion of function-like macros**

When invoking a function-like macro with identifiers, strings, characters or numbers as arguments, they are treated as **literal text**, they wouldn't be evaluated or interpreted in any way like C language does, and then substituted into the replacement text.

If passed arguments are macros themselves, they are **expanded first** (recursively expanded if necessary), and then the expanded text is substituted into the replacement.

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
// 4. Substitute the arguments into the replacement and gets: `(left) + (2)`.
```

The arguments are first expanded also in nested function-like macro invocations:

```c
#define INC(a) (a) + 1
int x = INC(INC(3));            // Expands to: `int x = ((3) + 1) + 1;`

// The steps of expansion of `INC(INC(3))` are:
//
// 1. The argument `INC(3)` is a macro invocation, so it is expanded first.
// 2. Substitute the argument `3` into the replacement and gets: `(3) + 1`.
// 3. Invoke the outer `INC(...)`.
// 4. Substitute the argument into the replacement and gets: `((3) + 1) + 1`.
```

When the subsitution is done, the resulting text is **scanned again** for macros and expanded.

```c
#define OPEATOR +
#define ADD(a, b) (a) OPEATOR (b)

int x = ADD(1, 2);              // Expands to: `int x = (1) + (2);`

// The steps of expansion of `ADD(1, 2)` are:
//
// 1. Argument `1` and `2` are not macros, so they remain unchanged.
// 2. Substitute the arguments into the replacement and gets: `(1) OPEATOR (2)`.
// 3. Scan the result for macros and find `OPEATOR`, it is expanded to `+`.
// 4. The final expansion is `(1) + (2)`.
```

If the resulting text contains another function-like macro invocation, it is also expanded:

```c
#define GREETING(msg) puts(msg);
#define INVOKE(func_name, argument) func_name(argument)

INVOKE(GREETING, "Hello, World!\n");    // Expands to: `puts("Hello, World!\n");`

// The steps of expansion of `INVOKE(GREETING, "Hello, World!\n")` are:
//
// 1. Argument `GREETING` is an identifier (it's not an invocation), so it remains unchanged.
// 2. Argument `"Hello, World!\n"` is a string literal, so it remains unchanged as well.
// 3. Substitute the arguments into the replacement and gets: `GREETING("Hello, World!\n");`.
// 4. Scan the result for macros and find `GREETING`.
// 5. Expand `GREETING("Hello, World!\n")`.
// 6. Substitute the argument into the replacement and gets: `puts("Hello, World!\n");`.
```

Becareful with nested function-like macro invocations, because the actual arguments that produced from the first expansion may be mismatched for the second invocation:

```c
#define WRITE(msg) puts(msg);
#define SURROUND(text) "[", text, "]"

#define LOG(s) WRITE(s)

LOG(SURROUND("Hello"))          // Error: Mismatched arguments.

// The steps of expansion of `LOG(SURROUND("Hello"))` are:
//
// 1. Argument `SURROUND("Hello")` is a macro invocation, so it is expanded first.
// 2. Substitute the argument `"Hello"` into the replacement and gets: `"[", "Hello", "]"`.
// 3. Invoke the outer `LOG(...)`.
// 4. Substitute the argument into the replacement and gets: `WRITE("[", "Hello", "]");`.
// 5. Scan the result for macros and find `WRITE`.
// 6. Error: `WRITE` expects only one argument, but three arguments are provided.
```

**Self-referential function-like macros**

Invokes of self-referential function-like macros also only expand once per occurrence.

```c
#define DEC(x) DEC(x - 1)
int n = DEC(5);                 // Expands to: `int n = DEC(5 - 1);`.

#define WRITE(x) ">" FMT(x)
#define FMT(y) WRITE(y) "!\n"

WRITE("Hello")                  // Expands to: `">" WRITE("Hello") "!\n";`
```

**Stringizing (the `#` operator)**

In a function-like macro's replacement list, placing `#` before a parameter name (spaces between `#` and the parameter are allowed) converts the argument's source spelling into a C string literal.

```c
#define DEBUG(var) printf("%s=%d\n", #var, var);
int foo = 42;
DEBUG(foo);                     // Expands to: `printf("%s=%d\n", "foo", x);`
```

Not only identifiers, but also numbers, chars, and string literals can be stringized. When stringize a string literal, the quotes and special characters are escaped automatically.

```c
#define SHOW(x) puts(#x);
SHOW(42);                       // Expands to: `puts("42");`
SHOW('c');                      // Expands to: `puts("'c'");`
SHOW("Hello");                  // Expands to: `puts("\"Hello\"");`
```

The spaces are allowed between `#` and the parameter name, so `puts(#x)` above can also be written as `puts(# x)`.

`[ANCPP RESTRICTION]`: Stringizing can only be applied to identifiers, numbers, chars, or string literals.

```c
#define FOO(x) #x
FOO(a + b);                     // Disabled: Argument is an expression.
FOO(foo());                     // Disabled: Argument is a macro invocation.
```

Note that the `#` operator will stop expansion of the argument, so if you pass a macro as the argument, the macro name is used. You may remember that the macro will be **expanded first** when passed as arguments in function-like macro invocations, but the `#` operator is an exception, it discards the expansion result and uses the original macro name instead.

```c
#define SHOW(x) puts(#x);

#define FOO 42
SHOW(FOO);                      // Expands to: `puts("FOO");`, not `puts("42");`
```

If you want to stringize the expanded value of a macro argument, you need to use an additional level of indirection.

```c
#define SHOW_MACRO_VAL(x) SHOW(x)
#define SHOW(x) puts(#x);

#define FOO 42
SHOW_MACRO_VAL(FOO);            // Expands to: `puts("42");`

// The steps of expansion of `SHOW_MACRO_VAL(FOO)` are:
//
// 1. Expand `SHOW_MACRO_VAL(FOO)`, because the replacement `SHOW(x)` does not contain `#`, so the argument `FOO` is expanded first to `42`.
// 2. Substitute the argument into the replacement and gets: `SHOW(42)`.
// 3. Scan the result for macros and find `SHOW`, so it is expanded next.
// 4. Expand `SHOW(42)`, now the replacement contains `#`, so the argument `42` remains unchanged.
// 5. The final expansion is `puts("42");`.
```

The stringizing result table:

| Argument      | Stringize Result |
|---------------|------------------|
| `identifier`  | `"identifier"`   |
| `number`      | `"number"`       |
| `'character'` | `"'character'"`  |
| `"string"`    | `"\"string\""`   |
| `MACRO_NAME`  | `"MACRO_NAME"`   |

Stringizing may only be applied to parameters of function-like macros.

**Token concatenation (the `##` operator)**

Token concatenation joins two or more tokens into a single token. The tokens are concatenated without any whitespace in between.

```c
#define ALT(x) x##2
ALT(foo)                        // Expands to: `foo2`

#define VAR(family, member, index) family ## member ## index
VAR(bar_, a, 1)                 // Expands to: `bar_a1`
VAR(buz_, b, 2)                 // Expands to: `buz_b2`
```

Spaces around the `##` operator are allowed, so `x##2` above can also be written as `x ## 2`, and `family ## member ## index` can also be written as `family##member##index`.

`[ANCPP RESTRICTION]`: The result of token concatenation must be a valid C identifier: the first token must be an identifier, and the remaining tokens must be either identifiers or integer numbers.

```c
#define FOO(x) x##9s
FOO(2b);                        // Disabled: Result token "2b9s" is not a valid identifier.
FOO(+);                         // Disabled: Result token "+9s" is not a valid identifier.
```

Like the stringizing operator, `##` also stops expansion of the macro arguments.

```c
#define CONCAT(a, b) a##b
#define PREFIX foo
CONCAT(PREFIX, 1)               // Expands to: `PREFIX1`, not `foo1`
```

Token concatenation (the `##` operator) may only appear in macro replacement lists. It is most useful in function-like macros; while it is permitted in object-like macros, doing so is rarely meaningful.

### 3.4 Variadic macros

Variadic macros are function-like macros that accept a variable number of arguments. They are defined using an ellipsis `...` in the parameter list.

Syntax:

```c
#define IDENTIFIER(PARAMETERS, ...) REPLACEMENT
```

The additional arguments can be accessed using `__VA_ARGS__` macro, which is then replaced with arguments.

Examples:

```c
#define LOG(fmt, ...) fprintf(stderr, fmt, __VA_ARGS__)
LOG("Value: %d\n", 42);         // Expands to: `fprintf(stderr, "Value: %d\n", 42);`
LOG("Hello, World!\n", );       // Expands to: `fprintf(stderr, "Hello, World!\n", );`

#define STR(...) char str[] = { __VA_ARGS__ , '\0'}
STR('f', 'o', 'o');            // Expands to: `char line[] = { 'f', 'o', 'o', '\0'};`
STR();                         // Expands to: `char line[] = { , '\0'};`

#define VEC(type, name, ...) type name[] = { __VA_ARGS__ }
VEC(int, numbers, 1, 2, 3);     // Expands to: `int numbers[] = { 1, 2, 3 };`
VEC(char, letters);             // Expands to: `char letters[] = {  };`
```

You may use `__VA_OPT__(...)` to include content only if there are additional arguments provided, let's improve the examples above with `__VA_OPT__(...)`:

```c
#define LOG(fmt, ...) fprintf(stderr, fmt __VA_OPT__(,) __VA_ARGS__)
LOG("Value: %d\n", 42);         // Expands to: `fprintf(stderr, "Value: %d\n", 42);`
LOG("Hello, World!\n");         // Expands to: `fprintf(stderr, "Hello, World!\n" );`

#define STR(...) char str[] = { __VA_ARGS__ __VA_OPT__(,) '\0'}
STR('f', 'o', 'o');            // Expands to: `char str[] = { 'f', 'o', 'o', '\0'};`
STR();                         // Expands to: `char str[] = { '\0'};`

#define VEC(type, name, ...) type name[] __VA_OPT__( = { __VA_ARGS__ } )
VEC(int, numbers, 1, 2, 3);     // Expands to: `int numbers[] = { 1, 2, 3 };`
VEC(char, letters);             // Expands to: `char letters[];`
```

Note that `__VA_ARGS__` and `__VA_OPT__(...)` can only be used in the replacement of variadic macros.

`[ANCPP RESTRICTION]`: Parameter name is not allowed to precede the ellipsis in variadic macros.

```c
#define FOO(args...)            // Disabled: Ellipsis with name is not allowed.
```

`[ANCPP RESTRICTION]`: Apply `##` to `__VA_ARGS__` is not supported.

Some preprocessors support the `##` operator with `__VA_ARGS__` to remove the preceding comma when no additional arguments are provided. For example, the statement `#define LOG printf(fmt, ##__VA_ARGS__)` is effectively equivalent to `#define LOG printf(fmt __VA_OPT__(,) __VA_ARGS__)`. ANCPP does not support this feature to keep the preprocessor simple and avoid confusion.

```c
// Disabled: `##` with `__VA_ARGS__` is not supported.
#define LOG(format, ...) fprintf(stderr, format, ##__VA_ARGS__)
```

### 3.5 Predefined Macros

There are several macros that are predefined by the preprocessor, you can use them directly without defining them first. They are supported by all preprocessors, so ANCPP also built-in them:

| Macro      | Description                                      |
|------------|--------------------------------------------------|
| `__FILE__` | The current source file name as a string literal. In ANCPP, it is the value of the `source_file_path_name` parameter passed to `process_source_file(...)`. |
| `__LINE__` | The current line number as an integer literal. `__FILE__` and `__LINE__` are useful in generating an error message that indicates the location in the source code. |
| `__DATE__` | The current date in the format "MMM DD YYYY". |
| `__TIME__` | The current time in the format "HH:MM:SS". |
| `__STDC_EMBED_NOT_FOUND__`, `__STDC_EMBED_FOUND__` and `__STDC_EMBED_EMPTY__` | Expand to ​0​, 1, and 2, respectively. They are the possible results of the `__has_embed(...)` operator in conditional expressions. |

When the preprocessor is used in a C compiler, there are additional predefined macros:

| Macro           | Description                                      |
|-----------------|--------------------------------------------------|
| `__STDC__`      | Expands to the integer constant 1. This macro is intended to indicate a conforming implementation. |
| `__STDC_VERSION__` | Expands to an integer constant of type long whose value increases with each version of the C standard: `199409L` for C95, `199901L` for C99, `201112L` for C11, `201710L` for C17 and `202311L` for C23. |
| `__STDC_HOSTED__` | Expands to the integer constant 1 if the implementation is hosted (runs under an OS), ​0​ if freestanding (runs without an OS). |
| `__STDC_UTF_16__` | Expands to 1 to indicate that `char16_t` use UTF-16 encoding |
| `__STDC_UTF_32__` | Expands to 1 to indicate that `char32_t` use UTF-32 encoding. |

These macros are not built-in in ANCPP, they and other predefined macros are defined by the C compiler and passed to ANCPP via the `predefinitions` parameter of `process_source_file(...)` function.

### 3.6 Conditional compilation (`#if`, `#else` and `#endif`)

Conditional compilation instructs the preprocessor to select whether or not to include a chunk of code in the final token stream passed to the compiler. For example, a program may need to use different code depending on the machine or operating system it is to run on or the features that are enabled.

Preprocessor conditionals can test arithmetic expressions, or whether a name (identifier) is defined as a macro, or both simultaneously using the special `defined` operator.

A conditional in the C preprocessor resembles in some ways an `if` statement in C, but it is important to understand the difference between them. The condition in an `if` statement is tested during the execution of your program, all code in both branches of the `if` statement is compiled into the program. Its purpose is to allow your program to behave differently from run to run, depending on the data it is operating on. The condition in a preprocessing conditional directive is tested when your program is compiled. Its purpose is to allow different code to be included in the program depending on the situation at the time of compilation.

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

The `#elif` and `#else` parts are optional. You can have any number of `#elif` parts, but at most one `#else` part. The `#if` and `#elif` only accept expressions that can be evaluated to integer values, in which 0 means `false`, and 1 (and other non-zero integer) means `true`.

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

The `defined(...)` is a operator in conditional expressions to check whether a macro is defined (no matter it has a value or not). The result of `defined` or `defined(...)` operator is 1 if the macro is defined, and 0 if it is not defined.

`defined(...)` can be also written as `defined IDENTIFIER`:

```c
#define FEATURE_PNG

#if defined FEATURE_PNG
    // Code that supports PNG images
#endif
```

Since the flag checking is so common, preprocessor provides four shorthand directives:

- `#ifdef IDENTIFIER` is equivalent to `#if defined(IDENTIFIER)`
- `#ifndef IDENTIFIER` is equivalent to `#if !defined(IDENTIFIER)`
- `#elifdef IDENTIFIER` is equivalent to `#elif defined(IDENTIFIER)`
- `#elifndef IDENTIFIER` is equivalent to `#elif !defined(IDENTIFIER)`

Note that shorthand directives can only be followed by a single identifier, they are less flexible than `#if` and `#elif` which accept expressions.

Let's rewrite the previous example using shorthand directives:

```c
#define FEATURE_PNG

#ifdef FEATURE_PNG
    // Code that supports PNG images
#endif
```

**Feature flag checking conventions**

A confusing of flag checking is that what if a macro is defined with a value of zero? It depends on how you check it.

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

The good practice is define feature flags without values, and check them using `#ifdef` or `#ifndef`. This avoids confusion and makes the intention clear.

`[ANCPP RESTRICTION]`: In conditional expressions, expressions must be evaluated finally to integer values. Therefore, using or macros expanding to non-integer values (i.e., string literals, character literals, floating-point numbers, empty) is not allowed, and using undefined macros is not allowed either.

In traditional preprocessors, the character literals is evaluated to their integer values, and undefined macros are allowed and are treated as zero. However, this can easily confuse programmers, so ANCPP disallows them in conditional expressions.

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

It's similar to C expressions except lacking some operators like `++`, `--` etc, and the operands **must be integer values**, the result of the expression is also an integer value.

```c
#define VERSION 1
#define BUILD 20260404L

#if VERSION == 1 && BUILD >= 20260101L
    // Code for version 1 with build date on or after 2026-01-01
#endif
```

And the grouping parentheses `(...)` are also supported to control the precedence of evaluation:

```c
#define OS_LINUX
#define ARCH_X86

#if (defined OS_LINUX || defined OS_BSD) && defined ARCH_X86
    // Code for x86 architecture on Linux or BSD
#endif
```

`[ANCPP RESTRICTION]`: In conditional expressions, all operands of operators (including logical operators, and comparison, arithmetic, and bitwise operators) must be integer values or macros expand to integer values.

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

**Attribute testing**

In conditional expressions, you can use the `__has_c_attribute(...)` operator to check whether a specific C attribute is supported by the compiler. For standard attributes, it will expand to the year and month in which the attribute was added to the working draft (see table below), otherwise it expands to 0.

| attribute-token | Attribute          | Value     |
|-----------------|--------------------|-----------|
| `deprecated`    | `[[deprecated]]`   | `201904L` |
| `fallthrough`   | `[[fallthrough]]`  | `201904L` |
| `maybe_unused`  | `[[maybe_unused]]` | `201904L` |
| `nodiscard`     | `[[nodiscard]]`    | `202003L` |
| `noreturn`      | `[[noreturn]]`     | `202202L` |
| `_Noreturn`     | `[[_Noreturn]]`    | `202202L` |
| `unsequenced`   | `[[unsequenced]]`  | `202207L` |
| `reproducible`  | `[[reproducible]]` | `202207L` |

```c
// Define a macro for the fallthrough attribute
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

### 3.7 Source file inclusion (`#include`)

Directive `#include` is used to include the contents of another source file into the current source file. It is commonly used to include header files that contain declarations and definitions needed by the source file.

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

The effect of `#include` is similar to copying and pasting the entire contents of the included file into the location of the `#include` directive (of course, its contents are processed by the preprocessor first). Consider there are two source files: `main.c` and `my_header.h`:

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

When the preprocessor processes `main.c`, it encounters the `#include "my_header.h"` directive. It load the file `my_header.h` and then replaces the `#include` line with the processed contents. The result is:

```c
int read(char* buffer, int size);

char buffer[1024];
int len = read(buffer, 1024);
```

As you can see, the resulting code would look like merging the contents of `my_header.h` into `main.c` and resolving all directives, including macro definitions and conditional compilation.

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

The difference between the two forms is the way the preprocessor searches for the specified file:

- `#include <FILE_PATH>`: The preprocessor searches for the file in the system include directories (which are defined by the compiler) and user-specified include directories (which are specified by user via compiler command-line options).
- `#include "FILE_PATH"`: The preprocessor first searches for the relative file path in the same directory as the source file. If the file is not found there, it then searches in the system include directories and user-specified include directories.

In ANCPP, the system include directories and user-specified include directories are configured via the `FileProvider` interface passed to the `process_source_file(...)` function. By default when ANCPP handle `#include "FILE_PATH"` form, it wouldn't search the relative file path in the same directory as the current source file, it searches in user-specified include directories directly and then system include directories. To enable searching relative file path, you need to pass `true` to the `resolve_relative_path_within_current_file` parameter of `process_source_file(...)` function.

**The limitations of `FILE_PATH`**

The `FILE_PATH` is more restricted than regular string literals since it does not support escape sequences, the character `\` is treated as a normal character, although it is usually used as path separator in Windows platform, but it is recommended that use forward slash `/` as path separator even in Windows platform.

```c
// Backslash is a normal character.
// It is good practice to use forward slash `/` as path separator, e.g.
// "include "include/my_header.h"
#include "include\my_header.h"

// It represents the directory "my" followed by the file "x5fheader.h",
// It is not "my_header.h" since escape sequences `\x5f` are not supported in FILE_PATH.
// It is recommended to use forward slash `/` as path separator to avoid confusion, e.g.
// `#include "my/header.h"`
#include "my\x5fheader.h"

#include "my"header.h"              // Error: Double quote terminates the string literal
#include <my>header.h>              // Error: Greater-than sign terminates the angle-bracketed file path
```

Per standard suggestion, avoid using following characters in `FILE_PATH`:

- The character `'`
- The character `"`
- The character `\`
- The character sequence `//`
- The character sequence `/*`

Because file path tokens may be reinterpreted during preprocessing, and these characters may cause unexpected results.

Note that if you define a string literal macro and then use it in `#include`, the string literal rules apply:

```c
#define HEADER_FILE "my\u2764header.h"  // Backslash is escaped in string literal
#include HEADER_FILE                    // Includes the file "my❤header.h"
```

**Include guards**

If a header file is included multiple times in a source file (directly or indirectly), it may cause redefinition errors.

Consider there are three source files: `foo.h`, `bar.h`, and `main.c`:

```c
// File: foo.h
#define MAX_SIZE 100

// File: bar.h
#include "foo.h"
#define MIN_SIZE 10

// File: main.c
#include "foo.h"
#include "bar.h"                // Error: Redefinition of `MAX_SIZE`
```

In the above example, `main.c` does not include `foo.h` multiple times, it seems correct. However, as explained before, the `#include` directive effectively copy-and-pastes, in the view of `main.c`, the contents looks like:

```c
// Contents from `#include "foo.h"`
#define MAX_SIZE 100

// Contents from `#include "bar.h"`
#define MAX_SIZE 100
#define MIN_SIZE 10
```

As you can see, `MAX_SIZE` is defined twice, which causes a redefinition error. To prevent this, _include guards_ are used in header files to ensure that their contents are only included once. Include guards are consist of three lines:

```c
#ifndef HEADER_FILENAME_H
#define HEADER_FILENAME_H
// ... contents of the header
#endif
```

When the header file is included for the first time, the macro `HEADER_FILENAME_H` is not defined, so the contents of the header are included and the macro is defined. On subsequent inclusions, the macro is already defined, so the contents are skipped.

> In conventional C programming, the macro name used in include guards is usually the uppercase version of the header file name with non-alphanumeric characters replaced by underscores (`_`).

Modern C compilers also support `#pragma once` directive to achieve the same effect as include guards:

```c
#pragma once
// ... contents of the header
```

If you unsure whether your compiler supports `#pragma once`, it's Ok to use both traditional include guards and `#pragma once` together for maximum compatibility.

`[ANCPP RESTRICTION]`: A header file only to be included once in ANCPP even if it does not have include guards or `#pragma once`.

**`__has_include(...)` operator**

The `__has_include(...)` operator in conditional expressions checks whether a specified file exists. It expands to 1 if the file exists, otherwise it expands to 0.

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

`[ANCPP RESTRICTION]`: ANCPP always does not supported escape sequences in `__has_include("FILE_PATH")`, though C23 standard allows it and reinterpret the FILE_PATH as regular string literal when the specified file does not exist the first time searching.

`[ANCPP RESTRICTION]`: ANCPP does not support `__has_include(<MACRO>)` form (that is, an additional pair of angle brackets surrounding a macro), because it may cause confusion about the `__has_include(MACRO)` form, though C23 standard allows it.

### 3.8 Resource file inclusion (`#embed`)

Directive `#embed` is used to embed the contents of a resource file into the source code. It is commonly used to include binary data, images, or other non-code resources directly into the compiled program.

The usage of `#embed` is similar to `#include`, but the contents of resource file wouldn't be processed, it's embedded as byte array.

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

Note that the escape sequences are allowed in the file path when a string literal macro is used:

```c#
#define BINARY_FILE "data\\data.bin"
#embed BINARY_FILE              // Backslash is escaped in string literal
```

**Parameters of `#embed`**

Directive `embed` supports parameters:

- `limit(NUMBER)`: Limit the maximum number of bytes to embed from the file.
- `prefix(BYTE_SEQUENCE)`: A sequence of bytes to prepend to the embedded data. If the resource file is empty this prefix has no effect.
- `suffix(BYTE_SEQUENCE)`: A sequence of bytes to append to the embedded data. If the resource file is empty this suffix has no effect.
- `if_empty(BYTE_SEQUENCE)`: A sequence of bytes to use as the embedded data if the resource file is empty. The `prefix` and `suffix` are ignored in this case.

```c
const char message[] =
{
    #embed "message.txt" if_empty('H', 'e', 'l', 'l', 'o', '!', '\n')
    ,'\0' // Append null terminator to `message[]`
};

const uint8_t random_data[] =
{
    #embed "/dev/random.bin" limit(1024) \
        prefix(0xCA, 0xFE, 0x03, 0x05) \
        suffix(0xBE, 0xEF, 0x07, 0x09)
};
```

> These parameters can also be written as `__limit__(NUMBER)`, `__prefix__(BYTE_SEQUENCE)`, `__suffix__(BYTE_SEQUENCE)` and `__if_empty__(BYTE_SEQUENCE)`.

`[ANCPP RESTRICTION]`: The value of parameter `limit` must be a non-negative integer number, an expression even if it evaluates to a non-negative integer is not allowed.

```c
const uint8_t data[] =
{
    #embed "file.txt" limit(256 + 256)  // Disabled: Expression is not allowed.
    #embed "file.txt" limit(-100)       // Disabled: Negative number is not allowed.
    #embed "file.txt" limit("1024")     // Disabled: String literal is not allowed.
};
```

`[ANCPP RESTRICTION]`: Parameters `prefix`, `suffix`, and `if_empty` must be a character or integer number sequence separated by commas.

```c
#embed "file.txt" prefix(0xAA 0xBB 0xCC)    // Disabled: Commas are required
#embed "file.txt" prefix("foo", "bar")      // Disabled: String literals are not allowed
#embed "file.txt" prefix(int a = 123)       // Disabled: Only character or integer number are allowed
```

**`__has_embed(...)` operator**

Just like `__has_include(...)`, there is a `__has_embed(...)` operator in conditional expressions to check whether a specified resource file exists.

Syntax:

- `__has_embed("file")`
- `__has_embed(<file>)`
- `__has_embed(MACRO)`

There are three possible results:

- `__STDC_EMBED_NOT_FOUND__` (expands to 0): Indicates that the specified resource file does not exist.
- `__STDC_EMBED_FOUND__` (expands to 1): Indicates that the specified resource file exists.
- `__STDC_EMBED_EMPTY__` (expands to 2): Indicates file exists but is empty.

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

`[ANCPP RESTRICTION]`: C23 standard allows parameters in `__has_embed(...)`, and if one of the parameters passed is not supported by the implementation, it will expand to `__STDC_EMBED_NOT_FOUND__`. For simplicity, ANCPP does not support parameters in `__has_embed(...)`.

### 3.9 Error and warning (`#error`, `#warning`)

Shows the given error message and renders the program ill-formed, or given warning message without affect the validity of the program.

```c
#error "This is an error message."
#warning "This is a warning message."
```

After encountering the `#error` directive, an implementation displays the message diagnostic-message and renders the program ill-formed (the compilation stops).

The `#warning` directive is similar to `#error`, except that the validity of the program is not affected and the compilation continues.

### 3.10 Behavior control (`#pragma`)

The pragma directive controls implementation-specific behavior of the compiler, such as disabling compiler warnings or changing alignment requirements. Any pragma that is not recognized is ignored.

Currently, ANCPP only supports one pragma:

```c
#pragma once
```

This pragma is used to prevent multiple inclusions of the current source file, it is equivalent to traditional include guards.

## 4. Unsupported Directives

Non-standard and obsolete directives and operator are not supported.

```c
#assert SYSTEM(unix)                // Error: Obsolete directive
#unassert SYSTEM                    // Error: Obsolete directive
#ident "foo"                        // Error: Non-standard directive
#sccs "foo"                         // Error: Non-standard directive
#include_next <file.h>              // Error: Non-standard directive
#if __has_include_next("file.h")    // Error: Non-standard operator
```

The `#line` directive is not supported because it is rarely used in modern C programming.

```c
#line 100 "new_file.c"              // Error: Directive `#line` is not supported.
```

The null directives (`#` followed by a line break) are not allowed since it is likely a mistake in the source code.

```c
#                                   // Error: Null directive.
```

## 5. Unsupported Features

Trigraphs and alternative tokens (digraphs) are disabled because they affect code readability and are rarely used in modern C programming.

```c
%:include <stdio.h>                 // Disabled: Digraphs are disabled.
if true <% return 1; %>             // Disabled: Digraphs are disabled.
printf("%s\n", argv<:1:>);          // Disabled: Digraphs are disabled.

??=include <stdlib.h>               // Error: Trigraphs are removed in C23.
if true ??< return 1; ??>           // Error: Trigraphs are removed in C23.
printf("%s??/n", argv??(1??));      // Error: Trigraphs are removed in C23.
```

## 6. Token Types

ANCPP produces tokens including identifiers, numbers, string literals, character literals, and punctuators. The definitions of these token types are based on the C23 standard.

Directives, macros and conditionals are removed during preprocessing, so they are not part of the final token stream.

### 6.1 Identifiers

Besides C type (struct, union, enum) names, function names and variable names, the C keywords are also treated as identifiers.

An identifier is a sequence of letters (A-Z, a-z), digits (0-9), and underscores (_), starting with a letter or an underscore.

- `myVariable`
- `_temp123`
- `MAX_SIZE`

Identifiers can not start with a digit:

- `2ndValue`        // invalid
- `123_abc`         // invalid

Identifiers are case-sensitive, so `myVar`, `MyVar`, and `MYVAR` are considered different identifiers.

Unicode characters are also allowed in identifiers, including `\u{a0}` to `\u{d7ff}`, and `\u{e000}` to `\u{10ffff}`.

- `变量`
- `ヴァリアブル`
- `❤️`

Note that namespace separators (`::`) in attribute names are also included in identifiers.

### 6.2 String and Character Literals

Character literals are a single character enclosed in single quotes (`'`), while string literals are a sequence of characters enclosed in double quotes (`"`).

- `'A'`                 // character literal
- `"Hello, World!"`     // string literal

Both character and string literals can include escape sequences to represent special characters, supported escape sequences:

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
| `\xh...`        | Hexadecimal value, 1 or more digits, only two digits are supported by ANCCP. e.g., `\x41` (A), `\x0A` (newline) |
| `\uXXXX`        | Unicode code point (4 hex digits), e.g., `\u0041` (A), `\u03A9` (Ω) |
| `\UXXXXXXXX`    | Unicode code point (8 hex digits), e.g., `\U00000041` (A), `\U000003A9` (Ω) |

`\e` is not standard escape sequence, though it is supported by some compilers (e.g., GCC, Clang), ANCPP does not support it.

String and character literal prefixes are also supported:

| Prefix | Description                        |
|--------|------------------------------------|
| `L`    | Wide character or string literal   |
| `u`    | UTF-16 character or string literal |
| `U`    | UTF-32 character or string literal |
| `u8`   | UTF-8 character or string literal  |

- `L'A'`                // wide character literal
- `u8"Hello"`           // UTF-8 string literal

`[ANCPP RESTRICTION]`: Multicharacter constant is not supported. e.g., `'AB'`, `u'CD'` are not allowed.

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

Integer can have suffixes to indicate their width:

| Suffix       | Description       | Example           |
|--------------|-------------------|-------------------|
| `l` or `L`   | Long integer      | `123l`, `0b1010L` |
| `ll` or `LL` | Long long integer | `123ll`, `456LL`  |
| `wb` or `WB` | Wide integer      | `123wb`, `0XFFWB` |

Additionally, unsigned suffix (`u` or `U`) is also supported, it can be combined with other suffixes, e.g., `123ul`, `45ull`, `0XFFUWB`, or use alone to indicate unsigned integer, e.g., `123u`, `0b1010U`.

If a suffix is not specified, the type of the integer literal is determined based on its value, that is the smallest type that can hold the value is chosen.

**Floating-point literals**

Floating-point literals can be in decimal or hexadecimal format:

| Format      | Prefix       | Example                 |
|-------------|--------------|-------------------------|
| Decimal     | None         | `123.45`, `.667`, `1.414e10`, `1.732e+2`, `2.718e-3` |
| Hexadecimal | `0x` or `0X` | `0x1.23p4`, `0x4.5p+6`,  `0X7.5p-8` |

Floating-point literals can have suffixes to indicate their width:

| Suffix      | Description | Example                |
|-------------|-------------|------------------------|
| `f` or `F`  | Float       | `123.45f`, `0x1.23p4F` |
| `l` or `L`  | Double      | `123.45l`, `0X4.5P+6L` |

Additionally, the suffix `d` or `D` is also supported to indicate a decimal floating-point literal (`_Decimal{32|64|128}`), it can be combined with other suffixes, e.g.,`123.45df`, `0x1.23p4DL`, or use alone to indicate default precision (`_Decimal64`), e.g., `123.45dd`, `0.667DD`.

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
| `||`       | Logical OR               |
| `!`        | Logical NOT              |

**Bitwise Operators**

| Punctuator | Description          |
|------------|----------------------|
| `&`        | Bitwise AND          |
| `|`        | Bitwise OR           |
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
| `|=`       | Bitwise OR assignment     |
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

## 7. License

See the "LICENSE" and "LICENSE.additional" files in the root directory of this project for details.

## 8. References

- [The XiaoXuan C Compiler Project](https://github.com/hemashushu/xiaoxuan-c-compiler)
- [C Reference](https://en.cppreference.com/w/c/preprocessor.html)
- [GCC CPP Manual](https://gcc.gnu.org/onlinedocs/cpp/)
