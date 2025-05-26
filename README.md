# XiaoXuan C Preprocessor

**XiaoXuan C Preprocessor (ANCPP)** is a C preprocessor written in Rust. It is fully compliant with the C23 standard and supports all standard C preprocessor features, including macro expansion, conditional compilation, and file inclusion.

Unlike standalone tools such as GNU `cpp` or `gcc -E`, ANCPP is designed to be used as a library. It takes C source code as input and produces a stream of tokens, which can be consumed directly by a parser.
