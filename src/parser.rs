// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    PreprocessError,
    ast::{Branch, Condition, Define, Embed, If, Include, Pragma, Program, Statement},
    lexer::lex_from_str,
    peekableiter::PeekableIter,
    range::Range,
    token::{Number, Punctuator, StringType, Token, TokenWithRange},
};

pub const PEEK_BUFFER_LENGTH_MERGE_STRINGS: usize = 2;
pub const PEEK_BUFFER_LENGTH_PARSER: usize = 4;

pub fn parse_from_str(source_code: &str) -> Result<Program, PreprocessError> {
    let tokens = lex_from_str(source_code)?;
    let mut token_iter = tokens.into_iter();
    let mut peekable_token_iter = PeekableIter::new(&mut token_iter, PEEK_BUFFER_LENGTH_PARSER);
    let mut parser = Parser::new(&mut peekable_token_iter);
    parser.parse_program()
}

pub struct Parser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithRange>,
    last_range: Range,
}

// Implementation of the Parser
//
// see:
// - https://en.cppreference.com/w/c/preprocessor.html
// - https://gcc.gnu.org/onlinedocs/cpp/
impl<'a> Parser<'a> {
    fn new(upstream: &'a mut PeekableIter<'a, TokenWithRange>) -> Self {
        let last_range = if let Some(first_token) = upstream.peek(0) {
            // Initialize last_range with the first token's range.
            first_token.range.clone()
        } else {
            Range::from_detail_and_length(0, 0, 0, 0)
        };

        Self {
            upstream,
            last_range,
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithRange { token, range }) => {
                self.last_range = range;
                Some(token)
            }
            None => None,
        }
    }

    fn peek_range(&self, offset: usize) -> Option<&Range> {
        match self.upstream.peek(offset) {
            Some(TokenWithRange { range, .. }) => Some(range),
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithRange { token, .. }) => Some(token),
            None => None,
        }
    }

    fn peek_token_and_equals(&self, offset: usize, expected_token: &Token) -> bool {
        matches!(
            self.peek_token(offset),
            Some(token) if token == expected_token)
    }

    // fn peek_identifier_and_equals_any_of(
    //     &self,
    //     offset: usize,
    //     expected_identifiers: &[&str],
    // ) -> Option<&str> {
    //     match self.peek_token(offset) {
    //         Some(Token::Identifier(id)) if expected_identifiers.contains(&id.as_str()) => Some(id),
    //         _ => None,
    //     }
    // }

    fn expect_and_consume_identifier(&mut self) -> Result<String, PreprocessError> {
        match self.next_token() {
            Some(Token::Identifier(id)) => Ok(id),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect an identifier.".to_owned(),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(
                "Expect an identifier.".to_owned(),
            )),
        }
    }

    fn expect_and_consume_specified_identifier(
        &mut self,
        identifier: &str,
    ) -> Result<String, PreprocessError> {
        match self.next_token() {
            Some(Token::Identifier(id)) if id == identifier => Ok(id),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                format!("Expect identifier \"{}\".", identifier),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(format!(
                "Expect identifier \"{}\".",
                identifier
            ))),
        }
    }

    fn expect_and_consume_string(&mut self) -> Result<(String, StringType), PreprocessError> {
        match self.next_token() {
            Some(Token::String(s, t)) => Ok((s, t)),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect a string.".to_owned(),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(
                "Expect a string.".to_owned(),
            )),
        }
    }

    fn expect_and_consume_number(&mut self) -> Result<Number, PreprocessError> {
        match self.next_token() {
            Some(Token::Number(n)) => Ok(n),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect a number.".to_owned(),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(
                "Expect a number.".to_owned(),
            )),
        }
    }

    fn expect_and_consume_newline_or_eof(&mut self) -> Result<(), PreprocessError> {
        match self.next_token() {
            Some(Token::Newline) => Ok(()),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect a newline.".to_owned(),
                self.last_range.start,
            )),
            None => Ok(()), // EOF is acceptable here
        }
    }

    fn expect_and_consume_token(
        &mut self,
        expected_token: &Token,
        token_description: &str,
    ) -> Result<(), PreprocessError> {
        match self.next_token() {
            Some(token) => {
                if &token == expected_token {
                    Ok(())
                } else {
                    Err(PreprocessError::MessageWithPosition(
                        format!("Expect token: {}.", token_description),
                        self.last_range.start,
                    ))
                }
            }
            None => Err(PreprocessError::UnexpectedEndOfDocument(format!(
                "Expect token: {}.",
                token_description
            ))),
        }
    }

    // expects open parenthesis '(' and consumes it.
    fn expect_and_consume_opening_paren(&mut self) -> Result<(), PreprocessError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    // expects close parenthesis ')' and consumes it.
    fn expect_and_consume_closing_paren(&mut self) -> Result<(), PreprocessError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisClose),
            "closing parenthesis",
        )
    }
    //     // '['
    //     fn consume_left_bracket(&mut self) -> Result<(), PreprocessError> {
    //         self.expect_and_consume_token(&Token::LeftBracket, "left bracket")
    //     }
    //
    //     // ']'
    //     fn consume_right_bracket(&mut self) -> Result<(), PreprocessError> {
    //         self.expect_and_consume_token(&Token::RightBracket, "right bracket")
    //     }
    //
    //     // '}'
    //     fn consume_right_brace(&mut self) -> Result<(), PreprocessError> {
    //         self.expect_and_consume_token(&Token::RightBrace, "right brace")
    //     }
    //
    //     // '='
    //     fn consume_equal(&mut self) -> Result<(), PreprocessError> {
    //         self.expect_and_consume_token(&Token::Equal, "equal sign")
    //     }
    //
    //     // ':'
    //     fn consume_colon(&mut self) -> Result<(), PreprocessError> {
    //         self.expect_and_consume_token(&Token::Colon, "colon sign")
    //     }
}

impl Parser<'_> {
    pub fn parse_program(&mut self) -> Result<Program, PreprocessError> {
        let mut statements: Vec<Statement> = vec![];
        while let Some(token) = self.peek_token(0) {
            match token {
                Token::Newline => {
                    self.next_token(); // consume newline
                    continue; // skip empty lines
                }
                _ => {
                    let statement = self.parse_statement()?;
                    statements.push(statement);
                }
            }
        }

        let program = Program { statements };

        Ok(program)
    }

    fn parse_statement(&mut self) -> Result<Statement, PreprocessError> {
        // ```diagram
        // token ...
        // ^
        // |__ current token, validated (not NONE)
        // ```

        let statement = match self.peek_token(0).unwrap() {
            Token::Punctuator(Punctuator::Pound) => {
                self.next_token(); // consume '#'

                if let Some(token) = self.peek_token(0) {
                    match token {
                        Token::Identifier(name) => {
                            match name.as_str() {
                                "pragma" => {
                                    // Handle `pragma` directive.
                                    let pragma = self.parse_pragma()?;
                                    Statement::Pragma(pragma)
                                }
                                "define" => {
                                    // Handle `define` directive.
                                    let define = self.parse_define()?;
                                    Statement::Define(define)
                                }
                                "undef" => {
                                    // Handle `undef` directive.
                                    self.next_token(); // consume 'undef'
                                    let identifier = self.expect_and_consume_identifier()?;
                                    let range = self.last_range;
                                    self.expect_and_consume_newline_or_eof()?;

                                    Statement::Undef(identifier, range)
                                }
                                "include" => {
                                    // Handle `include` directive.
                                    let include = self.parse_include()?;
                                    Statement::Include(include)
                                }
                                "embed" => {
                                    // Handle `embed` directive.
                                    let embed = self.parse_embed()?;
                                    Statement::Embed(embed)
                                }
                                "if" | "ifdef" | "ifndef" => {
                                    // Handle `if` directive.
                                    let if_ = self.parse_if()?;
                                    Statement::If(if_)
                                }
                                "elif" | "elifdef" | "elifndef" | "else" | "endif" => {
                                    return Err(PreprocessError::MessageWithRange(
                                        format!(
                                            "Missing `if` (or `ifdef`, `ifndef`) directive before `{}`.",
                                            name
                                        ),
                                        *self.peek_range(0).unwrap(),
                                    ));
                                }
                                "error" => {
                                    // Handle error directive.
                                    self.next_token(); // consumes 'error'
                                    let (error_message, _) = self.expect_and_consume_string()?;
                                    self.expect_and_consume_newline_or_eof()?;

                                    Statement::Error(error_message)
                                }
                                "warning" => {
                                    // Handle warning directive.
                                    self.next_token(); // consumes 'warning'
                                    let (warning_message, _) = self.expect_and_consume_string()?;
                                    self.expect_and_consume_newline_or_eof()?;

                                    Statement::Warning(warning_message)
                                }

                                "line" | "ident" | "sccs" | "assert" | "unassert"
                                | "include_next" => {
                                    return Err(PreprocessError::MessageWithRange(
                                        format!("The `{}` directive is not supported.", name),
                                        *self.peek_range(0).unwrap(),
                                    ));
                                }
                                _ => {
                                    return Err(PreprocessError::MessageWithRange(
                                        format!("Invalid preprocessor directive: `#{}`.", name),
                                        *self.peek_range(0).unwrap(),
                                    ));
                                }
                            }
                        }
                        _ => {
                            return Err(PreprocessError::MessageWithPosition(
                                "Expect a preprocessor directive after `#`.".to_owned(),
                                self.peek_range(0).unwrap().start,
                            ));
                        }
                    }
                } else {
                    // If there's no token after `#`, it's an null directive,
                    // while it is valid in syntax, it is not allowed in ANCPP
                    // because it's likely a coding mistake.
                    return Err(PreprocessError::MessageWithPosition(
                        "NULL preprocessor directive is not allowed, expected a directive after `#`.".to_owned(),
                        self.peek_range(0).unwrap().start,
                    ));
                }
            }
            _ => {
                // Handle regular C code
                let mut code = vec![];

                // Collect tokens until we hit a '#' or EOF.
                while let Some(token) = self.peek_token(0) {
                    match token {
                        Token::Newline => {
                            self.next_token(); // Consumes newline
                        }
                        Token::Punctuator(Punctuator::Pound) => {
                            // If we hit a `#`, we stop collecting code tokens.
                            break;
                        }
                        _ => {
                            code.push(self.upstream.next().unwrap());
                        }
                    }
                }

                Statement::Code(code)
            }
        };

        Ok(statement)
    }

    fn parse_pragma(&mut self) -> Result<Pragma, PreprocessError> {
        // ```diagram
        // pragma ... \n //
        // ^          ^__// to here
        // |_____________// current token, validated
        // ```

        self.next_token(); // consumes "pragma"

        let mut parts = vec![];

        // Move all tokens to `parts` until we hit a newline or EOF.
        while let Some(token) = self.peek_token(0) {
            match token {
                Token::Newline => {
                    break;
                }
                _ => {
                    let token_with_range = self.upstream.next().unwrap();
                    parts.push(token_with_range);
                }
            }
        }

        // Empty pragma parameters are not allowed.
        if parts.is_empty() {
            if self.peek_range(0).is_some() {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect pragma parameters after `#pragma`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            } else {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expect pragma parameters after `#pragma`.".to_owned(),
                ));
            }
        }

        self.expect_and_consume_newline_or_eof()?;

        Ok(Pragma { parts })
    }

    fn parse_define(&mut self) -> Result<Define, PreprocessError> {
        // ```diagram
        // define NAME DEFINITION \n //
        // ^                      ^__// to here
        // |_________________________// current token, validated
        //
        // variants:
        // - define NAME \n
        // - define NAME(PARAMETERS) DEFINITION \n
        // ```

        self.next_token(); // consumes "define"

        let parse_parameters = |parser: &mut Parser| -> Result<Vec<String>, PreprocessError> {
            let mut parameters = vec![];

            parser.expect_and_consume_opening_paren()?; // consumes '('

            while let Some(token) = parser.peek_token(0) {
                match token {
                    Token::Identifier(id) => {
                        parameters.push(id.clone());
                        parser.next_token(); // consume identifier
                    }
                    Token::Punctuator(Punctuator::Ellipsis) => {
                        parameters.push("...".to_owned());
                        parser.next_token(); // consume '...'
                    }
                    Token::Punctuator(Punctuator::ParenthesisClose) => {
                        break;
                    }
                    _ => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Expect an identifier in macro parameters.".to_owned(),
                            parser.peek_range(0).unwrap().start,
                        ));
                    }
                }

                if !parser.peek_token_and_equals(0, &Token::Punctuator(Punctuator::Comma)) {
                    break;
                }

                parser.next_token(); // consumes comma

                // If we have a comma, we expect another identifier next.
                if parser.peek_token_and_equals(0, &Token::Punctuator(Punctuator::ParenthesisClose))
                {
                    return Err(PreprocessError::MessageWithPosition(
                        "Expect an identifier after comma in macro parameters.".to_owned(),
                        parser.peek_range(0).unwrap().start,
                    ));
                }
            }

            parser.expect_and_consume_closing_paren()?; // consumes ')'

            Ok(parameters)
        };

        let parse_definition = |parser: &mut Parser| -> Vec<TokenWithRange> {
            // Move all tokens to `definition` until we hit a newline or EOF.
            // Note that empty definitions are allowed, e.g., `#define FOO`.
            let mut definition = vec![];
            while let Some(token) = parser.peek_token(0) {
                match token {
                    Token::Newline => {
                        break;
                    }
                    _ => {
                        let token_with_range = parser.upstream.next().unwrap();
                        definition.push(token_with_range);
                    }
                }
            }

            definition
        };

        let define = match self.peek_token(0) {
            Some(Token::Identifier(id)) => {
                // Handle object-like macro definition
                let name = id.clone();
                self.next_token(); // consume the identifier token

                let definition = parse_definition(self);
                Define::ObjectLike { name, definition }
            }
            Some(Token::FunctionLikeMacroIdentifier(id)) => {
                // Handle function-like macro definition
                let name = id.clone();
                self.next_token(); // consume the identifier token

                let parameters = parse_parameters(self)?;
                let definition = parse_definition(self);
                Define::FunctionLike {
                    name,
                    parameters,
                    definition,
                }
            }
            _ => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect an identifier after directive `#define`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            }
        };

        self.expect_and_consume_newline_or_eof()?;

        Ok(define)
    }

    fn parse_include(&mut self) -> Result<Include, PreprocessError> {
        // ```diagram
        // include "FILE_PATH" \n //
        // ^                   ^__// to here
        // |______________________// current token, validated
        //
        // variants:
        // - include <FILE_PATH> \n
        // - include MACRO_NAME \n
        // ```
        self.next_token(); // consumes "include"

        let include = match self.peek_token(0) {
            Some(Token::FilePath(file_path, is_system_header)) => {
                // Handle include with file path
                let range = *self.peek_range(0).unwrap();
                let include = Include::FilePath {
                    file_path: (file_path.to_owned(), range),
                    is_system_header: *is_system_header,
                };

                self.next_token(); // consume the file path token

                include
            }
            Some(Token::Identifier(identifier)) => {
                // Handle include with macro name
                let range = *self.peek_range(0).unwrap();
                let include = Include::Identifier(identifier.to_owned(), range);

                self.next_token(); // consume the identifier token

                include
            }
            Some(_) => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect a file path or macro name after directive `#include`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            }
            None => {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expect a file path or macro name after directive `#include`.".to_owned(),
                ));
            }
        };

        self.expect_and_consume_newline_or_eof()?;

        Ok(include)
    }

    fn parse_embed(&mut self) -> Result<Embed, PreprocessError> {
        // ```diagram
        // embed "FILE_PATH" OPTIONAL_PARAMETERS \n //
        // ^                                     ^__// to here
        // |_______________________________________-// current token, validated
        //
        // Variants:
        // - embed <FILE_PATH> OPTIONAL_PARAMETERS \n
        // - embed MACRO_NAME OPTIONAL_PARAMETERS \n
        //
        //
        // ```
        self.next_token(); // consumes "embed"

        let collect_balanced_tokens =
            |parser: &mut Parser| -> Result<Vec<TokenWithRange>, PreprocessError> {
                // for collecting balanced brackets
                // e.g., `(...)`, `[...]`, `{...}`, etc.
                // when this stack is empty, it means we are in a balanced state.
                let mut bracket_stack: Vec<Punctuator> = vec![];

                let mut tokens = vec![];
                while let Some(token) = parser.peek_token(0) {
                    match token {
                        Token::Punctuator(Punctuator::ParenthesisClose)
                            if bracket_stack.is_empty() =>
                        {
                            break;
                        }
                        Token::Punctuator(
                            opening @ (Punctuator::ParenthesisOpen
                            | Punctuator::BraceOpen
                            | Punctuator::BracketOpen),
                        ) => {
                            // If we encounter an opening bracket, we push it onto the stack.
                            bracket_stack.push(*opening);

                            let token_with_range = parser.upstream.next().unwrap();
                            tokens.push(token_with_range);
                        }
                        Token::Punctuator(Punctuator::ParenthesisClose) => {
                            if matches!(bracket_stack.last(), Some(Punctuator::ParenthesisOpen)) {
                                bracket_stack.pop();
                            } else {
                                return Err(PreprocessError::MessageWithRange(
                                    "Unmatched closing parenthesis.".to_owned(),
                                    *parser.peek_range(0).unwrap(),
                                ));
                            }

                            let token_with_range = parser.upstream.next().unwrap();
                            tokens.push(token_with_range);
                        }
                        Token::Punctuator(Punctuator::BraceClose) => {
                            if matches!(bracket_stack.last(), Some(Punctuator::BraceOpen)) {
                                bracket_stack.pop();
                            } else {
                                return Err(PreprocessError::MessageWithRange(
                                    "Unmatched closing brace.".to_owned(),
                                    *parser.peek_range(0).unwrap(),
                                ));
                            }

                            let token_with_range = parser.upstream.next().unwrap();
                            tokens.push(token_with_range);
                        }
                        Token::Punctuator(Punctuator::BracketClose) => {
                            if matches!(bracket_stack.last(), Some(Punctuator::BracketOpen)) {
                                bracket_stack.pop();
                            } else {
                                return Err(PreprocessError::MessageWithRange(
                                    "Unmatched closing bracket.".to_owned(),
                                    *parser.peek_range(0).unwrap(),
                                ));
                            }

                            let token_with_range = parser.upstream.next().unwrap();
                            tokens.push(token_with_range);
                        }
                        _ => {
                            let token_with_range = parser.upstream.next().unwrap();
                            tokens.push(token_with_range);
                        }
                    }
                }

                Ok(tokens)
            };

        let embed = match self.peek_token(0) {
            Some(Token::FilePath(file_path, is_system_header)) => {
                // Handle embed with file path

                let range = *self.peek_range(0).unwrap();
                let file_path_owned = file_path.to_owned();
                let is_system_header_owned = *is_system_header;

                self.next_token(); // consumes the file path token

                let mut limit: Option<usize> = None;
                let mut suffix: Vec<TokenWithRange> = vec![];
                let mut prefix: Vec<TokenWithRange> = vec![];
                let mut if_empty: Option<Vec<TokenWithRange>> = None;

                while let Some(Token::Identifier(param_name)) = self.peek_token(0) {
                    match param_name.as_str() {
                        "limit" => {
                            self.next_token(); // consumes "limit"
                            self.expect_and_consume_opening_paren()?;

                            match self.expect_and_consume_number()? {
                                Number::Integer(number) => {
                                    let number_value = number.as_u64().map_err(|_| {
                                        PreprocessError::MessageWithRange(
                                            "Invalid integer number.".to_owned(),
                                            self.last_range,
                                        )
                                    })?;

                                    limit = Some(number_value as usize);
                                }
                                _ => {
                                    return Err(PreprocessError::MessageWithRange(
                                        "Embed parameter `limit` must be an integer.".to_owned(),
                                        self.last_range,
                                    ));
                                }
                            }

                            self.expect_and_consume_closing_paren()?;
                        }
                        "prefix" => {
                            self.next_token(); // consumes "prefix"
                            self.expect_and_consume_opening_paren()?;
                            let tokens = collect_balanced_tokens(self)?;
                            prefix = tokens;
                            self.expect_and_consume_closing_paren()?;
                        }
                        "suffix" => {
                            self.next_token(); // consumes "suffix"
                            self.expect_and_consume_opening_paren()?;
                            let tokens = collect_balanced_tokens(self)?;
                            suffix = tokens;
                            self.expect_and_consume_closing_paren()?;
                        }
                        "if_empty" => {
                            self.next_token(); // consumes "if_empty"
                            self.expect_and_consume_opening_paren()?;
                            let tokens = collect_balanced_tokens(self)?;
                            if_empty = Some(tokens);
                            self.expect_and_consume_closing_paren()?;
                        }
                        "__limit__" | "__prefix__" | "__suffix__" | "__if_empty__" => {
                            let standard_param_name =
                                param_name.trim_start_matches("__").trim_end_matches("__");
                            return Err(PreprocessError::MessageWithRange(
                                format!(
                                    "Embed parameter `{}` is not supported, use `{}` instead.",
                                    param_name, standard_param_name
                                ),
                                *self.peek_range(0).unwrap(),
                            ));
                        }
                        _ => {
                            return Err(PreprocessError::MessageWithRange(
                                format!("Unsupported embed parameter: `{}`.", param_name),
                                *self.peek_range(0).unwrap(),
                            ));
                        }
                    }
                }

                self.expect_and_consume_newline_or_eof()?;
                let embed = Embed::FilePath {
                    file_path: (file_path_owned, range),
                    is_system_header: is_system_header_owned,
                    limit,
                    prefix,
                    suffix,
                    if_empty,
                };

                embed
            }
            Some(Token::Identifier(identifier)) => {
                // Handle embed with macro name
                let range = *self.peek_range(0).unwrap();
                let embed = Embed::Identifier(identifier.to_owned(), range);

                self.next_token(); // consumes the identifier token

                embed
            }
            Some(_) => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect a file path or macro name after directive `#embed`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            }
            None => {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expect a file path or macro name after directive `#embed`.".to_owned(),
                ));
            }
        };

        self.expect_and_consume_newline_or_eof()?;

        Ok(embed)
    }

    fn parse_if(&mut self) -> Result<If, PreprocessError> {
        // ```diagram
        // if CONDITION \n
        // ^            ^__// to here
        // |________________// current token, validated
        //
        // variants:
        // - if CONDITION \n
        // - ifdef IDENTIFIER \n
        // - ifndef IDENTIFIER \n
        // - elif CONDITION \n
        // - elifdef IDENTIFIER \n
        // - elifndef IDENTIFIER \n
        // ```

        let mut branches: Vec<Branch> = vec![];
        let mut alternative: Option<Vec<Statement>> = None;

        let collect_expression =
            |parser: &mut Parser| -> Result<Vec<TokenWithRange>, PreprocessError> {
                let mut expression_tokens = vec![];

                // Collect tokens until we hit a newline or EOF.
                while let Some(token) = parser.peek_token(0) {
                    match token {
                        Token::Newline => {
                            break;
                        }
                        _ => {
                            let token_with_range = parser.upstream.next().unwrap();
                            expression_tokens.push(token_with_range);
                        }
                    }
                }

                Ok(expression_tokens)
            };

        let collect_consequence = |parser: &mut Parser| -> Result<Vec<Statement>, PreprocessError> {
            let mut consequence = vec![];

            // Collect statements until we hit an `#else`, `#elif`, `#elifdef`, `#elifndef` or `#endif`.
            while parser.peek_token(0).is_some() {
                if matches!(parser.peek_token(0), Some(Token::Punctuator(Punctuator::Pound))) && // the char '#'
                   matches!(parser.peek_token(1), Some(Token::Identifier(id)) if
                   ["else", "elif", "elifdef", "elifndef", "endif"].contains(&id.as_str()))
                {
                    break;
                }

                let statement = parser.parse_statement()?;
                consequence.push(statement);
            }

            Ok(consequence)
        };

        // Consumes "if" ("ifdef", "ifndef") in the first iteration,
        // and "elif" ("elifdef", "elifndef") in the subsequent iterations.
        while let Some(Token::Identifier(if_type)) = self.next_token() {
            match if_type.as_str() {
                "if" | "elif" => {
                    // Handle `if` or `elif` directive
                    let expression = collect_expression(self)?;
                    self.expect_and_consume_newline_or_eof()?;

                    let condition = Condition::Expression(expression);
                    let consequence = collect_consequence(self)?;
                    branches.push(Branch {
                        condition,
                        consequence,
                    });
                }
                "ifdef" | "elifdef" => {
                    // Handle `ifdef` or `elifdef` directive
                    let identifier = self.expect_and_consume_identifier()?;
                    self.expect_and_consume_newline_or_eof()?;

                    let condition = Condition::Defined(identifier, self.last_range);
                    let consequence = collect_consequence(self)?;
                    branches.push(Branch {
                        condition,
                        consequence,
                    });
                }
                "ifndef" | "elifndef" => {
                    // Handle `ifndef` or `elifndef` directive
                    let identifier = self.expect_and_consume_identifier()?;
                    self.expect_and_consume_newline_or_eof()?;

                    let condition = Condition::NotDefined(identifier, self.last_range);
                    let consequence = collect_consequence(self)?;
                    branches.push(Branch {
                        condition,
                        consequence,
                    });
                }
                _ => {
                    unreachable!()
                }
            }

            // Continue to the next iteration if we encounter
            // `#elif`, `#elifdef` or `#elifndef` directives.
            if matches!(self.peek_token(0),Some(Token::Punctuator(Punctuator::Pound))) && // the char '#'
               matches!(self.peek_token(1), Some(Token::Identifier(id)) if
               ["elif", "elifdef", "elifndef"].contains(&id.as_str()))
            {
                // it is directive `elif`, `elifdef`, or `elifndef`,
                // continue to the next iteration.
                self.next_token(); // Consumes '#'
            } else {
                break;
            }
        }

        // If we hit an `#else`, we collect the alternative statements.
        if matches!(self.peek_token(0),Some(Token::Punctuator(Punctuator::Pound))) && // the char '#'
           matches!(self.peek_token(1), Some(Token::Identifier(id)) if id.as_str() == "else")
        {
            self.next_token(); // consumes '#'
            self.next_token(); // consumes "else"
            self.next_token(); // consumes newline
            let alternative_statements = collect_consequence(self)?;
            alternative = Some(alternative_statements);
        }

        // Finally, we expect an `#endif` to close the `if` block.
        self.expect_and_consume_token(&Token::Punctuator(Punctuator::Pound), "#")?;
        self.expect_and_consume_specified_identifier("endif")?;
        self.expect_and_consume_newline_or_eof()?;

        let if_ = If {
            branches,
            alternative,
        };

        Ok(if_)
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{ast_printer::print_to_string, parser::parse_from_str};

    fn format(s: &str) -> String {
        match parse_from_str(s) {
            Ok(program) => print_to_string(&program),
            Err(err) => panic!("{}", err.with_source(s)),
        }
    }

    #[test]
    fn test_parse_pragma() {
        assert_eq!(format("#pragma once"), "#pragma once\n");

        assert_eq!(
            format("#pragma STDC FENV_ACCESS ON"),
            "#pragma STDC FENV_ACCESS ON\n"
        );

        assert_eq!(
            format("#pragma STDC FP_CONTRACT OFF"),
            "#pragma STDC FP_CONTRACT OFF\n"
        );

        assert_eq!(
            format("#pragma STDC CX_LIMITED_RANGE DEFAULT"),
            "#pragma STDC CX_LIMITED_RANGE DEFAULT\n"
        );
    }

    #[test]
    fn test_parse_define() {
        assert_eq!(format("#define MAX 100"), "#define MAX 100\n");

        assert_eq!(format("#define EMPTY"), "#define EMPTY\n");

        assert_eq!(
            format("#define SQUARE(x,y) ((x)*(y))"),
            "#define SQUARE(x, y) ( ( x ) * ( y ) )\n"
        );

        // Test `#`
        assert_eq!(
            format("#define PRINT(var) (printf(\"%s = %d\", #var, var))"),
            "#define PRINT(var) ( printf ( \"%s = %d\" , # var , var ) )\n"
        );

        // Test `##`
        assert_eq!(
            format("#define ARRAY(prefix, len) int[len] prefix ## len;"),
            "#define ARRAY(prefix, len) int [ len ] prefix ## len ;\n"
        );

        // Test variadic macros
        assert_eq!(
            format("#define showlist(...) puts(__VA_ARGS__)"),
            "#define showlist(...) puts ( __VA_ARGS__ )\n"
        );
    }

    #[test]
    fn test_parse_undef() {
        assert_eq!(format("#undef FOO"), "#undef FOO\n");
    }

    #[test]
    fn test_parse_include() {
        assert_eq!(format("#include HEADER"), "#include HEADER\n");

        assert_eq!(format("#include <stdio.h>"), "#include <stdio.h>\n");

        assert_eq!(
            format("#include \"my_header.h\""),
            "#include \"my_header.h\"\n"
        );
    }

    #[test]
    fn test_parse_embed() {
        assert_eq!(format("#embed RESOURCE"), "#embed RESOURCE\n");

        assert_eq!(format("#embed <data.bin>"), "#embed <data.bin>\n");

        assert_eq!(format("#embed \"hippo.png\""), "#embed \"hippo.png\"\n");

        assert_eq!(
            format(
                "#embed </dev/random> limit(100) prefix('a','b') suffix('c','\0') if_empty('x','y', 'z')"
            ),
            "#embed </dev/random> limit(100) prefix('a' , 'b') suffix('c' , '\\0') if_empty('x' , 'y' , 'z')\n"
        );
    }

    #[test]
    fn test_parse_if() {
        assert_eq!(
            format(
                "\
#if EDITION==2025
    int x;
#endif"
            ),
            "\
#if EDITION == 2025
    int x ;
#endif\n"
        );

        assert_eq!(
            format(
                "\
#ifdef IDENTIFIER
    int x;
#else
    int y;
#endif"
            ),
            "\
#ifdef IDENTIFIER
    int x ;
#else
    int y ;
#endif\n"
        );

        assert_eq!(
            format(
                "\
#if defined ANCC
    a;
#elif EDITION==2025
    b;
#elifdef FOO
    c;
#else
    d;
#endif"
            ),
            "\
#if defined ANCC
    a ;
#elif EDITION == 2025
    b ;
#elifdef FOO
    c ;
#else
    d ;
#endif\n"
        );

        // Test multiline consequence and empty alternative
        assert_eq!(
            format(
                "\
#ifdef FOO
    for(int i = 0; i < 10; i++) {
        printf(\"%d\\n\", i);
    }
#elifdef BAR
#else
#endif"
            ),
            "\
#ifdef FOO
    for ( int i = 0 ; i < 10 ; i ++ ) { printf ( \"%d\\n\" , i ) ; }
#elifdef BAR
#else
#endif\n"
        );

        // Test empty consequence and multiline alternative
        assert_eq!(
            format(
                "\
#ifdef FOO
#elifdef BAR
    for(int i = 0; i < 10; i++) {
        printf(\"%d\\n\", i);
    }
#else
    int x = 0;
    int y = 1;
#endif"
            ),
            "\
#ifdef FOO
#elifdef BAR
    for ( int i = 0 ; i < 10 ; i ++ ) { printf ( \"%d\\n\" , i ) ; }
#else
    int x = 0 ; int y = 1 ;
#endif\n"
        );

        // Test directive within consequence and alternative
        assert_eq!(
            format(
                "\
#ifdef FOO
    #include \"foo.h\"
    int x;
#elifdef BAR
    int y;
    #include \"bar.h\"
#else
    #define BAZ 42
#endif"
            ),
            "\
#ifdef FOO
    #include \"foo.h\"
    int x ;
#elifdef BAR
    int y ;
    #include \"bar.h\"
#else
    #define BAZ 42
#endif\n"
        );

        // Test nested if directives
        assert_eq!(
            format(
                "\
#ifdef FOO
    11
    #ifdef BAR
        13
    #elifdef BAZ
        17
    #else
        19
    #endif
#elif define BUZZ
    23
#else
    29
#endif"
            ),
            "\
#ifdef FOO
    11
    #ifdef BAR
        13
    #elifdef BAZ
        17
    #else
        19
    #endif
#elif define BUZZ
    23
#else
    29
#endif\n"
        );
    }

    #[test]
    fn test_parse_error() {
        assert_eq!(format("#error \"foo bar\""), "#error \"foo bar\"\n");
    }

    #[test]
    fn test_parse_warning() {
        assert_eq!(format("#warning \"foo bar\""), "#warning \"foo bar\"\n");
    }

    #[test]
    fn test_parse_code_statement() {
        // Test a simple code statement
        assert_eq!(format("int x = 42;"), "int x = 42 ;\n");

        // Test a code statement with multiple lines
        assert_eq!(
            format(
                "\
int main() {
    printf(\"Hello, World!\\n\");
    return 0;
}"
            ),
            "\
int main ( ) { printf ( \"Hello, World!\\n\" ) ; return 0 ; }\n"
        );
    }

    #[test]
    fn test_parse_program() {
        // Test a complete program with various directives
        let program = "\
#define FOO 42

#ifdef ANCC
    #include <std/io.h>
#else
    #include <stdio.h>
#endif

int main() {
    printf(\"Number is %d\\n\", FOO);
    return 0;
}
";
        assert_eq!(
            format(program),
            "\
#define FOO 42
#ifdef ANCC
    #include <std/io.h>
#else
    #include <stdio.h>
#endif
int main ( ) { printf ( \"Number is %d\\n\" , FOO ) ; return 0 ; }\n"
        );
    }

    // #[test]
    //     fn test_parse_error() {
    //         // Test invalid preprocessor directive
    //         assert!(format("#invalid_directive").contains("Invalid preprocessor directive: `#invalid_directive`."));
    //
    //         // Test missing identifier after `#define`
    //         assert!(format("#define").contains("Expect an identifier after directive `#define`."));
    //
    //         // Test missing file path or macro name after `#include`
    //         assert!(format("#include").contains("Expect a file path or macro name after directive `#include`."));
    //
    //         // Test missing file path or macro name after `#embed`
    //         assert!(format("#embed").contains("Expect a file path or macro name after directive `#embed`."));
    //
    //         // Test missing condition after `#if`
    //         assert!(format("#if").contains("Expect a condition after directive `#if`."));
    //     }
}
