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
        while self.peek_token(0).is_some() {
            let statement = self.parse_statement()?;
            statements.push(statement);
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

                // Collect tokens until we hit a directive or EOF.
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
                let definition = parse_definition(self);
                Define::ObjectLike { name, definition }
            }
            Some(Token::FunctionLikeMacroIdentifier(id)) => {
                // Handle function-like macro definition
                let name = id.clone();
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
            if !matches!(self.peek_token(0),Some(Token::Punctuator(Punctuator::Pound))) || // the char '#'
               !matches!(self.peek_token(1), Some(Token::Identifier(id)) if
               ["elif", "elifdef", "elifndef"].contains(&id.as_str()))
            {
                break;
            }
        }

        // If we hit an `#else`, we collect the alternative statements.
        if matches!(self.peek_token(0),Some(Token::Punctuator(Punctuator::Pound))) && // the char '#'
           matches!(self.peek_token(1), Some(Token::Identifier(id)) if id.as_str() == "else")
        {
            self.next_token(); // consumes '#'
            self.next_token(); // consumes "else"
            let alternative_statements = collect_consequence(self)?;
            alternative = Some(alternative_statements);
        }

        // Finally, we expect an `#endif` to close the `if` block.
        self.expect_and_consume_specified_identifier("endif")?;

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

    use crate::parser::parse_from_str;

    fn format(s: &str) -> String {
        // match parse_from_str(s) {
        //     Ok(program) => print_to_string(&program),
        //     Err(err) => panic!("{}", err.with_source(s)),
        // }
        todo!()
    }

    #[test]
    fn test_parse_code_statement() {
        // todo
    }

    //     //     #[test]
    //     //     fn test_parse_use_statement() {
    //     //         assert_eq!(format("use std::memory::copy"), "use std::memory::copy\n\n");
    //     //
    //     //         // test 'as'
    //     //         assert_eq!(
    //     //             format("use parent::sub_sub_module::some_data as other_data"),
    //     //             "use parent::sub_sub_module::some_data as other_data\n\n"
    //     //         );
    //     //
    //     //         // test multiple items
    //     //         assert_eq!(
    //     //             format(
    //     //                 "\
    //     // use module::sub_module::some_func
    //     // use self::sub_module::some_func"
    //     //             ),
    //     //             "\
    //     // use module::sub_module::some_func
    //     // use self::sub_module::some_func\n\n"
    //     //         );
    //     //
    //     //         // test line breaks
    //     //         assert_eq!(
    //     //             format(
    //     //                 "\
    //     // use
    //     // std::memory::copy
    //     // as
    //     // mem_copy"
    //     //             ),
    //     //             "use std::memory::copy as mem_copy\n\n"
    //     //         );
    //     //     }
    //
    //     #[test]
    //     fn test_parse_import_function_statement() {
    //         assert_eq!(
    //             format("import fn foo::bar()->()"),
    //             "import fn foo::bar() -> ()\n\n"
    //         );
    //
    //         // test omit return part
    //         assert_eq!(
    //             format("import fn foo::bar()"),
    //             "import fn foo::bar() -> ()\n\n"
    //         );
    //
    //         // test with params
    //         assert_eq!(
    //             format("import fn foo::add(i32,i32)->i32"),
    //             "import fn foo::add(i32, i32) -> i32\n\n"
    //         );
    //
    //         // test with params and results
    //         assert_eq!(
    //             format("import fn foo::div(i64)->(i32,i32)"),
    //             "import fn foo::div(i64) -> (i32, i32)\n\n"
    //         );
    //
    //         // test 'as'
    //         assert_eq!(
    //             format("import fn foo::add(i32,i32)->i32 as add_i32"),
    //             "import fn foo::add(i32, i32) -> i32 as add_i32\n\n"
    //         );
    //
    //         // test no results but has 'as'
    //         assert_eq!(
    //             format("import fn foo::bar() as baz"),
    //             "import fn foo::bar() -> () as baz\n\n"
    //         );
    //
    //         // test from
    //         assert_eq!(
    //             format("import fn foo::bar() from mymod"),
    //             "import fn foo::bar() -> () from mymod\n\n"
    //         );
    //
    //         // test multiple items
    //         assert_eq!(
    //             format(
    //                 "\
    // import fn foo::bar()
    // import fn foo::add(i32,i32)->i32"
    //             ),
    //             "\
    // import fn foo::bar() -> ()
    // import fn foo::add(i32, i32) -> i32\n\n"
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // import
    // fn
    // foo::add
    // (
    // i32
    // i32
    // )
    // ->
    // i32
    // as
    // add_i32
    // from
    // mymod"
    //             ),
    //             "import fn foo::add(i32, i32) -> i32 as add_i32 from mymod\n\n"
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_import_data_statement() {
    //         assert_eq!(
    //             format("import data foo::count type i32"),
    //             "import data foo::count type i32\n\n"
    //         );
    //
    //         assert_eq!(
    //             format("import readonly data foo::PI type f32"),
    //             "import readonly data foo::PI type f32\n\n"
    //         );
    //
    //         assert_eq!(
    //             format("import uninit data foo::table type byte[]"),
    //             "import uninit data foo::table type byte[]\n\n"
    //         );
    //
    //         // test 'as'
    //         assert_eq!(
    //             format("import data foo::bar type byte[] as baz"),
    //             "import data foo::bar type byte[] as baz\n\n"
    //         );
    //
    //         // test from
    //         assert_eq!(
    //             format("import data foo::count type i32 from mymod"),
    //             "import data foo::count type i32 from mymod\n\n"
    //         );
    //
    //         // test multiple items
    //         assert_eq!(
    //             format(
    //                 "\
    // import readonly data foo::PI type f32
    // import data foo::bar type byte[] as baz"
    //             ),
    //             "\
    // import readonly data foo::PI type f32
    // import data foo::bar type byte[] as baz\n\n"
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // import
    // readonly
    // data
    // foo::bar
    // type
    // byte
    // [
    // ]
    // as
    // baz
    // from
    // mymod"
    //             ),
    //             "import readonly data foo::bar type byte[] as baz from mymod\n\n"
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_external_function_statement() {
    //         assert_eq!(
    //             format("external fn libfoo::bar()->()"),
    //             "external fn libfoo::bar() -> ()\n\n"
    //         );
    //
    //         // test omit return part
    //         assert_eq!(
    //             format("external fn libfoo::bar()"),
    //             "external fn libfoo::bar() -> ()\n\n"
    //         );
    //
    //         // test with params
    //         assert_eq!(
    //             format("external fn libfoo::add(i32,i32)->i32"),
    //             "external fn libfoo::add(i32, i32) -> i32\n\n"
    //         );
    //
    //         // test 'as'
    //         assert_eq!(
    //             format("external fn libfoo::add(i32,i32)->i32 as add_i32"),
    //             "external fn libfoo::add(i32, i32) -> i32 as add_i32\n\n"
    //         );
    //
    //         // test 'as' without return
    //         assert_eq!(
    //             format("external fn libfoo::bar() as baz"),
    //             "external fn libfoo::bar() -> () as baz\n\n"
    //         );
    //
    //         // test multiple items
    //         assert_eq!(
    //             format(
    //                 "\
    // external fn libfoo::bar()
    // external fn libfoo::add(i32,i32)->i32"
    //             ),
    //             "\
    // external fn libfoo::bar() -> ()
    // external fn libfoo::add(i32, i32) -> i32\n\n"
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // external
    // fn
    // libfoo::add
    // (
    // i32
    // i32
    // )
    // ->
    // i32
    // as
    // add_i32"
    //             ),
    //             "external fn libfoo::add(i32, i32) -> i32 as add_i32\n\n"
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_external_data_statement() {
    //         assert_eq!(
    //             format("external data libfoo::PI type f32"),
    //             "external data libfoo::PI type f32\n\n"
    //         );
    //
    //         // test 'as'
    //         assert_eq!(
    //             format("external data libfoo::bar type byte[] as baz"),
    //             "external data libfoo::bar type byte[] as baz\n\n"
    //         );
    //
    //         // test multiple items
    //         assert_eq!(
    //             format(
    //                 "\
    // external data libfoo::PI type f32
    // external data libfoo::bar type byte[] as baz"
    //             ),
    //             "\
    // external data libfoo::PI type f32
    // external data libfoo::bar type byte[] as baz\n\n"
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // external
    // data
    // libfoo::bar
    // type
    // byte
    // [
    // ]
    // as
    // baz"
    //             ),
    //             "external data libfoo::bar type byte[] as baz\n\n"
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_data_statement() {
    //         assert_eq!(format("data foo:i32=11"), "data foo:i32 = 11\n\n");
    //
    //         // section 'readonly'
    //         assert_eq!(
    //             format("pub readonly data bar:i32=13"),
    //             "pub readonly data bar:i32 = 13\n\n"
    //         );
    //
    //         // section 'uninit'
    //         assert_eq!(
    //             format("pub uninit data baz:i32"),
    //             "pub uninit data baz:i32\n\n"
    //         );
    //
    //         // data type i64
    //         assert_eq!(format("data bar:i64=17_i64"), "data bar:i64 = 17_i64\n\n");
    //
    //         // other data types and values
    //         assert_eq!(
    //             format(
    //                 r#"
    // pub data foo1:byte[32] = h"11 13 17 19" // length is 32
    // pub data foo1:byte[32,align=8] = [0x11_i32, 0x13_i32, 0x17_i32, 0x19_i32] // length is 32
    // pub data foo2:byte[align=4] = [0x11_i32, 0x13_i32, 0x17_i32, 0x19_i32] // length is 4
    // pub data foo3:byte[] = "Hello, World!" // length is 13
    // pub data foo4:byte[] = "Hello, World!\0" // length is 13+1
    // pub data foo5:byte[] = ["Hello, World!", 0_i8] // length is 13+1""#
    //             ),
    //             "\
    // pub data foo1:byte[32] = h\"11 13 17 19\"
    // pub data foo1:byte[32, align=8] = [
    //     17
    //     19
    //     23
    //     25
    // ]
    // pub data foo2:byte[align=4] = [
    //     17
    //     19
    //     23
    //     25
    // ]
    // pub data foo3:byte[] = \"Hello, World!\"
    // pub data foo4:byte[] = \"Hello, World!\\0\"
    // pub data foo5:byte[] = [
    //     \"Hello, World!\"
    //     0_i8
    // ]
    //
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // pub
    // data
    // foo
    // :
    // byte
    // [
    // 32
    // align
    // =
    // 8
    // ]
    // =
    // [
    // 11
    // \"abc\"
    //     [
    //         13
    //         17
    //     ]
    // ]
    // "
    //             ),
    //             "\
    // pub data foo:byte[32, align=8] = [
    //     11
    //     \"abc\"
    //     [
    //         13
    //         17
    //     ]
    // ]\n\n"
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_function_statement() {
    //         assert_eq!(
    //             format("fn foo() nop()"),
    //             "\
    // fn foo() -> ()
    //     nop()
    // "
    //         );
    //
    //         // with params and return
    //         assert_eq!(
    //             format("fn foo(hi:i32,lo:i32)->i64 nop()"),
    //             "\
    // fn foo(hi:i32, lo:i32) -> i64
    //     nop()
    // "
    //         );
    //
    //         // with results
    //         assert_eq!(
    //             format("fn foo(n:i64)->(i32,i32) nop()"),
    //             "\
    // fn foo(n:i64) -> (i32, i32)
    //     nop()
    // "
    //         );
    //
    //         // with empty local variable
    //         assert_eq!(
    //             format("fn foo()->() [] nop()"),
    //             "\
    // fn foo() -> ()
    //     nop()
    // "
    //         );
    //
    //         // with local variable
    //         assert_eq!(
    //             format("fn foo() [a:i32] nop()"),
    //             "\
    // fn foo() -> ()
    //     [a:i32]
    //     nop()
    // "
    //         );
    //
    //         // with multiple local variables
    //         assert_eq!(
    //             format("fn foo() [a:i32,b:byte[16,align=4]] nop()"),
    //             "\
    // fn foo() -> ()
    //     [a:i32, b:byte[16, align=4]]
    //     nop()
    // "
    //         );
    //
    //         // with instruction expressions
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo(left:i32,right:i32)-> i32 []
    //     add_i32(
    //         local_load_i32s(left),
    //         local_load_i32s(right),
    //     )"
    //             ),
    //             "\
    // fn foo(left:i32, right:i32) -> i32
    //     add_i32(
    //         local_load_i32s(left),
    //         local_load_i32s(right))
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // pub
    // fn
    // foo
    // (
    // left
    // :
    // i32
    // right
    // :
    // i32
    // )
    // ->
    // i32
    // [
    // abc
    // :
    // i32
    // ]
    // imm_i32
    // (
    // 11
    // )"
    //             ),
    //             "\
    // pub fn foo(left:i32, right:i32) -> i32
    //     [abc:i32]
    //     imm_i32(11)
    // "
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_expression_group() {
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo() {
    //     imm_i32(11)
    //     imm_i32(31)
    // }"
    //             ),
    //             "\
    // fn foo() -> ()
    //     {
    //         imm_i32(11)
    //         imm_i32(31)
    //     }
    // "
    //         );
    //
    //         // nested group
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     {
    //         imm_i32(11)
    //         {
    //             imm_i32(31)
    //         }
    //     }"
    //             ),
    //             "\
    // fn foo() -> ()
    //     {
    //         imm_i32(11)
    //         {
    //             imm_i32(31)
    //         }
    //     }
    // "
    //         );
    //
    //         // test without line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo() {
    //     imm_i32(11) imm_i32(13) imm_i32(17)
    // }"
    //             ),
    //             "\
    // fn foo() -> ()
    //     {
    //         imm_i32(11)
    //         imm_i32(13)
    //         imm_i32(17)
    //     }
    // "
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_expression_when() {
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     when imm_i32(1)
    //         local_store_i32(imm_i32(11))"
    //             ),
    //             "\
    // fn foo() -> ()
    //     when
    //         imm_i32(1)
    //         local_store_i32(
    //             imm_i32(11))
    // "
    //         );
    //
    //         // with local variables
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     when [left:i32,right:i32] imm_i32(1) nop()
    // "
    //             ),
    //             "\
    // fn foo() -> ()
    //     when
    //         [left:i32, right:i32]
    //         imm_i32(1)
    //         nop()
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     when
    //     [
    //     left
    //     :
    //     i32
    //     right
    //     :
    //     i32
    //     ]
    //     imm_i32
    //     (
    //     1
    //     )
    //     nop
    //     (
    //     )
    // "
    //             ),
    //             "\
    // fn foo() -> ()
    //     when
    //         [left:i32, right:i32]
    //         imm_i32(1)
    //         nop()
    // "
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_expression_if() {
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     if
    //         eqz_i32(local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     if -> ()
    //         eqz_i32(
    //             local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    // "
    //         );
    //
    //         // with params and return values
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     if -> (i32,i32)
    //         eqz_i32(local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     if -> (i32, i32)
    //         eqz_i32(
    //             local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    // "
    //         );
    //
    //         // without params
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     if ->i32
    //         eqz_i32(local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     if -> i32
    //         eqz_i32(
    //             local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    // "
    //         );
    //
    //         // without params and return values
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     if eqz_i32(local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     if -> ()
    //         eqz_i32(
    //             local_load_i32_s(num))
    //         imm_i32(11)
    //         imm_i32(13)
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // fn
    // foo()
    // if
    // ->
    // (
    // i32
    // i32
    // )
    // imm_i32(11)
    // imm_i32(13)
    // imm_i32(17)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     if -> (i32, i32)
    //         imm_i32(11)
    //         imm_i32(13)
    //         imm_i32(17)
    // "
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_expression_block() {
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     block (num:i32=
    //         data_load_extend_i32_s(buffer, local_load_i32_s(offset))
    //         )
    //         imm_i32(11)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block (num:i32=data_load_extend_i32_s(buffer,
    //         local_load_i32_s(offset))) -> ()
    //         imm_i32(11)
    // "
    //         );
    //
    //         // with params and return values
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     block (left:i32=imm_i32(11), right:i32=imm_i32(13))->i32
    //         imm_i32(11)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block (left:i32=imm_i32(11), right:i32=imm_i32(13)) -> i32
    //         imm_i32(11)
    // "
    //         );
    //
    //         // without params
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     block ()->i32
    //         imm_i32(11)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block () -> i32
    //         imm_i32(11)
    // "
    //         );
    //
    //         // omits params and return values
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     block imm_i32(11)
    //         "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block () -> ()
    //         imm_i32(11)
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    // block
    // (
    // num
    // :
    // i32
    // =
    // imm_i32
    // (
    // 11
    // )
    // )
    // ->
    // i32
    // imm_i32
    // (
    // 13
    // )
    // "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block (num:i32=imm_i32(11)) -> i32
    //         imm_i32(13)
    // "
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_expression_break() {
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     block {
    //         break(imm_i32(11), imm_i32(13))
    //         break_fn(imm_i32(29))
    //     }"
    //             ),
    //             "\
    // fn foo() -> ()
    //     block () -> ()
    //         {
    //             break(
    //                 imm_i32(11)
    //                 imm_i32(13)
    //             )
    //             break_fn(
    //                 imm_i32(29)
    //             )
    //         }
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    // block
    // break
    // (
    // imm_i32
    // (
    // 17
    // )
    // imm_i32
    // (
    // 23
    // )
    // )
    //     "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block () -> ()
    //         break(
    //             imm_i32(17)
    //             imm_i32(23)
    //         )
    // "
    //         );
    //     }
    //
    //     #[test]
    //     fn test_parse_expression_recur() {
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    //     block {
    //         recur(imm_i32(11), imm_i32(13))
    //         recur_fn(imm_i32(29))
    //     }"
    //             ),
    //             "\
    // fn foo() -> ()
    //     block () -> ()
    //         {
    //             recur(
    //                 imm_i32(11)
    //                 imm_i32(13)
    //             )
    //             recur_fn(
    //                 imm_i32(29)
    //             )
    //         }
    // "
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // fn foo()
    // block
    // recur
    // (
    // imm_i32
    // (
    // 17
    // )
    // imm_i32
    // (
    // 23
    // )
    // )
    //     "
    //             ),
    //             "\
    // fn foo() -> ()
    //     block () -> ()
    //         recur(
    //             imm_i32(17)
    //             imm_i32(23)
    //         )
    // "
    //         );
    //     }
}
