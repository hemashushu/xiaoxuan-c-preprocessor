// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    ast::{
        AlternativeBranch, Branch, Condition, Define, FunctionParameter, If, Pragma, Program,
        Statement,
    },
    error::PreprocessError,
    lexer::lex_from_str,
    peekable_iter::PeekableIter,
    range::Range,
    token::{Punctuator, StringEncoding, Token, TokenWithRange},
};

pub fn parse_from_str(source_text: &str) -> Result<Program, PreprocessError> {
    let tokens = lex_from_str(source_text)?;
    let mut token_iter = tokens.into_iter();
    let mut peekable_token_iter = PeekableIter::new(&mut token_iter);
    let mut parser = Parser::new(&mut peekable_token_iter);
    parser.parse_program()
}

pub struct Parser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithRange>,

    /// Range of the last token consumed by `next_token` or `next_token_with_range`.
    pub last_range: Range,
}

// Implementation of the Parser
//
// see:
// - https://en.cppreference.com/w/c/preprocessor.html
// - https://gcc.gnu.org/onlinedocs/cpp/
impl<'a> Parser<'a> {
    fn new(upstream: &'a mut PeekableIter<'a, TokenWithRange>) -> Self {
        Self {
            upstream,
            last_range: Range::default(),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.next_token_with_range() {
            Some(TokenWithRange { token, range }) => {
                self.last_range = range;
                Some(token)
            }
            None => None,
        }
    }

    fn next_token_with_range(&mut self) -> Option<TokenWithRange> {
        match self.upstream.next() {
            Some(token_with_range) => {
                self.last_range = token_with_range.range;
                Some(token_with_range)
            }
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithRange { token, .. }) => Some(token),
            None => None,
        }
    }

    fn peek_token_with_range(&self, offset: usize) -> Option<&TokenWithRange> {
        self.upstream.peek(offset)
    }

    fn peek_range(&self, offset: usize) -> Option<&Range> {
        match self.upstream.peek(offset) {
            Some(TokenWithRange { range, .. }) => Some(range),
            None => None,
        }
    }

    // Compare the token at the given lookahead offset, returning false at EOF.
    fn peek_token_and_equals(&self, offset: usize, expected_token: &Token) -> bool {
        matches!(
            self.peek_token(offset),
            Some(token) if token == expected_token)
    }

    fn consume_identifier(&mut self) -> Result<String, PreprocessError> {
        match self.next_token() {
            Some(Token::Identifier(id)) => Ok(id),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expected an identifier.".to_owned(),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(
                "Expected an identifier.".to_owned(),
            )),
        }
    }

    /// Consumes the expected identifier, or returns an error.
    fn consume_identifier_and_assert(
        &mut self,
        identifier: &str,
    ) -> Result<String, PreprocessError> {
        match self.next_token() {
            Some(Token::Identifier(id)) if id == identifier => Ok(id),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                format!("Expected identifier \"{}\".", identifier),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(format!(
                "Expected identifier \"{}\".",
                identifier
            ))),
        }
    }

    fn consume_string(&mut self) -> Result<(String, StringEncoding), PreprocessError> {
        match self.next_token() {
            Some(Token::String(s, t)) => Ok((s, t)),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expected a string literal.".to_owned(),
                self.last_range.start,
            )),
            None => Err(PreprocessError::UnexpectedEndOfDocument(
                "Expected a string literal.".to_owned(),
            )),
        }
    }

    fn consume_directive_end_or_eof(&mut self) -> Result<(), PreprocessError> {
        match self.next_token() {
            Some(Token::_DirectiveEnd) => Ok(()),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expected a newline.".to_owned(),
                self.last_range.start,
            )),
            None => Ok(()), // EOF is acceptable here
        }
    }

    fn consume_directive_end(&mut self) -> Result<(), PreprocessError> {
        match self.next_token() {
            Some(Token::_DirectiveEnd) => Ok(()),
            _ => Err(PreprocessError::MessageWithPosition(
                "Expected a newline.".to_owned(),
                self.last_range.start,
            )),
        }
    }

    /// Consumes the expected token, or returns an error.
    fn consume_token_and_assert(
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
                        format!("Expected token: {}.", token_description),
                        self.last_range.start,
                    ))
                }
            }
            None => Err(PreprocessError::UnexpectedEndOfDocument(format!(
                "Expected token: {}.",
                token_description
            ))),
        }
    }

    fn consume_opening_parenthesis(&mut self) -> Result<(), PreprocessError> {
        self.consume_token_and_assert(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    fn consume_closing_parenthesis(&mut self) -> Result<(), PreprocessError> {
        self.consume_token_and_assert(
            &Token::Punctuator(Punctuator::ParenthesisClose),
            "closing parenthesis",
        )
    }
}

impl Parser<'_> {
    fn parse_program(&mut self) -> Result<Program, PreprocessError> {
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
        // |__ current token, validated (wouldn't be NONE)
        // ```

        let statement = match self.peek_token(0).unwrap() {
            Token::_DirectiveStart => {
                self.next_token(); // consume '#'

                if let Some(TokenWithRange {
                    token: current_token,
                    range: current_range,
                }) = self.peek_token_with_range(0)
                {
                    let directive_range = *current_range;
                    match current_token {
                        Token::Identifier(name) => {
                            match name.as_str() {
                                "define" => {
                                    // Handle `define` directive.
                                    let define = self.parse_define()?;
                                    Statement::Define {
                                        define,
                                        directive_range,
                                    }
                                }
                                "undef" => {
                                    // Handle `undef` directive.
                                    self.next_token(); // consume 'undef'
                                    let identifier = self.consume_identifier()?;
                                    let identifier_range = self.last_range;
                                    self.consume_directive_end_or_eof()?;

                                    Statement::Undef {
                                        identifier,
                                        directive_range,
                                        identifier_range,
                                    }
                                }
                                "include" => {
                                    // Handle `include` directive.
                                    let components = self.parse_include()?;
                                    Statement::Include {
                                        components,
                                        directive_range,
                                    }
                                }
                                "embed" => {
                                    // Handle `embed` directive.
                                    let components = self.parse_embed()?;
                                    Statement::Embed {
                                        components,
                                        directive_range,
                                    }
                                }
                                "if" | "ifdef" | "ifndef" => {
                                    // Handle `if` directive.
                                    let if_ = self.parse_if()?;
                                    Statement::If(if_)
                                }
                                "elif" | "elifdef" | "elifndef" | "else" | "endif" => {
                                    // Branches without preceding `if` are invalid.
                                    return Err(PreprocessError::MessageWithRange(
                                        format!(
                                            "Missing `#if` (or `#ifdef`, `#ifndef`) before `{}`.",
                                            name
                                        ),
                                        *self.peek_range(0).unwrap(),
                                    ));
                                }
                                "error" => {
                                    // Handle error directive.
                                    self.next_token(); // consumes 'error'
                                    let (message, _) = self.consume_string()?;
                                    let message_range = self.last_range;

                                    self.consume_directive_end_or_eof()?;

                                    Statement::Error {
                                        message,
                                        directive_range,
                                        message_range,
                                    }
                                }
                                "warning" => {
                                    // Handle warning directive.
                                    self.next_token(); // consumes 'warning'
                                    let (message, _) = self.consume_string()?;
                                    let message_range = self.last_range;

                                    self.consume_directive_end_or_eof()?;

                                    Statement::Warning {
                                        message,
                                        directive_range,
                                        message_range,
                                    }
                                }
                                "pragma" => {
                                    // Handle `pragma` directive.
                                    let pragma = self.parse_pragma()?;
                                    Statement::Pragma {
                                        pragma,
                                        directive_range,
                                    }
                                }
                                "line" | "ident" | "sccs" | "assert" | "unassert"
                                | "include_next" => {
                                    return Err(PreprocessError::MessageWithRange(
                                        format!("Directive `{}` is not supported.", name),
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
                                "Expected a preprocessor directive after `#`.".to_owned(),
                                self.peek_range(0).unwrap().start,
                            ));
                        }
                    }
                } else {
                    // Standard C permits null directives, but ANCPP rejects them.
                    return Err(PreprocessError::MessageWithPosition(
                        "Null preprocessor directives are not allowed; expected a directive after `#`.".to_owned(),
                        self.peek_range(0).unwrap().start,
                    ));
                }
            }
            _ => {
                // Handle regular C code
                let mut code = vec![];

                // Collect tokens until we hit a directive start ('#' sign) or EOF.
                while let Some(token) = self.peek_token(0) {
                    match token {
                        Token::_DirectiveStart => {
                            // If we hit a directive start ('#' sign), we stop collecting code tokens.
                            break;
                        }
                        _ => {
                            code.push(self.next_token_with_range().unwrap());
                        }
                    }
                }

                Statement::Code(code)
            }
        };

        Ok(statement)
    }

    fn parse_define(&mut self) -> Result<Define, PreprocessError> {
        // ```diagram
        // define NAME DEFINITION \n //
        // ^                      ^__// to here
        // |_________________________// current token, validated (wouldn't be NONE)
        //
        // variants:
        // - define NAME \n
        // - define NAME(PARAMETERS) DEFINITION \n
        // ```

        self.next_token(); // consumes "define"

        // Collect named parameters and a possible final `Variadic` entry for `...`.
        let parse_parameters =
            |parser: &mut Parser| -> Result<Vec<FunctionParameter>, PreprocessError> {
                let mut parameters = vec![];

                parser.consume_opening_parenthesis()?; // consumes '('

                while let Some(token) = parser.peek_token(0) {
                    match token {
                        Token::Identifier(id) => {
                            parameters.push(FunctionParameter::Identifier(id.clone()));
                            parser.next_token(); // consume identifier
                        }
                        Token::Punctuator(Punctuator::Ellipsis) => {
                            parameters.push(FunctionParameter::Variadic);
                            parser.next_token(); // consume '...'
                        }
                        Token::Punctuator(Punctuator::ParenthesisClose) => {
                            break;
                        }
                        _ => {
                            return Err(PreprocessError::MessageWithPosition(
                                "Expected an identifier in macro parameters.".to_owned(),
                                parser.peek_range(0).unwrap().start,
                            ));
                        }
                    }

                    if !parser.peek_token_and_equals(0, &Token::Punctuator(Punctuator::Comma)) {
                        break;
                    }

                    // An ellipsis must be the final parameter.
                    if parameters.last() == Some(&FunctionParameter::Variadic) {
                        break;
                    }

                    parser.next_token(); // consumes comma

                    // If we have a comma, we expect another identifier next.
                    if parser
                        .peek_token_and_equals(0, &Token::Punctuator(Punctuator::ParenthesisClose))
                    {
                        return Err(PreprocessError::MessageWithPosition(
                            "Expected an identifier after the comma in macro parameter list."
                                .to_owned(),
                            parser.peek_range(0).unwrap().start,
                        ));
                    }
                }

                parser.consume_closing_parenthesis()?; // consumes ')'

                Ok(parameters)
            };

        let parse_balanced_definition =
            |parser: &mut Parser| -> Result<Vec<TokenWithRange>, PreprocessError> {
                // Track balanced parentheses, brackets, and braces. Literals are
                // already complete tokens, so their quotes are not tracked here.
                let mut bracket_stack: Vec<Punctuator> = vec![];

                // Move all tokens to `tokens` until we hit a newline or EOF.
                // Note that empty definitions are valid, e.g., `#define FOO`.
                let mut tokens = vec![];
                loop {
                    if let Some(token) = parser.peek_token(0) {
                        match token {
                            Token::_DirectiveEnd => {
                                if bracket_stack.is_empty() {
                                    // The replacement is empty or all delimiters are balanced.
                                    break;
                                } else {
                                    return Err(PreprocessError::MessageWithPosition(
                                        "Unterminated or mismatched brackets or quotes in macro definition.".to_owned(),
                                        parser.peek_range(0).unwrap().start,
                                    ));
                                }
                            }
                            Token::Punctuator(
                                opening @ (Punctuator::ParenthesisOpen
                                | Punctuator::BraceOpen
                                | Punctuator::BracketOpen),
                            ) => {
                                // If we encounter an opening bracket, we push it onto the stack.
                                bracket_stack.push(*opening);

                                let token_with_range = parser.next_token_with_range().unwrap();
                                tokens.push(token_with_range);
                            }
                            Token::Punctuator(
                                closing @ (Punctuator::ParenthesisClose
                                | Punctuator::BraceClose
                                | Punctuator::BracketClose),
                            ) => {
                                // Check if the closing bracket matches the last opening bracket.
                                match bracket_stack.last() {
                                    Some(Punctuator::ParenthesisOpen)
                                        if closing == &Punctuator::ParenthesisClose =>
                                    {
                                        bracket_stack.pop();
                                    }
                                    Some(Punctuator::BraceOpen)
                                        if closing == &Punctuator::BraceClose =>
                                    {
                                        bracket_stack.pop();
                                    }
                                    Some(Punctuator::BracketOpen)
                                        if closing == &Punctuator::BracketClose =>
                                    {
                                        bracket_stack.pop();
                                    }
                                    _ => {
                                        return Err(PreprocessError::MessageWithPosition(
                                            "Unmatched closing bracket in token sequence."
                                                .to_owned(),
                                            parser.peek_range(0).unwrap().start,
                                        ));
                                    }
                                }

                                let token_with_range = parser.next_token_with_range().unwrap();
                                tokens.push(token_with_range);
                            }
                            _ => {
                                let token_with_range = parser.next_token_with_range().unwrap();
                                tokens.push(token_with_range);
                            }
                        }
                    } else if bracket_stack.is_empty() {
                        break; // stop collecting tokens
                    } else {
                        return Err(PreprocessError::UnexpectedEndOfDocument(
                            "Unbalanced brackets in macro definition.".to_owned(),
                        ));
                    }
                }

                Ok(tokens)
            };

        let define = match self.peek_token(0) {
            Some(Token::Identifier(id)) => {
                // Handle object-like macro definition
                let name = id.clone();
                let range = *self.peek_range(0).unwrap();
                self.next_token(); // consume the identifier token

                let definition = parse_balanced_definition(self)?;
                self.consume_directive_end_or_eof()?;

                Define::ObjectLike {
                    identifier: name,
                    identifier_range: range,
                    definition,
                }
            }
            Some(Token::_FunctionLikeMacroIdentifier(id)) => {
                // Handle function-like macro definition
                let name = id.clone();
                let range = *self.peek_range(0).unwrap();
                self.next_token(); // consume the identifier token

                let parameters = parse_parameters(self)?;
                let definition = parse_balanced_definition(self)?;
                self.consume_directive_end_or_eof()?;

                Define::FunctionLike {
                    identifier: name,
                    identifier_range: range,
                    parameters,
                    definition,
                }
            }
            Some(_) => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expected an identifier after directive `#define`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            }
            None => {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expected an identifier after directive `#define`.".to_owned(),
                ));
            }
        };

        Ok(define)
    }

    fn parse_include(&mut self) -> Result<Vec<TokenWithRange>, PreprocessError> {
        // ```diagram
        // include "FILE_PATH" \n //
        // ^                   ^__// to here
        // |______________________// current token, validated (wouldn't be NONE)
        //
        // variants:
        // - include <FILE_PATH> \n
        // - include MACRO_NAME \n
        // ```
        self.next_token(); // consumes "include"

        let mut components = vec![];

        // Move all tokens to `components` until we hit a newline or EOF.
        while let Some(token) = self.peek_token(0) {
            match token {
                Token::_DirectiveEnd => {
                    break;
                }
                _ => {
                    let token_with_range = self.next_token_with_range().unwrap();
                    components.push(token_with_range);
                }
            }
        }

        // Empty inclusion is not allowed.
        if components.is_empty() {
            if self.peek_range(0).is_some() {
                return Err(PreprocessError::MessageWithPosition(
                    "Expected a file path or macro name after `#include`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            } else {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expected a file path or macro name after `#include`.".to_owned(),
                ));
            }
        }

        self.consume_directive_end_or_eof()?;

        Ok(components)
    }

    fn parse_embed(&mut self) -> Result<Vec<TokenWithRange>, PreprocessError> {
        // ```diagram
        // embed "FILE_PATH" OPTIONAL_PARAMETERS \n //
        // ^                                     ^__// to here
        // |_______________________________________-// current token, validated (wouldn't be NONE)
        //
        // Variants:
        // - embed <FILE_PATH> OPTIONAL_PARAMETERS \n
        // - embed MACRO_NAME OPTIONAL_PARAMETERS \n
        //
        //
        // ```
        self.next_token(); // consumes "embed"

        let mut components = vec![];

        // Move all tokens to `components` until we hit a newline or EOF.
        while let Some(token) = self.peek_token(0) {
            match token {
                Token::_DirectiveEnd => {
                    break;
                }
                _ => {
                    let token_with_range = self.next_token_with_range().unwrap();
                    components.push(token_with_range);
                }
            }
        }

        // Empty inclusion is not allowed.
        if components.is_empty() {
            if self.peek_range(0).is_some() {
                return Err(PreprocessError::MessageWithPosition(
                    "Expected a file path or macro name after `#embed`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            } else {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expected a file path or macro name after `#embed`.".to_owned(),
                ));
            }
        }

        self.consume_directive_end_or_eof()?;

        Ok(components)
    }

    fn parse_if(&mut self) -> Result<If, PreprocessError> {
        // ```diagram
        // if CONDITION \n
        // ^            ^__// to here
        // |________________// current token, validated (wouldn't be NONE)
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
        let mut alternative = None;

        let collect_expression =
            |parser: &mut Parser| -> Result<Vec<TokenWithRange>, PreprocessError> {
                let mut expression_tokens = vec![];

                // Collect tokens until we hit a newline or EOF.
                loop {
                    match parser.peek_token(0) {
                        Some(token) if token == &Token::_DirectiveEnd => {
                            break;
                        }
                        Some(_) => {
                            let token_with_range = parser.next_token_with_range().unwrap();
                            expression_tokens.push(token_with_range);
                        }
                        None => {
                            return Err(PreprocessError::UnexpectedEndOfDocument(
                                "Incomplete expression after `#if` or `#elif`.".to_owned(),
                            ));
                        }
                    }
                }

                Ok(expression_tokens)
            };

        let collect_consequence = |parser: &mut Parser| -> Result<Vec<Statement>, PreprocessError> {
            let mut consequence = vec![];

            // Collect statements until #else, #elif, #elifdef, #elifndef, or #endif.
            loop {
                if parser.peek_token(0).is_some() {
                    if matches!(parser.peek_token(0), Some(Token::_DirectiveStart))
                        && matches!(parser.peek_token(1), Some(Token::Identifier(id)) if
                       ["else", "elif", "elifdef", "elifndef", "endif"].contains(&id.as_str()))
                    {
                        break;
                    }

                    let statement = parser.parse_statement()?;
                    consequence.push(statement);
                } else {
                    return Err(PreprocessError::UnexpectedEndOfDocument(
                        "Incomplete consequence in the `#if` structure.".to_owned(),
                    ));
                }
            }

            Ok(consequence)
        };

        // Consumes "if" ("ifdef", "ifndef") in the first iteration,
        // and "elif" ("elifdef", "elifndef") in the subsequent iterations.
        while let Some(TokenWithRange {
            token: Token::Identifier(if_type),
            range: directive_range,
        }) = self.next_token_with_range()
        {
            match if_type.as_str() {
                "if" | "elif" => {
                    // Handle `if` or `elif` directive
                    let expression = collect_expression(self)?;
                    self.consume_directive_end_or_eof()?;

                    if expression.is_empty() {
                        return Err(PreprocessError::MessageWithPosition(
                            "Expected an expression after `#if` or `#elif`.".to_owned(),
                            self.last_range.start,
                        ));
                    }

                    let condition = Condition::Expression(expression);
                    let consequence = collect_consequence(self)?;
                    branches.push(Branch {
                        condition,
                        consequence,
                        directive_range,
                    });
                }
                "ifdef" | "elifdef" => {
                    // Handle `ifdef` or `elifdef` directive
                    let identifier = self.consume_identifier()?;
                    let range = self.last_range;

                    self.consume_directive_end_or_eof()?;

                    let condition = Condition::Defined {
                        identifier,
                        directive_range: range,
                    };
                    let consequence = collect_consequence(self)?;
                    branches.push(Branch {
                        condition,
                        consequence,
                        directive_range,
                    });
                }
                "ifndef" | "elifndef" => {
                    // Handle `ifndef` or `elifndef` directive
                    let identifier = self.consume_identifier()?;
                    let range = self.last_range;

                    self.consume_directive_end_or_eof()?;

                    let condition = Condition::NotDefined {
                        identifier,
                        directive_range: range,
                    };
                    let consequence = collect_consequence(self)?;
                    branches.push(Branch {
                        condition,
                        consequence,
                        directive_range,
                    });
                }
                _ => {
                    unreachable!()
                }
            }

            // Continue to the next iteration if we encounter
            // `#elif`, `#elifdef` or `#elifndef` directives.
            if matches!(self.peek_token(0), Some(Token::_DirectiveStart))
                && matches!(self.peek_token(1), Some(Token::Identifier(id)) if
               ["elif", "elifdef", "elifndef"].contains(&id.as_str()))
            {
                // it is directive `elif`, `elifdef`, or `elifndef`,
                // continue to the next iteration.
                self.next_token(); // Consumes '#'
            } else {
                break;
            }
        }

        // Collect the alternative statements if #else follows.
        if matches!(self.peek_token(0), Some(Token::_DirectiveStart))
            && matches!(self.peek_token(1), Some(Token::Identifier(id)) if id.as_str() == "else")
        {
            self.next_token(); // consumes '#'
            self.next_token(); // consumes "else"

            let directive_else_range = self.last_range;
            self.consume_directive_end()?;

            let alternative_statements = collect_consequence(self)?;
            alternative = Some(AlternativeBranch {
                statements: alternative_statements,
                directive_range: directive_else_range,
            });
        }

        // Require #endif to close the conditional group.
        self.consume_token_and_assert(&Token::_DirectiveStart, "directive start punctuator `#`")?;
        self.consume_identifier_and_assert("endif")?;
        self.consume_directive_end_or_eof()?;

        let if_ = If {
            branches,
            alternative,
        };

        Ok(if_)
    }

    fn parse_pragma(&mut self) -> Result<Pragma, PreprocessError> {
        // ```diagram
        // pragma ... \n //
        // ^          ^__// to here
        // |_____________// current token, validated (wouldn't be NONE)
        // ```

        self.next_token(); // consumes "pragma"

        let mut components = vec![];

        // Move all tokens to `components` until we hit a newline or EOF.
        while let Some(token) = self.peek_token(0) {
            match token {
                Token::_DirectiveEnd => {
                    break;
                }
                _ => {
                    let token_with_range = self.next_token_with_range().unwrap();
                    components.push(token_with_range);
                }
            }
        }

        // Empty pragma parameters are not allowed.
        if components.is_empty() {
            if self.peek_range(0).is_some() {
                return Err(PreprocessError::MessageWithPosition(
                    "Expected pragma parameters after `#pragma`.".to_owned(),
                    self.peek_range(0).unwrap().start,
                ));
            } else {
                return Err(PreprocessError::UnexpectedEndOfDocument(
                    "Expected pragma parameters after `#pragma`.".to_owned(),
                ));
            }
        }

        self.consume_directive_end_or_eof()?;

        Ok(Pragma { components })
    }
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        ast::{Program, Statement},
        ast_printer::print_program_to_string,
        error::PreprocessError,
        parser::parse_from_str,
        position::Position,
        range::Range,
    };

    fn format(s: &str) -> String {
        match parse_from_str(s) {
            Ok(program) => print_program_to_string(&program),
            Err(_) => unreachable!(),
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

        // Error: missing name and parameters
        assert!(matches!(
            parse_from_str("#pragma\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 7,
                    line: 0,
                    column: 7
                }
            ))
        ));

        // Error: missing name and parameters (without newline)
        assert!(matches!(
            parse_from_str("#pragma"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));
    }

    #[test]
    fn test_parse_define() {
        assert_eq!(format("#define MAX 100"), "#define MAX 100\n");

        assert_eq!(format("#define EMPTY"), "#define EMPTY\n");

        assert_eq!(
            format("#define SQUARE(X,Y) ((X)*(Y))"),
            "#define SQUARE(X, Y) ( ( X ) * ( Y ) )\n"
        );

        // Test `#`
        assert_eq!(
            format("#define PRINT(VAR) (printf(\"%s = %d\", #VAR, VAR))"),
            "#define PRINT(VAR) ( printf ( \"%s = %d\" , # VAR , VAR ) )\n"
        );

        // Test `##`
        assert_eq!(
            format("#define ARRAY(PREFIX, LEN) int[LEN] PREFIX ## LEN;"),
            "#define ARRAY(PREFIX, LEN) int [ LEN ] PREFIX ## LEN ;\n"
        );

        // Test variadic macros
        assert_eq!(
            format("#define SHOW_LIST(...) puts(__VA_ARGS__)"),
            "#define SHOW_LIST(...) puts ( __VA_ARGS__ )\n"
        );

        // Error: missing identifier after `#define`
        assert!(matches!(
            parse_from_str("#define\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 7,
                    line: 0,
                    column: 7
                }
            ))
        ));

        // Error: invalid type token after `#define`
        assert!(matches!(
            parse_from_str("#define 123"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 8,
                    line: 0,
                    column: 8
                }
            ))
        ));

        // Error: unbalanced parentheses in definition
        assert!(matches!(
            parse_from_str("#define FOO for ("),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // Error: unbalanced parentheses in definition
        assert!(matches!(
            parse_from_str("#define FOO for (\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 17,
                    line: 0,
                    column: 17
                }
            ))
        ));

        // Error: unbalanced parentheses in definition
        assert!(matches!(
            parse_from_str("#define FOO for (;;){\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 21,
                    line: 0,
                    column: 21
                }
            ))
        ));

        // Error: unbalanced brace in definition
        assert!(matches!(
            parse_from_str("#define FOO {a)\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 14,
                    line: 0,
                    column: 14
                }
            ))
        ));

        // Error: unbalanced bracket in definition
        assert!(matches!(
            parse_from_str("#define FOO [a)\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 14,
                    line: 0,
                    column: 14
                }
            ))
        ));

        // Error: the lexer reaches EOF before the string literal's closing quote.
        assert!(matches!(
            parse_from_str("#define FOO \"a\n"),
            Err(PreprocessError::UnexpectedEndOfDocument(_))
        ));

        // Error: the lexer reaches EOF before the character literal's closing quote.
        assert!(matches!(
            parse_from_str("#define FOO 'a\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 14,
                    line: 0,
                    column: 14
                }
            ))
        ));

        // Error: arguments after variadic parameter
        assert!(matches!(
            parse_from_str("#define FOO(..., a, b) a, b"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 15,
                    line: 0,
                    column: 15
                }
            ))
        ));
    }

    #[test]
    fn test_parse_undef() {
        assert_eq!(format("#undef FOO"), "#undef FOO\n");

        // Error: missing identifier after `#undef`
        assert!(matches!(
            parse_from_str("#undef\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 6,
                    line: 0,
                    column: 6
                }
            ))
        ));
    }

    #[test]
    fn test_parse_include() {
        assert_eq!(format("#include HEADER"), "#include HEADER\n");

        assert_eq!(format("#include <stdio.h>"), "#include <stdio.h>\n");

        assert_eq!(
            format("#include \"my_header.h\""),
            "#include \"my_header.h\"\n"
        );

        // Error: missing file path or macro name after `#include`
        assert!(matches!(
            parse_from_str("#include\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 8,
                    line: 0,
                    column: 8
                }
            ))
        ));
    }

    #[test]
    fn test_parse_embed() {
        assert_eq!(
            format("#embed HEMASHUSHU_JPEG_FILE"),
            "#embed HEMASHUSHU_JPEG_FILE\n"
        );

        assert_eq!(format("#embed <data.bin>"), "#embed <data.bin>\n");
        assert_eq!(format("#embed \"hippo.png\""), "#embed \"hippo.png\"\n");

        assert_eq!(
            format(
                "#embed </dev/random> limit(100) prefix(11, 13) suffix('\0') if_empty('a', 'b', 0)"
            ),
            "#embed </dev/random> limit ( 100 ) prefix ( 11 , 13 ) suffix ( '\\x00' ) if_empty ( 'a' , 'b' , 0 )\n"
        );

        // Error: missing file path or macro name after `#embed`
        assert!(matches!(
            parse_from_str("#embed\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 6,
                    line: 0,
                    column: 6
                }
            ))
        ));
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

        // Error: missing condition expression after `#if`
        assert!(matches!(
            parse_from_str("#if\n#endif"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 3,
                    line: 0,
                    column: 3
                }
            ))
        ));

        // Error: missing identifier after `#ifdef`
        assert!(matches!(
            parse_from_str("#ifdef\n#endif"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 6,
                    line: 0,
                    column: 6
                }
            ))
        ));

        // Error: extraneous token after condition expression
        assert!(matches!(
            parse_from_str(
                "\
            #ifdef FOO 123\n#endif"
            ),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 11,
                    line: 0,
                    column: 11
                }
            ))
        ));

        // Error: incomplete structure, missing `#if`, encountered `#endif`
        assert!(matches!(
            parse_from_str("a\n#endif"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 3,
                        line: 1,
                        column: 1
                    },
                    end_inclusive: Position {
                        index: 7,
                        line: 1,
                        column: 5
                    }
                }
            ))
        ));

        // Error: incomplete structure, missing `#if`, encountered `#else`
        assert!(matches!(
            parse_from_str("a\n#else"),
            Err(PreprocessError::MessageWithRange(
                _,
                Range {
                    start: Position {
                        index: 3,
                        line: 1,
                        column: 1
                    },
                    end_inclusive: Position {
                        index: 6,
                        line: 1,
                        column: 4
                    }
                }
            ))
        ));

        // Error: syntax error, extraneous tokens after `#else`
        assert!(matches!(
            parse_from_str("#ifdef FOO\n#else 123\n#endif"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 17,
                    line: 1,
                    column: 6
                }
            ))
        ));

        // Error: syntax error, extraneous tokens after `#endif`
        assert!(matches!(
            parse_from_str("#ifdef FOO\n#endif 123"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 18,
                    line: 1,
                    column: 7
                }
            ))
        ));
    }

    #[test]
    fn test_parse_error() {
        assert_eq!(
            parse_from_str("#error \"foobar\"").unwrap(),
            Program {
                statements: vec![Statement::Error {
                    message: "foobar".to_owned(),
                    directive_range: Range::from_detail(1, 0, 1, 5),
                    message_range: Range::from_detail(7, 0, 7, 8)
                }],
            }
        );

        // Error: missing error message after `#error`
        assert!(matches!(
            parse_from_str("#error\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 6,
                    line: 0,
                    column: 6
                }
            ))
        ));

        // Error: invalid token after `#error`
        assert!(matches!(
            parse_from_str("#error 123"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 7,
                    line: 0,
                    column: 7
                }
            ))
        ));
    }

    #[test]
    fn test_parse_warning() {
        assert_eq!(
            parse_from_str("#warning \"foobar\"").unwrap(),
            Program {
                statements: vec![Statement::Warning {
                    message: "foobar".to_owned(),
                    directive_range: Range::from_detail(1, 0, 1, 7),
                    message_range: Range::from_detail(9, 0, 9, 8)
                }],
            }
        );

        // Error: missing warning message after `#warning`
        assert!(matches!(
            parse_from_str("#warning\n"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 8,
                    line: 0,
                    column: 8
                }
            ))
        ));

        // Error: invalid token after `#warning`
        assert!(matches!(
            parse_from_str("#warning 123"),
            Err(PreprocessError::MessageWithPosition(
                _,
                Position {
                    index: 9,
                    line: 0,
                    column: 9
                }
            ))
        ));
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
}
