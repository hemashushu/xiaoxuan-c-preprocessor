// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::{fmt::Display, io::Write};

use crate::{
    error::{PreprocessError, PreprocessFileError},
    location::Location,
    peekable_iter::PeekableIter,
    token::{Number, Punctuator, Token, TokenWithLocation},
};

// Buffer sizes for lookahead in `PeekableIter`.
const PEEK_BUFFER_LENGTH_EXPRESSION_PARSE: usize = 3;

/// Expanded expressions used in `#if` and `#elif` directives
///
/// This expression can only contain integer constants, binary operators,
/// unary operators and grouping parentheses.
///
/// Which means all macros, operators (`defined(...)`, `__has_include(...)` etc.)
/// must be expanded before constructing this expression.
#[derive(Debug, PartialEq)]
enum Expression {
    Number(u64, Location),
    Binary(BinaryOperator, Location, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Location, Box<Expression>),
}

#[derive(Debug, PartialEq)]
enum BinaryOperator {
    // Arithmetic operators
    Add,      // '+'
    Subtract, // '-'
    Multiply, // '*'
    Divide,   // '/'
    Modulo,   // '%'

    // Relational operators
    Equal,              // '=='
    NotEqual,           // '!='
    LessThan,           // '<'
    LessThanOrEqual,    // '<='
    GreaterThan,        // '>'
    GreaterThanOrEqual, // '>='

    // Logical operators
    And, // '&&'
    Or,  // '||'

    // Bitwise operators
    BitwiseAnd, // '&', also used as the `AddressOf` operator
    BitwiseOr,  // '|'
    BitwiseXor, // '^'
    ShiftLeft,  // '<<'
    ShiftRight, // '>>' (Note: C language does not have a logical right shift `>>>`)
}

#[derive(Debug, PartialEq)]
enum UnaryOperator {
    Plus,       // '+', does not change the operand's value (included for symmetry with `Minus`)
    Minus,      // '-', negation
    LogicalNot, // '!'
    BitwiseNot, // '~'
}

impl From<Punctuator> for BinaryOperator {
    fn from(punctuator: Punctuator) -> Self {
        match punctuator {
            // Arithmetic operators
            Punctuator::Add => BinaryOperator::Add,
            Punctuator::Subtract => BinaryOperator::Subtract,
            Punctuator::Multiply => BinaryOperator::Multiply,
            Punctuator::Divide => BinaryOperator::Divide,
            Punctuator::Modulo => BinaryOperator::Modulo,
            // Relational operators
            Punctuator::Equal => BinaryOperator::Equal,
            Punctuator::NotEqual => BinaryOperator::NotEqual,
            Punctuator::LessThan => BinaryOperator::LessThan,
            Punctuator::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
            Punctuator::GreaterThan => BinaryOperator::GreaterThan,
            Punctuator::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
            // Logical operators
            Punctuator::And => BinaryOperator::And,
            Punctuator::Or => BinaryOperator::Or,
            // Bitwise operators
            Punctuator::BitwiseAnd => BinaryOperator::BitwiseAnd,
            Punctuator::BitwiseOr => BinaryOperator::BitwiseOr,
            Punctuator::BitwiseXor => BinaryOperator::BitwiseXor,
            Punctuator::ShiftLeft => BinaryOperator::ShiftLeft,
            Punctuator::ShiftRight => BinaryOperator::ShiftRight,
            _ => unreachable!(),
        }
    }
}

impl From<Punctuator> for UnaryOperator {
    fn from(punctuator: Punctuator) -> Self {
        match punctuator {
            Punctuator::Add => UnaryOperator::Plus,
            Punctuator::Subtract => UnaryOperator::Minus,
            Punctuator::Not => UnaryOperator::LogicalNot,
            Punctuator::BitwiseNot => UnaryOperator::BitwiseNot,
            _ => unreachable!(),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            // Arithmetic operators
            BinaryOperator::Add => "+",
            BinaryOperator::Subtract => "-",
            BinaryOperator::Multiply => "*",
            BinaryOperator::Divide => "/",
            BinaryOperator::Modulo => "%",
            // Relational operators
            BinaryOperator::Equal => "==",
            BinaryOperator::NotEqual => "!=",
            BinaryOperator::LessThan => "<",
            BinaryOperator::LessThanOrEqual => "<=",
            BinaryOperator::GreaterThan => ">",
            BinaryOperator::GreaterThanOrEqual => ">=",
            // Logical operators
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            // Bitwise operators
            BinaryOperator::BitwiseAnd => "&",
            BinaryOperator::BitwiseOr => "|",
            BinaryOperator::BitwiseXor => "^",
            BinaryOperator::ShiftLeft => "<<",
            BinaryOperator::ShiftRight => ">>",
        };
        write!(f, "{}", symbol)
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = match self {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::LogicalNot => "!",
            UnaryOperator::BitwiseNot => "~",
        };
        write!(f, "{}", symbol)
    }
}

fn print_expression<W: Write>(writer: &mut W, expression: &Expression) -> std::io::Result<()> {
    match expression {
        Expression::Number(number, _) => write!(writer, "{number}"),
        Expression::Binary(operator, _, left, right) => {
            write!(writer, "(")?;
            print_expression(writer, left)?;
            write!(writer, " {operator} ")?;
            print_expression(writer, right)?;
            write!(writer, ")")
        }
        Expression::Unary(operator, _, exp) => {
            write!(writer, "({operator}")?;
            print_expression(writer, exp)?;
            write!(writer, ")")
        }
    }
}

#[allow(dead_code)]
fn print_expression_to_string(expression: &Expression) -> String {
    let mut output = Vec::new();
    print_expression(&mut output, expression).unwrap();
    String::from_utf8(output).unwrap()
}

// C Operator Precedence
// ---------------------
//
// The following table lists the precedence and associativity of C operators.
// Operators are listed top to bottom, in descending precedence.
//
// 1: `()` Function call
// 2:
//    - `+ -` Unary plus and minus
//    - `! ~` Logical NOT and bitwise NOT
// 3: `* / %` Multiplication, division, and remainder
// 4: `+ -` Addition and subtraction
// 5: `<< >>` Bitwise left shift and right shift
// 6:
//    - `< <=` For relational operators < and ≤ respectively
//    - `> >=` For relational operators > and ≥ respectively
// 7: `== !=` For relational = and ≠ respectively
// 8: `&` Bitwise AND
// 9: `^` Bitwise XOR (exclusive or)
// 10: `|` Bitwise OR (inclusive or)
// 11: `&&` Logical AND
// 12: `||` Logical OR
//
// See:
// https://en.cppreference.com/w/c/language/operator_precedence.html

/// Parse an expanded integer expression used in `#if` and `#elif` directives.
///
/// This expression can only contain integer constants, binary operators,
/// unary operators and grouping parentheses.
///
/// Which means all macros, operators (`defined(...)`, `__has_include(...)` etc.)
/// must be expanded before constructing this expression.
fn parse_expression(
    token_with_locations: &[TokenWithLocation],

    // The file number where the expression is located.
    // Used for error reporting.
    //
    // Because expression consisting of tokens that may be
    // expanded from macros defined in other files, although
    // `token_with_locations` contains location information, but
    // we do not know which file the expression is originally from,
    // we need to pass the current file number explicitly.
    file_number: usize,
) -> Result<Expression, PreprocessFileError> {
    let mut token_iter = token_with_locations.iter().cloned();
    let mut peekable_token_iter =
        PeekableIter::new(&mut token_iter, PEEK_BUFFER_LENGTH_EXPRESSION_PARSE);
    let mut parser = ExpressionParser::new(&mut peekable_token_iter, file_number);

    let expression = parser.parse_expression()?;

    if parser.peek_token(0).is_some() {
        let location = parser.peek_location(0).unwrap();
        return Err(PreprocessFileError::new(
            location.file_number,
            PreprocessError::MessageWithRange(
                "Unexpected tokens after the expression.".to_string(),
                location.range,
            ),
        ));
    }

    Ok(expression)
}

struct ExpressionParser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithLocation>,

    // The file number where the original expression is located.
    current_file_number: usize,

    // The location of the last consumed token.
    last_location: Location,
}

impl<'a> ExpressionParser<'a> {
    fn new(
        upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
        current_file_number: usize,
    ) -> Self {
        Self {
            upstream,
            current_file_number,
            last_location: Location::default(),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithLocation { token, location }) => {
                self.last_location = location;
                Some(token)
            }
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { token, .. }) => Some(token),
            None => None,
        }
    }

    fn peek_location(&self, offset: usize) -> Option<&Location> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { location, .. }) => Some(location),
            None => None,
        }
    }

    fn consume_token_and_expect(
        &mut self,
        expected_token: &Token,
        token_description: &str,
    ) -> Result<(), PreprocessFileError> {
        match self.next_token() {
            Some(token) => {
                if &token == expected_token {
                    Ok(())
                } else {
                    Err(PreprocessFileError::new(
                        self.last_location.file_number,
                        PreprocessError::MessageWithPosition(
                            format!("Expect token: {}.", token_description),
                            self.last_location.range.start,
                        ),
                    ))
                }
            }
            None => Err(PreprocessFileError::new(
                self.current_file_number,
                PreprocessError::UnexpectedEndOfDocument(format!(
                    "Expect token: {}.",
                    token_description
                )),
            )),
        }
    }

    // expects open parenthesis '(' and consumes it.
    fn consume_opening_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.consume_token_and_expect(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    // expects close parenthesis ')' and consumes it.
    fn consume_closing_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.consume_token_and_expect(
            &Token::Punctuator(Punctuator::ParenthesisClose),
            "closing parenthesis",
        )
    }
}

impl ExpressionParser<'_> {
    fn parse_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_logic_or_expression()
    }

    fn parse_logic_or_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::Or],
            ExpressionParser::parse_logic_and_expression,
        )
    }

    fn parse_logic_and_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::And],
            ExpressionParser::parse_bitwise_or_expression,
        )
    }

    fn parse_bitwise_or_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::BitwiseOr],
            ExpressionParser::parse_bitwise_xor_expression,
        )
    }

    fn parse_bitwise_xor_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::BitwiseXor],
            ExpressionParser::parse_bitwise_and_expression,
        )
    }

    fn parse_bitwise_and_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::BitwiseAnd],
            ExpressionParser::parse_equality_expression,
        )
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::Equal, Punctuator::NotEqual],
            ExpressionParser::parse_relational_expression,
        )
    }

    fn parse_relational_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[
                Punctuator::LessThan,
                Punctuator::LessThanOrEqual,
                Punctuator::GreaterThan,
                Punctuator::GreaterThanOrEqual,
            ],
            ExpressionParser::parse_shift_expression,
        )
    }

    fn parse_shift_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::ShiftLeft, Punctuator::ShiftRight],
            ExpressionParser::parse_additive_expression,
        )
    }

    fn parse_additive_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::Add, Punctuator::Subtract],
            ExpressionParser::parse_multiplicative_expression,
        )
    }

    fn parse_multiplicative_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        self.parse_binary_expression(
            &[Punctuator::Multiply, Punctuator::Divide, Punctuator::Modulo],
            ExpressionParser::parse_unary_expression,
        )
    }

    fn parse_binary_expression(
        &mut self,
        expected_punctuators: &[Punctuator],
        next_parse_function: fn(&mut Self) -> Result<Expression, PreprocessFileError>,
    ) -> Result<Expression, PreprocessFileError> {
        let mut left_expression = next_parse_function(self)?;

        while let Some(Token::Punctuator(punctuator)) = self.peek_token(0) {
            if let Some(punctuator) = expected_punctuators.iter().find(|&p| p == punctuator) {
                let operator = BinaryOperator::from(*punctuator);
                let location = *self.peek_location(0).unwrap();

                // Consume the operator token.
                self.next_token();

                // Parse the right-hand side expression.
                // This implementation assumes the operator is left-to-right associative.
                //
                // For right-to-left associative operators, you should implement a separate function,
                // such as `parse_binary_expression_right`. In that case, replace `next_parse_function`
                // below with `parse_expression` and remove the `loop`.
                //
                // About associativity:
                // Consider the expression `a + b + c + d`:
                // - If the operator is left-to-right associative, it is parsed as:
                //   `((a + b) + c) + d`
                // - If the operator is right-to-left associative, it is parsed as:
                //   `a + (b + (c + d))`
                let right_expression = next_parse_function(self)?;

                // Create a binary expression node.
                left_expression = Expression::Binary(
                    operator,
                    location,
                    Box::new(left_expression),
                    Box::new(right_expression),
                );
            } else {
                break; // No more operators, exit the loop.
            }
        }

        Ok(left_expression)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        // Check for unary operators.
        if let Some(Token::Punctuator(punctuator)) = self.peek_token(0) {
            match punctuator {
                Punctuator::Add
                | Punctuator::Subtract
                | Punctuator::Not
                | Punctuator::BitwiseNot => {
                    let operator = UnaryOperator::from(*punctuator);
                    let location = *self.peek_location(0).unwrap();

                    // Consume the unary operator.
                    self.next_token();

                    // Parse the operand.
                    let operand = self.parse_unary_expression()?;

                    // Create a unary expression node.
                    return Ok(Expression::Unary(operator, location, Box::new(operand)));
                }
                _ => {}
            }
        }

        // If no unary operator, parse primary expression.
        self.parse_primary_expression()
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, PreprocessFileError> {
        if let Some(token) = self.peek_token(0) {
            match token {
                // Handle numeric literals.
                Token::Number(Number::Integer(number)) => {
                    let location = *self.peek_location(0).unwrap();
                    let number_value = number.as_u64().map_err(|_| {
                        PreprocessFileError::new(
                            location.file_number,
                            PreprocessError::MessageWithRange(
                                "Invalid integer number.".to_string(),
                                location.range,
                            ),
                        )
                    })?;

                    self.next_token(); // Consume the number token.
                    Ok(Expression::Number(number_value, location))
                }
                // Handle grouped expression.
                Token::Punctuator(Punctuator::ParenthesisOpen) => {
                    self.consume_opening_paren()?;
                    // Parse the inner expression.
                    let inner_expression = self.parse_expression()?;
                    // Expect and consume the closing parenthesis.
                    self.consume_closing_paren()?;
                    Ok(inner_expression)
                }
                _ => {
                    let location = *self.peek_location(0).unwrap();
                    Err(PreprocessFileError::new(
                        location.file_number,
                        PreprocessError::MessageWithRange(
                            "Expect a macro, an integer number, or an operator.".to_string(),
                            location.range,
                        ),
                    ))
                }
            }
        } else {
            // If no token is available, return an error.
            Err(PreprocessFileError::new(
                self.last_location.file_number,
                PreprocessError::MessageWithRange(
                    "Incomplete expression.".to_string(),
                    self.last_location.range,
                ),
            ))
        }
    }
}

/// Evaluate an expanded expression used in `#if` and `#elif` directives.
///
/// This expression can only contain integer constants, binary operators,
/// unary operators and grouping parentheses.
///
/// Which means all macros, operators (`defined(...)`, `__has_include(...)` etc.)
/// must be expanded before constructing this expression.
pub fn evaluate(
    token_with_locations: &[TokenWithLocation],
    file_number: usize,
) -> Result<isize, PreprocessFileError> {
    let expression = parse_expression(token_with_locations, file_number)?;
    evaluate_expression(&expression)
}

fn evaluate_expression(expression: &Expression) -> Result<isize, PreprocessFileError> {
    match expression {
        Expression::Number(value, _) => Ok(*value as isize),
        Expression::Binary(operator, operator_location, left_exp, right_exp) => {
            evaluate_binary_expression(operator, operator_location, left_exp, right_exp)
        }
        Expression::Unary(operator, operator_location, exp) => {
            evaluate_unary_expression(operator, operator_location, exp)
        }
    }
}

fn evaluate_binary_expression(
    operator: &BinaryOperator,
    operator_location: &Location,
    left_exp: &Expression,
    right_exp: &Expression,
) -> Result<isize, PreprocessFileError> {
    let left = evaluate_expression(left_exp)?;

    // Logical short-circuiting
    if matches!(operator, BinaryOperator::And) && left == 0 {
        return Ok(0);
    }

    if matches!(operator, BinaryOperator::Or) && left != 0 {
        return Ok(1);
    }

    let right = evaluate_expression(right_exp)?;

    let result = match operator {
        // Arithmetic operators
        BinaryOperator::Add => left + right,
        BinaryOperator::Subtract => left - right,
        BinaryOperator::Multiply => left * right,
        BinaryOperator::Divide => {
            if right == 0 {
                return Err(PreprocessFileError::new(
                    operator_location.file_number,
                    PreprocessError::MessageWithRange(
                        "Division by zero is not allowed".to_string(),
                        operator_location.range,
                    ),
                ));
            }
            left / right
        }
        BinaryOperator::Modulo => {
            if right == 0 {
                return Err(PreprocessFileError::new(
                    operator_location.file_number,
                    PreprocessError::MessageWithRange(
                        "Modulo by zero is not allowed".to_string(),
                        operator_location.range,
                    ),
                ));
            }
            left % right
        }
        // Relational operators
        BinaryOperator::Equal => {
            if left == right {
                1
            } else {
                0
            }
        }
        BinaryOperator::NotEqual => {
            if left != right {
                1
            } else {
                0
            }
        }
        BinaryOperator::LessThan => {
            if left < right {
                1
            } else {
                0
            }
        }
        BinaryOperator::LessThanOrEqual => {
            if left <= right {
                1
            } else {
                0
            }
        }
        BinaryOperator::GreaterThan => {
            if left > right {
                1
            } else {
                0
            }
        }
        BinaryOperator::GreaterThanOrEqual => {
            if left >= right {
                1
            } else {
                0
            }
        }
        // Logical operators
        BinaryOperator::And => {
            if left != 0 && right != 0 {
                1
            } else {
                0
            }
        }
        BinaryOperator::Or => {
            if left != 0 || right != 0 {
                1
            } else {
                0
            }
        }
        // Bitwise operators
        BinaryOperator::BitwiseAnd => left & right,
        BinaryOperator::BitwiseOr => left | right,
        BinaryOperator::BitwiseXor => left ^ right,
        BinaryOperator::ShiftLeft => left << right,
        BinaryOperator::ShiftRight => left >> right,
    };
    Ok(result)
}

fn evaluate_unary_expression(
    operator: &UnaryOperator,
    _operator_location: &Location,
    exp: &Expression,
) -> Result<isize, PreprocessFileError> {
    let value = evaluate_expression(exp)?;
    match operator {
        UnaryOperator::Plus => Ok(value),
        UnaryOperator::Minus => Ok(-value),
        UnaryOperator::LogicalNot => Ok(if value == 0 { 1 } else { 0 }),
        UnaryOperator::BitwiseNot => Ok(!value),
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        error::{PreprocessError, PreprocessFileError},
        expression::{
            BinaryOperator, Expression, UnaryOperator, evaluate, parse_expression,
            print_expression_to_string,
        },
        lexer::lex_from_str,
        location::Location,
        range::Range,
        token::TokenWithLocation,
    };

    // Helper function to build an expression from a string.
    fn build_expresson(s: &str) -> Result<Expression, PreprocessFileError> {
        let tokens = lex_from_str(s)
            .unwrap()
            .iter()
            .map(|item| TokenWithLocation {
                token: item.token.clone(),
                location: Location::new(1, &item.range),
            })
            .collect::<Vec<_>>();
        parse_expression(&tokens, 1)
    }

    /// Helper function to format an expression from a string.
    fn format(s: &str) -> String {
        let expression = build_expresson(s).unwrap();
        print_expression_to_string(&expression)
    }

    /// Helper function to evaluate an expression from a string.
    fn eval(s: &str) -> isize {
        let tokens = lex_from_str(s)
            .unwrap()
            .iter()
            .map(|item| TokenWithLocation {
                token: item.token.clone(),
                location: Location::new(1, &item.range),
            })
            .collect::<Vec<_>>();
        evaluate(&tokens, 1).unwrap()
    }

    #[test]
    fn test_print_expression() {
        // Test printing a simple number
        assert_eq!(
            print_expression_to_string(&Expression::Number(11, Location::default(),)),
            "11"
        );

        // Test printing a binary expression
        assert_eq!(
            print_expression_to_string(&Expression::Binary(
                BinaryOperator::Add,
                Location::default(),
                Box::new(Expression::Number(11, Location::default())),
                Box::new(Expression::Number(13, Location::default())),
            )),
            "(11 + 13)"
        );

        // Test printing a nested binary expression: ((1 + 2) * 3)
        assert_eq!(
            print_expression_to_string(&Expression::Binary(
                BinaryOperator::Multiply,
                Location::default(),
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Location::default(),
                    Box::new(Expression::Number(1, Location::default(),)),
                    Box::new(Expression::Number(2, Location::default(),)),
                )),
                Box::new(Expression::Number(3, Location::default(),)),
            )),
            "((1 + 2) * 3)"
        );

        // Test printing a unary expression: (-42)
        assert_eq!(
            print_expression_to_string(&Expression::Unary(
                UnaryOperator::Minus,
                Location::default(),
                Box::new(Expression::Number(42, Location::default())),
            )),
            "(-42)"
        );

        // Test printing a unary expression with a nested binary expression: (-(11 + 13))
        assert_eq!(
            print_expression_to_string(&Expression::Unary(
                UnaryOperator::Minus,
                Location::default(),
                Box::new(Expression::Binary(
                    BinaryOperator::Add,
                    Location::default(),
                    Box::new(Expression::Number(11, Location::default())),
                    Box::new(Expression::Number(13, Location::default())),
                )),
            )),
            "(-(11 + 13))"
        );

        // Test printing a binary expression with a unary operator: (11 + (-13))
        assert_eq!(
            print_expression_to_string(&Expression::Binary(
                BinaryOperator::Add,
                Location::default(),
                Box::new(Expression::Number(11, Location::default())),
                Box::new(Expression::Unary(
                    UnaryOperator::Minus,
                    Location::default(),
                    Box::new(Expression::Number(13, Location::default())),
                )),
            )),
            "(11 + (-13))"
        );
    }

    #[test]
    fn test_parse_expression_number() {
        assert_eq!(format("42"), "42");
        assert_eq!(format("0x2A"), "42");
        assert_eq!(format("0b101010"), "42");
        assert_eq!(format("052"), "42");

        // Test floating point numbers.
        assert!(matches!(
            build_expresson("3.14"),
            Err(PreprocessFileError {
                file_number: 1,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                ),
            })
            if range == Range::from_detail(0, 0, 0, 4)
        ));

        assert!(matches!(
            build_expresson("0x1.8p2"),
            Err(PreprocessFileError {
                file_number: 1,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                ),
            })
            if range == Range::from_detail(0, 0, 0, 7)
        ));
    }

    #[test]
    fn test_parse_expression_unsupported_literal() {
        // Test char literals.
        assert!(matches!(
            build_expresson("'a'"),
            Err(PreprocessFileError {
                file_number: 1,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                ),
            })
            if range == Range::from_detail(0, 0, 0, 3)
        ));

        // Test string literals.
        assert!(matches!(
            build_expresson("\"hello\""),
            Err(PreprocessFileError {
                file_number: 1,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                ),
            })
            if range == Range::from_detail(0, 0, 0, 7)
        ));
    }

    #[test]
    fn test_parse_expression_binary() {
        // Test arithmetic operators.
        assert_eq!(format("11 + 13"), "(11 + 13)");
        assert_eq!(format("11 - 13"), "(11 - 13)");
        assert_eq!(format("11 * 13"), "(11 * 13)");
        assert_eq!(format("11 / 13"), "(11 / 13)");
        assert_eq!(format("11 % 13"), "(11 % 13)");

        // Test relational operators.
        assert_eq!(format("11 == 13"), "(11 == 13)");
        assert_eq!(format("11 != 13"), "(11 != 13)");
        assert_eq!(format("11 < 13"), "(11 < 13)");
        assert_eq!(format("11 <= 13"), "(11 <= 13)");
        assert_eq!(format("11 > 13"), "(11 > 13)");
        assert_eq!(format("11 >= 13"), "(11 >= 13)");

        // Test logical operators.
        assert_eq!(format("11 && 13"), "(11 && 13)");
        assert_eq!(format("11 || 13"), "(11 || 13)");

        // Test bitwise operators.
        assert_eq!(format("11 & 13"), "(11 & 13)");
        assert_eq!(format("11 | 13"), "(11 | 13)");
        assert_eq!(format("11 ^ 13"), "(11 ^ 13)");
        assert_eq!(format("11 << 13"), "(11 << 13)");
        assert_eq!(format("11 >> 13"), "(11 >> 13)");

        // Test mixed operators with different precedence.
        assert_eq!(format("11 + 13 * 17"), "(11 + (13 * 17))");
        assert_eq!(format("11 - 13 / 17"), "(11 - (13 / 17))");
        assert_eq!(format("11 * 13 + 17"), "((11 * 13) + 17)");
        assert_eq!(format("11 / 13 - 17"), "((11 / 13) - 17)");
        assert_eq!(format("11 & 13 | 17"), "((11 & 13) | 17)");
        assert_eq!(format("11 | 13 ^ 17"), "(11 | (13 ^ 17))");
        assert_eq!(format("11 ^ 13 & 17"), "(11 ^ (13 & 17))");

        assert_eq!(format("11 == 13 && 17 != 19"), "((11 == 13) && (17 != 19))");
        assert_eq!(format("11 < 13 || 17 > 19"), "((11 < 13) || (17 > 19))");

        // Test changing precedence with parentheses.
        assert_eq!(format("(11 + 13) * (17 - 19)"), "((11 + 13) * (17 - 19))");
        assert_eq!(
            format("((11 == 13) * (17 & 19))"),
            "((11 == 13) * (17 & 19))"
        );
    }

    #[test]
    fn test_parse_expression_unary() {
        // Test unary operators.
        assert_eq!(format("+11"), "(+11)");
        assert_eq!(format("-11"), "(-11)");
        assert_eq!(format("!11"), "(!11)");
        assert_eq!(format("~11"), "(~11)");

        // Test unary operators with binary expressions.
        assert_eq!(format("-11 + 13"), "((-11) + 13)");
        assert_eq!(format("-(11 + 13)"), "(-(11 + 13))");
    }

    #[test]
    fn test_parse_expression_syntax_error() {
        // Test extraneous tokens.
        assert!(matches!(
            build_expresson("11 13"),
            Err(PreprocessFileError {
                file_number: 1,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                ),
            })
            if range == Range::from_detail(3, 0, 3, 2)
        ));

        // Test invalid operator.
        assert!(matches!(
            build_expresson("11 += 13"),
            Err(PreprocessFileError {
                file_number: 1,
                error: PreprocessError::MessageWithRange(
                    _,
                    range
                ),
            })
            if range == Range::from_detail(3, 0, 3, 2)
        ));
    }

    #[test]
    fn test_evaluate_expression_number() {
        assert_eq!(eval("42"), 42);
        assert_eq!(eval("-42"), -42);
        assert_eq!(eval("0"), 0);
        assert_eq!(eval("0x2a"), 42);
        assert_eq!(eval("0b101010"), 42);
        assert_eq!(eval("052"), 42);
    }

    #[test]
    fn test_evaluate_expression_binary() {
        // Arithmetic operations
        assert_eq!(eval("8 + 4"), 12);
        assert_eq!(eval("8 - 4"), 4);
        assert_eq!(eval("8 * 4"), 32);
        assert_eq!(eval("8 / 4"), 2);
        assert_eq!(eval("8 % 3"), 2);

        // Relational operations
        assert_eq!(eval("8 == 8"), 1);
        assert_eq!(eval("8 != 4"), 1);
        assert_eq!(eval("8 < 10"), 1);
        assert_eq!(eval("8 <= 8"), 1);
        assert_eq!(eval("8 > 4"), 1);
        assert_eq!(eval("8 >= 8"), 1);

        assert_eq!(eval("8 == 4"), 0);
        assert_eq!(eval("8 != 8"), 0);
        assert_eq!(eval("8 < 8"), 0);
        assert_eq!(eval("8 <= 4"), 0);
        assert_eq!(eval("8 > 10"), 0);
        assert_eq!(eval("8 >= 10"), 0);

        // Logical operations
        assert_eq!(eval("1 && 1"), 1);
        assert_eq!(eval("1 && 0"), 0);
        assert_eq!(eval("0 && 1"), 0);
        assert_eq!(eval("0 && 0"), 0);
        assert_eq!(eval("1 || 1"), 1);
        assert_eq!(eval("1 || 0"), 1);
        assert_eq!(eval("0 || 1"), 1);
        assert_eq!(eval("0 || 0"), 0);

        // Bitwise operations
        assert_eq!(eval("8 & 4"), 0);
        assert_eq!(eval("8 | 4"), 12);
        assert_eq!(eval("8 ^ 4"), 12);
        assert_eq!(eval("8 << 2"), 32);
        assert_eq!(eval("8 >> 2"), 2);

        // Mixed operations with precedence
        assert_eq!(eval("2 + 3 * 4"), 14);
        assert_eq!(eval("2 * 3 + 4"), 10);
        assert_eq!(eval("2 + 3 * 4 - 5"), 9);

        assert_eq!(eval("8 & 4 | 2"), 2);
        assert_eq!(eval("8 | 4 ^ 2"), 14);
        assert_eq!(eval("8 ^ 4 & 2"), 8);

        assert_eq!(eval("8 == 8 && 8 != 4"), 1);
        assert_eq!(eval("8 > 4 || 8 < 4"), 1);

        // Change precedence with parentheses
        assert_eq!(eval("(2 + 3) * 4"), 20);
        assert_eq!(eval("2 + (3 * 4)"), 14);
        assert_eq!(eval("8 & (4 | 2)"), 0);
        assert_eq!(eval("8 | (4 ^ 2)"), 14);
        assert_eq!(eval("8 == (8 && 4)"), 0);
    }

    #[test]
    fn test_evaluate_expression_unary() {
        // Unary operations
        assert_eq!(eval("+42"), 42);
        assert_eq!(eval("-42"), -42);
        assert_eq!(eval("!0"), 1);
        assert_eq!(eval("!1"), 0);
        assert_eq!(eval("~42"), -43); // Bitwise NOT
        assert_eq!(eval("~0"), -1); // Bitwise NOT
        assert_eq!(eval("~1"), -2); // Bitwise NOT

        // Unary operations with binary
        assert_eq!(eval("-8 + 4"), -4);
        assert_eq!(eval("8 - -4"), 12);
        assert_eq!(eval("!8 && 0"), 0);
        assert_eq!(eval("!0 || 1"), 1);
        assert_eq!(eval("~8 & 4"), 4);
        assert_eq!(eval("8 | ~4"), -5);
    }
}
