// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    PreprocessError, PreprocessFileError, TokenWithLocation,
    expression::ast::{BinaryOperator, Expression, UnaryOperator},
    location::Location,
    peekable_iter::PeekableIter,
    range::Range,
    token::{Number, Punctuator, Token},
};

const PEEK_BUFFER_LENGTH_PARSER: usize = 3;

pub fn parse_from_tokens(
    token_with_locations: &[TokenWithLocation],
    file_number: usize,
) -> Result<Expression, PreprocessFileError> {
    assert!(
        !token_with_locations.is_empty(),
        "Token list cannot be empty."
    );

    let mut token_iter = token_with_locations.to_vec().into_iter();
    let mut peekable_token_iter = PeekableIter::new(&mut token_iter, PEEK_BUFFER_LENGTH_PARSER);
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

pub struct ExpressionParser<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
    pub last_location: Location,
    pub current_file_number: usize,
}

impl<'a> ExpressionParser<'a> {
    fn new(
        upstream: &'a mut PeekableIter<'a, TokenWithLocation>,
        current_file_number: usize,
    ) -> Self {
        let last_location = if let Some(first_token) = upstream.peek(0) {
            // Initialize last_location with the first token's location.
            first_token.location
        } else {
            Location::new(current_file_number, &Range::default())
        };

        Self {
            upstream,
            last_location,
            current_file_number,
        }
    }

    // fn next_token_with_location(&mut self) -> Option<TokenWithLocation> {
    //     match self.upstream.next() {
    //         Some(token_with_location) => {
    //             self.last_location = token_with_location.location;
    //             Some(token_with_location)
    //         }
    //         None => None,
    //     }
    // }

    fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithLocation { token, location }) => {
                self.last_location = location;
                Some(token)
            }
            None => None,
        }
    }

    fn peek_location(&self, offset: usize) -> Option<&Location> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { location, .. }) => Some(location),
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithLocation { token, .. }) => Some(token),
            None => None,
        }
    }

    // fn peek_token_and_equals(&self, offset: usize, expected_token: &Token) -> bool {
    //     matches!(
    //         self.peek_token(offset),
    //         Some(token) if token == expected_token)
    // }

    fn expect_and_consume_token(
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
    fn expect_and_consume_opening_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisOpen),
            "opening parenthesis",
        )
    }

    // expects close parenthesis ')' and consumes it.
    fn expect_and_consume_closing_paren(&mut self) -> Result<(), PreprocessFileError> {
        self.expect_and_consume_token(
            &Token::Punctuator(Punctuator::ParenthesisClose),
            "closing parenthesis",
        )
    }
}

/**
 * C Operator Precedence
 * ---------------------
 *
 * The following table lists the precedence and associativity of C operators.
 * Operators are listed top to bottom, in descending precedence.
 *
 * 1: `()` Function call
 * 2:
 *    - `+ -` Unary plus and minus
 *    - `! ~` Logical NOT and bitwise NOT
 * 3: `* / %` Multiplication, division, and remainder
 * 4: `+ -` Addition and subtraction
 * 5: `<< >>` Bitwise left shift and right shift
 * 6:
 *    - `< <=` For relational operators < and ≤ respectively
 *    - `> >=` For relational operators > and ≥ respectively
 * 7: `== !=` For relational = and ≠ respectively
 * 8: `&` Bitwise AND
 * 9: `^` Bitwise XOR (exclusive or)
 * 10: `|` Bitwise OR (inclusive or)
 * 11: `&&` Logical AND
 * 12: `||` Logical OR
 *
 * See:
 * https://en.cppreference.com/w/c/language/operator_precedence.html
 */

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
                // For right-to-left associative operators, a separate function such as
                // `parse_binary_expression_right` should be implemented. In that case,
                // replace `next_parse_function` below with `parse_expression` and remove the `loop`.
                //
                // About associativity:
                //
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
        // let parse_arguments =
        //     |parser: &mut ExpressionParser| -> Result<Vec<Expression>, PreprocessFileError> {
        //         parser.expect_and_consume_opening_paren()?; // Consume the opening parenthesis `(`.
        //
        //         let mut arguments = Vec::new();
        //
        //         // Parse arguments until a closing parenthesis is found.
        //         while let Some(token) = parser.peek_token(0) {
        //             if token == &Token::Punctuator(Punctuator::ParenthesisClose) {
        //                 break; // End of arguments.
        //             }
        //
        //             // Parse an expression for the argument.
        //             let argument = parser.parse_expression()?;
        //             arguments.push(argument);
        //
        //             // Check for a comma to separate arguments.
        //             if parser.peek_token_and_equals(0, &Token::Punctuator(Punctuator::Comma)) {
        //                 parser.next_token(); // Consume the comma.
        //             } else {
        //                 break; // No more arguments.
        //             }
        //         }
        //
        //         parser.expect_and_consume_closing_paren()?;
        //
        //         Ok(arguments)
        //     };

        if let Some(token) = self.peek_token(0) {
            match token {
                // Handle numeric literals.
                Token::Number(Number::Integer(number)) => {
                    let location = *self.peek_location(0).unwrap();
                    let number_value = number.as_usize().map_err(|_| {
                        PreprocessFileError::new(
                            location.file_number,
                            PreprocessError::MessageWithRange(
                                "Invalid integer number.".to_string(),
                                location.range,
                            ),
                        )
                    })?;

                    self.next_token(); // Consume the number token.
                    return Ok(Expression::Number(number_value, location));
                }
                // Handle grouped expression.
                Token::Punctuator(Punctuator::ParenthesisOpen) => {
                    self.expect_and_consume_opening_paren()?;
                    // Parse the inner expression.
                    let inner_expression = self.parse_expression()?;
                    // Expect and consume the closing parenthesis.
                    self.expect_and_consume_closing_paren()?;
                    return Ok(inner_expression);
                }
                _ => {
                    let location = *self.peek_location(0).unwrap();
                    return Err(PreprocessFileError::new(
                        location.file_number,
                        PreprocessError::MessageWithRange(
                            "Expect a defined macro, integer number, or operator.".to_string(),
                            location.range,
                        ),
                    ));
                }
            }
        } else {
            // If no token is available, return an error.
            return Err(PreprocessFileError::new(
                self.current_file_number,
                PreprocessError::MessageWithRange(
                    "Expect a defined macro, integer number, or operator after this token."
                        .to_string(),
                    self.last_location.range,
                ),
            ));
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        PreprocessError, PreprocessFileError, TokenWithLocation,
        expression::{ast::Expression, ast_printer::print_to_string, parser::parse_from_tokens},
        lexer::lex_from_str,
        location::Location,
        range::Range,
    };
    use pretty_assertions::assert_eq;

    fn generate_expresson(s: &str) -> Result<Expression, PreprocessFileError> {
        let tokens = lex_from_str(s)
            .unwrap()
            .iter()
            .map(|item| TokenWithLocation {
                token: item.token.clone(),
                location: Location::new(1, &item.range),
            })
            .collect::<Vec<_>>();
        parse_from_tokens(&tokens, 1)
    }

    fn format(s: &str) -> String {
        let expression = generate_expresson(s).unwrap();
        print_to_string(&expression)
    }

    #[test]
    fn test_parse_expression_number() {
        assert_eq!(format("42"), "42");
        assert_eq!(format("0x2A"), "42");
        assert_eq!(format("0b101010"), "42");
        assert_eq!(format("052"), "42");

        // Test floating point numbers.
        assert!(matches!(
            generate_expresson("3.14"),
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
            generate_expresson("0x1.8p2"),
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
            generate_expresson("'a'"),
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
            generate_expresson("\"hello\""),
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
            generate_expresson("11 13"),
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
            generate_expresson("11 += 13"),
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
}
