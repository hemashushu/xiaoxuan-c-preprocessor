// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{
    PreprocessError, PreprocessFileError, TokenWithLocation,
    expression::{
        ast::{BinaryOperator, Expression, UnaryOperator},
        parser::parse_from_tokens,
    },
    location::Location,
};

pub fn evaluate_token_with_locations(
    token_with_locations: &[TokenWithLocation],
    file_number: usize,
) -> Result<isize, PreprocessFileError> {
    let expression = parse_from_tokens(token_with_locations, file_number)?;
    evaluate_expression(&expression)
}

pub fn evaluate_expression(expression: &Expression) -> Result<isize, PreprocessFileError> {
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
    use pretty_assertions;

    use crate::{
        TokenWithLocation,
        expression::{evaluator::evaluate_expression, parser::parse_from_tokens},
        lexer::lex_from_str,
        location::Location,
    };

    fn eval(s: &str) -> isize {
        let tokens = lex_from_str(s)
            .unwrap()
            .iter()
            .map(|item| TokenWithLocation {
                token: item.token.clone(),
                location: Location::new(1, &item.range),
            })
            .collect::<Vec<_>>();
        let expression = parse_from_tokens(&tokens, 1).unwrap();
        evaluate_expression(&expression).unwrap()
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
