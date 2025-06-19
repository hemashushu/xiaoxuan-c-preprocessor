// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::io::Write;

use crate::expression::ast::Expression;

pub fn print_expression<W: Write>(writer: &mut W, expression: &Expression) -> std::io::Result<()> {
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
pub fn print_to_string(expression: &Expression) -> String {
    let mut output = Vec::new();
    print_expression(&mut output, expression).unwrap();
    String::from_utf8(output).unwrap()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use crate::{
        expression::{
            ast::{BinaryOperator, Expression, UnaryOperator},
            ast_printer::print_to_string,
        },
        location::Location,
    };

    #[test]
    fn test_print_expression() {
        // Test printing a simple number
        assert_eq!(
            print_to_string(&Expression::Number(11, Location::default(),)),
            "11"
        );

        // Test printing a binary expression
        assert_eq!(
            print_to_string(&Expression::Binary(
                BinaryOperator::Add,
                Location::default(),
                Box::new(Expression::Number(11, Location::default())),
                Box::new(Expression::Number(13, Location::default())),
            )),
            "(11 + 13)"
        );

        // Test printing a nested binary expression: ((1 + 2) * 3)
        assert_eq!(
            print_to_string(&Expression::Binary(
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
            print_to_string(&Expression::Unary(
                UnaryOperator::Minus,
                Location::default(),
                Box::new(Expression::Number(42, Location::default())),
            )),
            "(-42)"
        );

        // Test printing a unary expression with a nested binary expression: (-(11 + 13))
        assert_eq!(
            print_to_string(&Expression::Unary(
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
            print_to_string(&Expression::Binary(
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
}
