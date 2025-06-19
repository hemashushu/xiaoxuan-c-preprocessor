// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{location::Location, token::Punctuator};

#[derive(Debug, PartialEq)]
pub enum Expression {
    Number(usize, Location),
    Binary(BinaryOperator, Location, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Location, Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
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
    ShiftRight, // '>>' (note: C does not have a logical right shift `>>>`)
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
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
