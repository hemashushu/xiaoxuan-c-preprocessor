// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.


#[derive(Debug, PartialEq)]
pub enum Expression {
    Number((Number, Range)),
    Identifier((String, Range)),
    Bool(bool),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    FunctionCall(/* function name */ String, Vec<Expression>),
    Group(Vec<Expression>),
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
