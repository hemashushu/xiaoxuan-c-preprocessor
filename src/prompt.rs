// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use crate::{position::Position, range::Range};

/// `Prompt` is similar to `PreprocessError`, but is intended for user-facing messages
/// that do not necessarily indicate an error. It can be used to display informational
/// messages, warnings, or other notifications relevant to the user.
#[derive(Debug, PartialEq)]
pub enum Prompt {
    Message(PromptLevel, String),
    MessageWithPosition(PromptLevel, String, Position),
    MessageWithRange(PromptLevel, String, Range),
}

#[derive(Debug, PartialEq)]
pub enum PromptLevel {
    Info,
    Warning,
    Forbidden,
}
