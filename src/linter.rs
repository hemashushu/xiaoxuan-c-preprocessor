// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use std::fmt::Display;

use crate::{
    position::Position,
    range::Range,
    source_message::{attach_with_snippet_by_position, attach_with_snippet_by_range},
};

/// A nonfatal diagnostic, such as a warning or suggestion.
/// Unlike a preprocessing error, a lint does not stop processing and can be
/// suppressed by its identifier.
#[derive(Debug, PartialEq)]
pub enum Lint {
    Message(
        /* lint_level */ LintLevel,
        /* suppress_name */ String,
        /* file_number */ usize,
        /* message */ String,
    ),
    MessageWithPosition(
        /* lint_level */ LintLevel,
        /* suppress_name */ String,
        /* file_number */ usize,
        /* message */ String,
        Position,
    ),
    MessageWithRange(
        /* lint_level */ LintLevel,
        /* suppress_name */ String,
        /* file_number */ usize,
        /* message */ String,
        Range,
    ),
}

/// Severity of a nonfatal diagnostic: information or a warning.
///
/// Unsupported constructs and uses of disabled options are reported separately
/// as preprocessing errors, not additional lint levels.
#[derive(Debug, PartialEq)]
pub enum LintLevel {
    /// An informational message or suggestion, such as a redundant definition.
    Info,

    /// A warning about a potential issue or an explicit #warning directive.
    Warn,
}

impl Display for LintLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintLevel::Info => write!(f, "Info"),
            LintLevel::Warn => write!(f, "Warn"),
        }
    }
}

impl Lint {
    /// Formats the lint, adding a source snippet when a position or range exists.
    /// The caller supplies the path and source text. For included files, these
    /// can be looked up by file number in `HeaderFileCache`.
    pub fn source_message(
        &self,
        source_file_canonical_full_path: &str,
        source_text_content: &str,
    ) -> String {
        match self {
            Lint::Message(lint_level, suppress_name, _, msg) => {
                let title = format!("{}: {}", lint_level, msg);
                let file = format!("File: {}", source_file_canonical_full_path);
                let name = format!("Lint: {}", suppress_name);

                format!("{}\n{}\n{}\n", title, file, name)
            }
            Lint::MessageWithPosition(lint_level, suppress_name, _, msg, position) => {
                let title = format!("{}:", lint_level);
                let message =
                    attach_with_snippet_by_position(source_text_content, position.index, msg);
                let location = format!(
                    "Position: line {}, column {}",
                    position.line + 1,
                    position.column + 1
                );
                let file = format!("File: {}", source_file_canonical_full_path);
                let name = format!("Lint: {}", suppress_name);

                format!("{}\n{}\n{}\n{}\n{}\n", title, message, location, file, name)
            }
            Lint::MessageWithRange(lint_level, suppress_name, _, msg, range) => {
                let title = format!("{}:", lint_level);

                let message = attach_with_snippet_by_range(
                    source_text_content,
                    range.start.index,
                    range.end_inclusive.index - range.start.index + 1,
                    msg,
                );

                let location = format!(
                    "Position: line {}, column {}",
                    range.start.line + 1,
                    range.start.column + 1,
                );
                let file = format!("File: {}", source_file_canonical_full_path);
                let name = format!("Lint: {}", suppress_name);
                format!("{}\n{}\n{}\n{}\n{}\n", title, message, location, file, name)
            }
        }
    }
}
