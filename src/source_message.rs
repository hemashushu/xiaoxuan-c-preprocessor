// Copyright (c) 2026 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

// A source snippet and the highlighted range within it, measured in characters.
//
// ```diagram
//                 /-- snippet offset in source text (snippet is a substring of source text)
//                 |
//                 |            |-- snippet length
//                 v            v
// prefix -->   ...sni[ppet]_text...  <-- suffix
//                     ^^^^
//                     |  |-- highlight length
//                     |
//                     \----- highlight offset in snippet text
// ```
struct SnippetAndIndication {
    prefix_ellipsis: bool, // Whether the snippet should start with an ellipsis.
    suffix_ellipsis: bool, // Whether the snippet should end with an ellipsis.
    snippet_offset_in_source: usize, // The starting index of the snippet in the source text.
    snippet_length: usize, // The length of the snippet in the source text.
    highlight_offset_in_snippet: usize, // The offset of the highlight within the snippet.
    highlight_length: usize, // The length of the highlight within the snippet.
}

/// Selects a source snippet and clamps the highlight to its visible characters.
fn calculate_snippet_and_indication(
    highlight_text_start_in_source: usize,
    highlight_text_length: usize,
    source_text_length: usize,
) -> SnippetAndIndication {
    // ```diagram
    //
    //     |-- leading text       |-- trailing text
    //     v                      v
    // |--------------|xxxxx|-----------------| <-- snippet text
    //                   ^
    //                   |-- highlight text
    // ```
    const LEADING_LENGTH: usize = 15;
    const SNIPPET_LENGTH: usize = 40;

    let (prefix_ellipsis, snippet_offset_in_source, highlight_offset_in_snippet) =
        if source_text_length < SNIPPET_LENGTH || highlight_text_start_in_source < LEADING_LENGTH {
            (false, 0, highlight_text_start_in_source)
        } else if highlight_text_start_in_source + SNIPPET_LENGTH > source_text_length {
            (
                true,
                source_text_length - SNIPPET_LENGTH,
                highlight_text_start_in_source - (source_text_length - SNIPPET_LENGTH),
            )
        } else {
            (
                true,
                highlight_text_start_in_source - LEADING_LENGTH,
                LEADING_LENGTH,
            )
        };

    let (suffix_ellipsis, snippet_length) =
        if snippet_offset_in_source + SNIPPET_LENGTH >= source_text_length {
            (false, source_text_length - snippet_offset_in_source)
        } else {
            (true, SNIPPET_LENGTH)
        };

    let highlight_length = if highlight_offset_in_snippet + highlight_text_length > snippet_length {
        snippet_length - highlight_offset_in_snippet
    } else {
        highlight_text_length
    };

    SnippetAndIndication {
        prefix_ellipsis,
        suffix_ellipsis,
        snippet_offset_in_source,
        snippet_length,
        highlight_offset_in_snippet,
        highlight_length,
    }
}

/// Builds the snippet line and caret-marker line for an error or lint message.
fn attach_with_snippet(
    source_chars: &mut dyn Iterator<Item = char>,
    snippet_and_indication: &SnippetAndIndication,
    message: &str,
) -> (
    /* snippet line */ String,
    /* indication line */ String,
) {
    // Build the first line: the snippet
    let mut snippet = String::new();
    snippet.push_str("| ");

    if snippet_and_indication.prefix_ellipsis {
        snippet.push_str("...");
    }

    let selection_chars = source_chars
        .skip(snippet_and_indication.snippet_offset_in_source)
        .take(snippet_and_indication.snippet_length);
    let selection_string = selection_chars
        .map(|c| match c {
            '\n' => ' ',
            '\t' => ' ',
            _ => c,
        })
        .collect::<String>();
    snippet.push_str(&selection_string);

    if snippet_and_indication.suffix_ellipsis {
        snippet.push_str("...");
    }

    // Build the second line: the highlight indication
    let mut indication = String::new();
    indication.push_str("| ");

    if snippet_and_indication.prefix_ellipsis {
        indication.push_str("   ");
    }

    indication.push_str(&" ".repeat(snippet_and_indication.highlight_offset_in_snippet));
    indication.push('^');
    if snippet_and_indication.highlight_length > 0 {
        indication.push_str(&"^".repeat(snippet_and_indication.highlight_length - 1));
    }

    indication.push_str("___");
    indication.push(' ');
    indication.push_str(message);

    (snippet, indication)
}

pub fn attach_with_snippet_by_position(
    source_text_content: &str,
    highlight_start_index: usize,
    message: &str,
) -> String {
    let source_text_length = source_text_content.chars().count();
    let mut chars = source_text_content.chars();
    let snippet_and_indication =
        calculate_snippet_and_indication(highlight_start_index, 0, source_text_length);
    let (snippet_line, indication_line) =
        attach_with_snippet(&mut chars, &snippet_and_indication, message);
    format!("{}\n{}", snippet_line, indication_line)
}

pub fn attach_with_snippet_by_last_position(source_text_content: &str, message: &str) -> String {
    let source_text_length = source_text_content.chars().count();
    let mut chars = source_text_content.chars();
    let snippet_and_indication =
        calculate_snippet_and_indication(source_text_length, 0, source_text_length);
    let (snippet_line, indication_line) =
        attach_with_snippet(&mut chars, &snippet_and_indication, message);
    format!("{}\n{}", snippet_line, indication_line)
}

pub fn attach_with_snippet_by_range(
    source_text_content: &str,
    highlight_start_index: usize,
    highlight_length: usize,
    message: &str,
) -> String {
    let source_text_length = source_text_content.chars().count();
    let mut chars = source_text_content.chars();
    let snippet_and_indication = calculate_snippet_and_indication(
        highlight_start_index,
        highlight_length,
        source_text_length,
    );
    let (snippet_line, indication_line) =
        attach_with_snippet(&mut chars, &snippet_and_indication, message);
    format!("{}\n{}", snippet_line, indication_line)
}

#[cfg(test)]
mod tests {
    use crate::source_message::{
        attach_with_snippet_by_last_position, attach_with_snippet_by_position,
        attach_with_snippet_by_range,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn test_attach_with_snippet_by_position() {
        let source1 = "0123456789"; // 10 chars
        let source2 = "012345678_b12345678_c12345678_d12345678_e123456789"; // 50 chars
        let msg = "abcde";

        // position at the first character

        assert_eq!(
            attach_with_snippet_by_position(source1, 0, msg),
            "\
| 0123456789
| ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_position(source2, 0, msg),
            "\
| 012345678_b12345678_c12345678_d12345678_...
| ^___ abcde"
        );

        // position at the head

        assert_eq!(
            attach_with_snippet_by_position(source1, 2, msg),
            "\
| 0123456789
|   ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_position(source2, 15, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|         ^___ abcde"
        );

        // position at the body

        assert_eq!(
            attach_with_snippet_by_position(source1, 5, msg),
            "\
| 0123456789
|      ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_position(source2, 25, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                   ^___ abcde"
        );

        // position at the tail

        assert_eq!(
            attach_with_snippet_by_position(source1, 8, msg),
            "\
| 0123456789
|         ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_position(source2, 45, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                                       ^___ abcde"
        );

        // position at the last character

        assert_eq!(
            attach_with_snippet_by_position(source1, 10, msg),
            "\
| 0123456789
|           ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_position(source2, 50, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                                            ^___ abcde"
        );
    }

    #[test]
    fn test_attach_with_snippet_by_last_position() {
        let source1 = "0123456789"; // 10 chars
        let source2 = "012345678_b12345678_c12345678_d12345678_e123456789"; // 50 chars
        let msg = "abcde";

        assert_eq!(
            attach_with_snippet_by_last_position(source1, msg),
            "\
| 0123456789
|           ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_last_position(source2, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                                            ^___ abcde"
        );
    }

    #[test]
    fn test_attach_with_snippet_by_range() {
        let source1 = "0123456789"; // 10 chars
        let source2 = "012345678_b12345678_c12345678_d12345678_e123456789"; // 50 chars
        let msg = "abcde";

        // range at the first character

        assert_eq!(
            attach_with_snippet_by_range(source1, 0, 4, msg),
            "\
| 0123456789
| ^^^^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_range(source2, 0, 8, msg),
            "\
| 012345678_b12345678_c12345678_d12345678_...
| ^^^^^^^^___ abcde"
        );

        // range at the head

        assert_eq!(
            attach_with_snippet_by_range(source1, 2, 4, msg),
            "\
| 0123456789
|   ^^^^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_range(source2, 15, 8, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|         ^^^^^^^^___ abcde"
        );

        // range at the body

        assert_eq!(
            attach_with_snippet_by_range(source1, 5, 4, msg),
            "\
| 0123456789
|      ^^^^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_range(source2, 25, 8, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                   ^^^^^^^^___ abcde"
        );

        // range at the tail

        assert_eq!(
            attach_with_snippet_by_range(source1, 8, 4, msg),
            "\
| 0123456789
|         ^^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_range(source2, 45, 8, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                                       ^^^^^___ abcde"
        );

        // range at the last character

        assert_eq!(
            attach_with_snippet_by_range(source1, 10, 4, msg),
            "\
| 0123456789
|           ^___ abcde"
        );

        assert_eq!(
            attach_with_snippet_by_range(source2, 50, 8, msg),
            "\
| ...b12345678_c12345678_d12345678_e123456789
|                                            ^___ abcde"
        );
    }
}
