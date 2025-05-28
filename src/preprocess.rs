// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

use anc_isa::{DataSectionType, MemoryDataType, OperandDataType};

use anc_assembly::ast::{
    ArgumentValue, BlockNode, BreakNode, DataNode, DataSection, DataTypeValuePair, DataValue,
    DeclareDataType, ExpressionNode, ExternalDataNode, ExternalFunctionNode, ExternalNode,
    FixedDeclareDataType, FunctionNode, IfNode, ImportDataNode, ImportFunctionNode, ImportNode,
    InstructionNode, LiteralNumber, LocalVariable, ModuleNode, NameValueParameter, NamedArgument,
    NamedParameter, WhenNode,
};

use crate::{
    lexer::lex_from_str,
    location::Location,
    normalizer::{clean, normalize},
    peekableiter::PeekableIter,
    token::{NumberToken, Token, TokenWithRange},
    ParserError,
};

pub const PEEK_BUFFER_LENGTH_PREPROCESS: usize = 4;

// see:
// - https://en.cppreference.com/w/c/language.html
// - https://en.cppreference.com/w/c/preprocessor.html
pub struct Preprocessor<'a> {
    upstream: &'a mut PeekableIter<'a, TokenWithRange>,
    last_range: Location,
}

impl<'a> Preprocessor<'a> {
    fn new(upstream: &'a mut PeekableIter<'a, TokenWithRange>) -> Self {
        Self {
            upstream,
            last_range: Location::new_range(/*0,*/ 0, 0, 0, 0),
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        match self.upstream.next() {
            Some(TokenWithRange { token, range }) => {
                self.last_range = range;
                Some(token)
            }
            None => None,
        }
    }

    fn peek_range(&self, offset: usize) -> Option<&Location> {
        match self.upstream.peek(offset) {
            Some(TokenWithRange { range, .. }) => Some(range),
            None => None,
        }
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        match self.upstream.peek(offset) {
            Some(TokenWithRange { token, .. }) => Some(token),
            None => None,
        }
    }

    fn expect_token(&self, offset: usize, expected_token: &Token) -> bool {
        matches!(
            self.peek_token(offset),
            Some(token) if token == expected_token)
    }

    /// Returns:
    /// - `None` if the specified token is not found.
    /// - `Some(false)` found the token without new-line.
    /// - `Some(true)` found the token and new-line.
    fn expect_token_ignore_newline(&self, offset: usize, expected_token: &Token) -> Option<bool> {
        if self.expect_token(offset, expected_token) {
            Some(false)
        } else if self.expect_token(offset, &Token::NewLine)
            && self.expect_token(offset + 1, expected_token)
        {
            Some(true)
        } else {
            None
        }
    }

    fn expect_keyword(&self, offset: usize, expected_keyword: &str) -> bool {
        matches!(self.peek_token(offset), Some(Token::Keyword(keyword)) if keyword == expected_keyword )
    }

    // /// Returns:
    // /// - `None` if the specified token is not found,
    // /// - `Some(false)` if no new-line is found,
    // /// - `Some(true)` otherwise.
    // fn expect_keyword_ignore_newline(&self, offset: usize, expected_keyword: &str) -> Option<bool> {
    //     if self.expect_keyword(offset, expected_keyword) {
    //         Some(false)
    //     } else if self.expect_token(offset, &Token::NewLine)
    //         && self.expect_keyword(offset + 1, expected_keyword)
    //     {
    //         Some(true)
    //     } else {
    //         None
    //     }
    // }

    // consume '\n' if it exists.
    fn consume_new_line_if_exist(&mut self) -> bool {
        match self.peek_token(0) {
            Some(Token::NewLine) => {
                self.next_token();
                true
            }
            _ => false,
        }
    }

    // consume '\n' or ',' if they exist.
    fn consume_new_line_or_comma_if_exist(&mut self) -> bool {
        match self.peek_token(0) {
            Some(Token::NewLine | Token::Comma) => {
                self.next_token();
                true
            }
            _ => false,
        }
    }

    // fn consume_new_line_or_eof(&mut self) -> Result<(), Error> {
    //     match self.peek_token(0) {
    //         Some(Token::NewLine) => {
    //             self.next_token();
    //             Ok(())
    //         }
    //         Some(_) => Err(PreprocessError::MessageWithPosition(
    //             "Expect a new-line.".to_owned(),
    //             self.peek_range(0).unwrap().get_position_by_range_start(),
    //         )),
    //         None => Ok(()),
    //     }
    // }

    // fn consume_new_line_or_comma(&mut self) -> Result<(), Error> {
    //     match self.peek_token(0) {
    //         Some(Token::NewLine | Token::Comma) => {
    //             self.next_token();
    //             Ok(())
    //         }
    //         Some(_) => Err(PreprocessError::MessageWithPosition(
    //             "Expect a comma or new-line.".to_owned(),
    //             self.peek_range(0).unwrap().get_position_by_range_start(),
    //         )),
    //         None => Err(Error::UnexpectedEndOfDocument(
    //             "Expect a comma or new-line.".to_owned(),
    //         )),
    //     }
    // }

    fn consume_token(
        &mut self,
        expected_token: &Token,
        token_description: &str,
    ) -> Result<(), ParserError> {
        match self.next_token() {
            Some(token) => {
                if &token == expected_token {
                    Ok(())
                } else {
                    Err(PreprocessError::MessageWithPosition(
                        format!("Expect token: {}.", token_description),
                        self.last_range.get_position_by_range_start(),
                    ))
                }
            }
            None => Err(ParserError::UnexpectedEndOfDocument(format!(
                "Expect token: {}.",
                token_description
            ))),
        }
    }

    fn consume_full_name(&mut self) -> Result<String, ParserError> {
        match self.next_token() {
            Some(Token::FullName(s)) => Ok(s),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect a name path.".to_owned(),
                self.last_range.get_position_by_range_start(),
            )),
            None => Err(ParserError::UnexpectedEndOfDocument(
                "Expect a name path.".to_owned(),
            )),
        }
    }

    fn consume_name(&mut self) -> Result<String, ParserError> {
        match self.next_token() {
            Some(Token::Name(s)) => Ok(s),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect a name.".to_owned(),
                self.last_range.get_position_by_range_start(),
            )),
            None => Err(ParserError::UnexpectedEndOfDocument(
                "Expect a name.".to_owned(),
            )),
        }
    }

    fn consume_keyword(&mut self, keyword: &str) -> Result<String, ParserError> {
        match self.next_token() {
            Some(Token::Keyword(s)) if s == keyword => Ok(s),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                format!("Expect keyword \"{}\".", keyword),
                self.last_range.get_position_by_range_start(),
            )),
            None => Err(ParserError::UnexpectedEndOfDocument(format!(
                "Expect keyword \"{}\".",
                keyword
            ))),
        }
    }

    fn consume_number_i32(&mut self) -> Result<u32, ParserError> {
        match self.next_token() {
            Some(Token::Number(NumberToken::I32(n))) => Ok(n),
            Some(_) => Err(PreprocessError::MessageWithPosition(
                "Expect an i32 number.".to_owned(),
                self.last_range.get_position_by_range_start(),
            )),
            None => Err(ParserError::UnexpectedEndOfDocument(
                "Expect an i32 number.".to_owned(),
            )),
        }
    }

    // '('
    fn consume_left_paren(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::LeftParen, "left parenthese")
    }

    // ')'
    fn consume_right_paren(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::RightParen, "right parenthese")
    }

    // '['
    fn consume_left_bracket(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::LeftBracket, "left bracket")
    }

    // ']'
    fn consume_right_bracket(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::RightBracket, "right bracket")
    }

    // '}'
    fn consume_right_brace(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::RightBrace, "right brace")
    }

    // '='
    fn consume_equal(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::Equal, "equal sign")
    }

    // ':'
    fn consume_colon(&mut self) -> Result<(), ParserError> {
        self.consume_token(&Token::Colon, "colon sign")
    }
}

impl Preprocessor<'_> {
    pub fn parse_module_node(&mut self) -> Result<ModuleNode, ParserError> {
        // let mut uses: Vec<UseNode> = vec![];
        let mut imports: Vec<ImportNode> = vec![];
        let mut externals: Vec<ExternalNode> = vec![];
        let mut datas: Vec<DataNode> = vec![];
        let mut functions: Vec<FunctionNode> = vec![];

        while let Some(token) = self.peek_token(0) {
            match token {
                // Token::Keyword(keyword) if keyword == "use" => {
                //     // use statement
                //     uses.push(self.parse_use_node()?);
                // }
                Token::Keyword(keyword) if keyword == "import" => {
                    // import statements
                    imports.push(self.parse_import_node()?);
                }
                Token::Keyword(keyword) if keyword == "external" => {
                    // external statement
                    externals.push(self.parse_external_node()?);
                }
                Token::Keyword(keyword) if keyword == "data" => {
                    // private read-write data statement
                    datas.push(self.parse_data_node(false, DataSectionType::ReadWrite)?);
                }
                Token::Keyword(keyword) if keyword == "fn" => {
                    // private fn statement
                    functions.push(self.parse_function_node(false)?);
                }
                Token::Keyword(keyword) if keyword == "readonly" || keyword == "uninit" => {
                    // private {readonly|uninit} data statement
                    let data_section_type = if keyword == "readonly" {
                        DataSectionType::ReadOnly
                    } else {
                        DataSectionType::Uninit
                    };

                    self.next_token(); // consume 'readonly' or 'uninit'
                    self.consume_new_line_if_exist();

                    datas.push(self.parse_data_node(false, data_section_type)?);
                }
                Token::Keyword(keyword) if keyword == "pub" => {
                    self.next_token(); // consume 'pub'
                    self.consume_new_line_if_exist();

                    if let Some(next_token) = self.peek_token(0) {
                        match next_token {
                            Token::Keyword(next_keyword) if next_keyword == "data" => {
                                // public read-write data statement
                                datas.push(self.parse_data_node(true, DataSectionType::ReadWrite)?);
                            }
                            Token::Keyword(next_keyword) if next_keyword == "fn" => {
                                // public fn statement
                                functions.push(self.parse_function_node(true)?);
                            }
                            Token::Keyword(next_keyword)
                                if next_keyword == "readonly" || next_keyword == "uninit" =>
                            {
                                // public {readonly|uninit} data statement
                                let data_section_type = if next_keyword == "readonly" {
                                    DataSectionType::ReadOnly
                                } else {
                                    DataSectionType::Uninit
                                };

                                self.next_token(); // consume 'readonly' or 'uninit'
                                self.consume_new_line_if_exist();

                                datas.push(self.parse_data_node(true, data_section_type)?);
                            }
                            _ => {
                                return Err(PreprocessError::MessageWithPosition(
                                    "Expect a data or a function.".to_owned(),
                                    self.peek_range(0).unwrap().get_position_by_range_start(),
                                ));
                            }
                        }
                    } else {
                        return Err(ParserError::UnexpectedEndOfDocument(
                            "Expect a data or a function.".to_owned(),
                        ));
                    }
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Unexpected token.".to_owned(),
                        self.peek_range(0).unwrap().get_position_by_range_start(),
                    ));
                }
            }
        }

        let module_node = ModuleNode {
            // full_name: full_name.to_owned(),
            // uses,
            imports,
            externals,
            datas,
            functions,
        };

        Ok(module_node)
    }

    // fn parse_use_node(&mut self) -> Result<UseNode, Error> {
    //     // use ... [as ...] ?  //
    //     // ^                ^__// to here
    //     // |-------------------// current token, validated
    //
    //     self.next_token(); // consume 'use'
    //     self.consume_new_line_if_exist();
    //
    //     let full_name = self.consume_full_name()?;
    //
    //     let alias_name = match self.expect_keyword_ignore_newline(0, "as") {
    //         Some(exists_newline) => {
    //             if exists_newline {
    //                 self.next_token(); // consume '\n'
    //             }
    //             self.next_token(); // consume 'as'
    //             self.consume_new_line_if_exist();
    //
    //             let name = self.consume_name()?;
    //             Some(name)
    //         }
    //         _ => None,
    //     };
    //
    //     self.consume_new_line_if_exist(); // consume '\n'
    //
    //     let node = UseNode {
    //         full_name,
    //         alias_name,
    //     };
    //     Ok(node)
    // }

    fn parse_import_node(&mut self) -> Result<ImportNode, ParserError> {
        // import {fn|data} ... ?  //
        // ^                      ^__// to here
        // |-------------------------// current token, validated

        self.next_token(); // consume 'import'
        self.consume_new_line_if_exist();

        if let Some(token) = self.peek_token(0) {
            match token {
                Token::Keyword(keyword) if keyword == "fn" => {
                    let function_node = self.parse_import_function_node()?;
                    Ok(ImportNode::Function(function_node))
                }
                Token::Keyword(keyword) if keyword == "data" => {
                    let data_node = self.parse_import_data_node(DataSectionType::ReadWrite)?;
                    Ok(ImportNode::Data(data_node))
                }
                Token::Keyword(keyword) if keyword == "readonly" => {
                    self.next_token(); // consume 'readonly'
                    self.consume_new_line_if_exist();

                    let data_node = self.parse_import_data_node(DataSectionType::ReadOnly)?;
                    Ok(ImportNode::Data(data_node))
                }
                Token::Keyword(keyword) if keyword == "uninit" => {
                    self.next_token(); // consume 'uninit'
                    self.consume_new_line_if_exist();

                    let data_node = self.parse_import_data_node(DataSectionType::Uninit)?;
                    Ok(ImportNode::Data(data_node))
                }
                _ => Err(PreprocessError::MessageWithPosition(
                    "Expect import \"fn\" or \"data\".".to_owned(),
                    self.peek_range(0).unwrap().get_position_by_range_start(),
                )),
            }
        } else {
            Err(ParserError::UnexpectedEndOfDocument(
                "Expect import \"fn\" or \"data\".".to_owned(),
            ))
        }
    }

    fn parse_import_function_node(&mut self) -> Result<ImportFunctionNode, ParserError> {
        // fn full_name ()->() [as ...] [from ...]?  //
        // ^                                      ^__// to here
        // |-----------------------------------------// current token, validated

        self.next_token(); // consume 'fn'
        self.consume_new_line_if_exist();

        let full_name = self.consume_full_name()?;
        self.consume_new_line_if_exist();

        // parse the parameters
        let params = self.continue_parse_function_signature_params()?;
        self.consume_new_line_if_exist();

        // parse the return data type
        let results: Vec<OperandDataType> = if self.expect_token(0, &Token::RightArrow) {
            self.next_token(); // consume '->'
            self.consume_new_line_if_exist();

            self.continue_parse_function_results()?
        } else {
            vec![]
        };
        self.consume_new_line_if_exist();

        // parse the 'as' part
        let alias_name = if self.expect_keyword(0, "as") {
            self.next_token(); // consume 'as'
            self.consume_new_line_if_exist();

            let name = self.consume_name()?;
            Some(name)
        } else {
            None
        };
        self.consume_new_line_if_exist();

        // parse the 'from' part
        let from = if self.expect_keyword(0, "from") {
            self.next_token(); // consume 'from'
            self.consume_new_line_if_exist();

            let name = self.consume_name()?;
            Some(name)
        } else {
            None
        };
        self.consume_new_line_if_exist();

        let node = ImportFunctionNode {
            full_name,
            params,
            results,
            alias_name,
            from,
        };

        Ok(node)
    }

    fn continue_parse_function_signature_params(
        &mut self,
    ) -> Result<Vec<OperandDataType>, ParserError> {
        // (type, type, ...) ?  //
        // ^                 ^__// to here
        // |--------------------// current token, NOT validated

        self.consume_left_paren()?; // consume '('
        self.consume_new_line_if_exist();

        let mut params = vec![];
        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightParen {
                break;
            }

            let data_type = self.continue_parse_function_data_type()?;
            params.push(data_type);

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_paren()?; // consume ')'

        Ok(params)
    }

    fn parse_import_data_node(
        &mut self,
        data_section_type: DataSectionType,
    ) -> Result<ImportDataNode, ParserError> {
        // data full_name type data_type [as ...] [from ...] ?  //
        // ^                                                 ^__// to here
        // |----------------------------------------------------// current token, NOT validated

        self.consume_keyword("data")?; // consume 'data'
        self.consume_new_line_if_exist();

        let full_name = self.consume_full_name()?;
        self.consume_new_line_if_exist();

        self.consume_keyword("type")?; // consume keyword "type"
        self.consume_new_line_if_exist();

        let data_type = self.continue_parse_external_data_type()?;
        self.consume_new_line_if_exist();

        // parse the 'as' part
        let alias_name = if self.expect_keyword(0, "as") {
            self.next_token(); // consume 'as'
            self.consume_new_line_if_exist();

            let name = self.consume_name()?;
            Some(name)
        } else {
            None
        };
        self.consume_new_line_if_exist();

        // parse the 'from' part
        let from = if self.expect_keyword(0, "from") {
            self.next_token(); // consume 'from'
            self.consume_new_line_if_exist();

            let name = self.consume_name()?;
            Some(name)
        } else {
            None
        };
        self.consume_new_line_if_exist();

        let node = ImportDataNode {
            data_section_type,
            full_name,
            data_type,
            alias_name,
            from,
        };

        Ok(node)
    }

    fn parse_external_node(&mut self) -> Result<ExternalNode, ParserError> {
        // external {fn|data} ... ?  //
        // ^                      ^__// to here
        // |-------------------------// current token, validated

        self.next_token(); // consume 'external'
        self.consume_new_line_if_exist();

        if let Some(token) = self.peek_token(0) {
            match token {
                Token::Keyword(keyword) if keyword == "fn" => {
                    // external fn ...
                    let function_node = self.parse_external_function_node()?;
                    Ok(ExternalNode::Function(function_node))
                }
                Token::Keyword(keyword) if keyword == "data" => {
                    // external data ...
                    let data_node = self.parse_external_data_node()?;
                    Ok(ExternalNode::Data(data_node))
                }
                _ => Err(PreprocessError::MessageWithPosition(
                    "Expect external \"fn\" or \"data\".".to_owned(),
                    self.peek_range(0).unwrap().get_position_by_range_start(),
                )),
            }
        } else {
            Err(ParserError::UnexpectedEndOfDocument(
                "Expect external \"fn\" or \"data\".".to_owned(),
            ))
        }
    }

    fn parse_external_function_node(&mut self) -> Result<ExternalFunctionNode, ParserError> {
        // fn full_name ()->() [as ...] ?  //
        // ^                            ^__// to here
        // |-------------------------------// current token, validated

        self.next_token(); // consume 'fn'
        self.consume_new_line_if_exist();

        let full_name = self.consume_full_name()?;
        self.consume_new_line_if_exist();

        // parse the parameters

        self.consume_left_paren()?; // consume '('
        self.consume_new_line_if_exist();

        let mut params: Vec<OperandDataType> = vec![];
        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightParen {
                break;
            }

            let data_type = self.continue_parse_function_data_type()?;
            params.push(data_type);

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_paren()?; // consume ')'
        self.consume_new_line_if_exist();

        // parse the return data type
        let return_: Option<OperandDataType> = if self.expect_token(0, &Token::RightArrow) {
            self.next_token(); // consume '->'
            self.consume_new_line_if_exist();

            if self.expect_token(0, &Token::LeftParen) {
                self.next_token(); // consume '('
                self.consume_right_paren()?; // consume ')'
                None
            } else {
                let data_type = self.continue_parse_function_data_type()?;
                Some(data_type)
            }
        } else {
            None
        };
        self.consume_new_line_if_exist();

        // parse the 'as' part
        let alias_name = if self.expect_keyword(0, "as") {
            self.next_token(); // consume 'as'
            self.consume_new_line_if_exist();

            let name = self.consume_name()?;
            Some(name)
        } else {
            None
        };
        self.consume_new_line_if_exist();

        let node = ExternalFunctionNode {
            full_name,
            params,
            result: return_,
            alias_name,
        };

        Ok(node)
    }

    fn continue_parse_function_data_type(&mut self) -> Result<OperandDataType, ParserError> {
        // i32 ?  //
        // ^   ^__// to here
        // |------// current token, DataTypeName, validated
        //
        // also:
        // i64, f32, f64

        let token = self.peek_token(0).unwrap();
        let data_type = match token {
            // the name of data type.
            // e.g. "i64", "i32", "byte"
            // it does not include the type details, such as
            // the length and alignment of byte, e.g. "byte[1024, align=8]".
            Token::DataTypeName(dt) => {
                match dt.as_str() {
                    "i64" => {
                        self.next_token(); // consume i64
                        OperandDataType::I64
                    }
                    "i32" => {
                        self.next_token(); // consume i32
                        OperandDataType::I32
                    }
                    "f64" => {
                        self.next_token(); // consume f64
                        OperandDataType::F64
                    }
                    "f32" => {
                        self.next_token(); // consume f32
                        OperandDataType::F32
                    }
                    _ => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Unsupported data type for function parameters.".to_owned(),
                            self.peek_range(0).unwrap().get_position_by_range_start(),
                        ));
                    }
                }
            }
            _ => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect a valid data type".to_owned(),
                    self.peek_range(0).unwrap().get_position_by_range_start(),
                ));
            }
        };

        Ok(data_type)
    }

    fn parse_external_data_node(&mut self) -> Result<ExternalDataNode, ParserError> {
        // data full_name type data_type [as ...] ?  //
        // ^                                      ^__// to here
        // |-----------------------------------------// current token, validated
        self.next_token(); // consume 'data'
        self.consume_new_line_if_exist();

        let full_name = self.consume_full_name()?;
        self.consume_new_line_if_exist();

        self.consume_keyword("type")?; // consume keyword "type"
        self.consume_new_line_if_exist();

        let data_type = self.continue_parse_external_data_type()?;
        self.consume_new_line_if_exist();

        // parse the 'as' part
        let alias_name = if self.expect_keyword(0, "as") {
            self.next_token(); // consume 'as'
            self.consume_new_line_if_exist();

            let name = self.consume_name()?;
            Some(name)
        } else {
            None
        };
        self.consume_new_line_if_exist();

        let node = ExternalDataNode {
            full_name,
            data_type,
            alias_name,
        };

        Ok(node)
    }

    fn continue_parse_external_data_type(&mut self) -> Result<MemoryDataType, ParserError> {
        // i32 ?  //
        // ^   ^__// to here
        // |------// current token, validated
        //
        // also:
        // - i64, f32, f64
        // - byte[]

        let token = self.peek_token(0).unwrap();
        let data_type = match token {
            // the name of data type.
            // e.g. "i64", "i32", "byte"
            // it does not include the type details, such as
            // the length and alignment of byte, e.g. "byte[1024, align=8]".
            Token::DataTypeName(dt) => {
                match dt.as_str() {
                    "i64" => {
                        self.next_token(); // consume i64
                        MemoryDataType::I64
                    }
                    "i32" => {
                        self.next_token(); // consume i32
                        MemoryDataType::I32
                    }
                    "f64" => {
                        self.next_token(); // consume f64
                        MemoryDataType::F64
                    }
                    "f32" => {
                        self.next_token(); // consume f32
                        MemoryDataType::F32
                    }
                    "byte" => {
                        self.next_token(); // consume 'byte'
                        self.consume_new_line_if_exist();

                        self.consume_left_bracket()?; // consule '['
                        self.consume_new_line_if_exist();

                        self.consume_right_bracket()?; // consume ']'
                        MemoryDataType::Bytes
                    }
                    _ => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Unsupported data type for external data.".to_owned(),
                            self.peek_range(0).unwrap().get_position_by_range_start(),
                        ));
                    }
                }
            }
            _ => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect a valid data type".to_owned(),
                    self.peek_range(0).unwrap().get_position_by_range_start(),
                ));
            }
        };

        Ok(data_type)
    }

    fn parse_data_node(
        &mut self,
        public: bool,
        data_section_type: DataSectionType,
    ) -> Result<DataNode, ParserError> {
        // data name:type = value ?  //
        // ^                      ^__// to here
        // |-------------------------// current token, NOT validated
        //
        // also:
        // - data name:type (for 'uninit' type data section)

        self.consume_keyword("data")?; // consume 'data'
        self.consume_new_line_if_exist();

        let name = self.consume_name()?;
        self.consume_new_line_if_exist();

        self.consume_colon()?; // consume ':'
        self.consume_new_line_if_exist();

        match data_section_type {
            DataSectionType::ReadOnly | DataSectionType::ReadWrite => {
                let data_type = self.continue_parse_declare_data_type()?;
                self.consume_new_line_if_exist();

                // parse value
                self.consume_equal()?;
                self.consume_new_line_if_exist();

                let value = self.continue_parse_data_value()?;

                self.consume_new_line_if_exist(); // consume '\n'

                Ok(DataNode {
                    public,
                    name,
                    data_section: if data_section_type == DataSectionType::ReadOnly {
                        DataSection::ReadOnly(DataTypeValuePair { data_type, value })
                    } else {
                        DataSection::ReadWrite(DataTypeValuePair { data_type, value })
                    },
                })
            }
            DataSectionType::Uninit => {
                let data_type = self.continue_parse_fixed_declare_data_type()?;
                self.consume_new_line_if_exist(); // consume '\n'

                Ok(DataNode {
                    public,
                    name,
                    data_section: DataSection::Uninit(data_type),
                })
            }
        }
    }

    fn continue_parse_declare_data_type(&mut self) -> Result<DeclareDataType, ParserError> {
        // i32 ?  //
        // ^   ^__// to here
        // |------// current token, validated
        //
        // also:
        // - i64, f32, f64
        // - byte[], byte[align=4]
        // - byte[1024], byte[1024, align=8]

        let token = self.peek_token(0).unwrap();
        let data_type = match token {
            // the name of data type.
            // e.g. "i64", "i32", "byte"
            // it does not include the type details, such as
            // the length and alignment of byte, e.g. "byte[1024, align=8]".
            Token::DataTypeName(dt) => {
                match dt.as_str() {
                    "i64" => {
                        self.next_token(); // consume i64
                        DeclareDataType::I64
                    }
                    "i32" => {
                        self.next_token(); // consume i32
                        DeclareDataType::I32
                    }
                    "f64" => {
                        self.next_token(); // consume f64
                        DeclareDataType::F64
                    }
                    "f32" => {
                        self.next_token(); // consume f32
                        DeclareDataType::F32
                    }
                    "byte" => {
                        self.next_token(); // consume 'byte'
                        self.consume_new_line_if_exist();

                        self.consume_left_bracket()?; // consule '['
                        self.consume_new_line_if_exist();

                        if matches!(self.peek_token(0), Some(Token::Number(_))) {
                            // fixed size byte array
                            let length = self.consume_number_i32()? as usize; // consume i32
                            let found_sep = self.consume_new_line_or_comma_if_exist();
                            let align = if found_sep && self.expect_keyword(0, "align") {
                                self.next_token(); // consume 'align'
                                self.consume_new_line_if_exist();

                                self.consume_equal()?; // consume '='
                                self.consume_new_line_if_exist();

                                let align = self.consume_number_i32()? as usize; // consume i32
                                self.consume_new_line_if_exist();

                                Some(align)
                            } else {
                                None
                            };

                            self.consume_right_bracket()?; //  consume ']'

                            DeclareDataType::FixedBytes(length, align)
                        } else if self.expect_keyword(0, "align") {
                            // variable size byte array
                            self.next_token(); // consume 'align'
                            self.consume_new_line_if_exist();

                            self.consume_equal()?; // consume '='
                            self.consume_new_line_if_exist();

                            let align = self.consume_number_i32()? as usize; // consume i32
                            self.consume_new_line_if_exist();

                            self.consume_right_bracket()?; //  consume ']'

                            DeclareDataType::Bytes(Some(align))
                        } else {
                            // variable size byte array

                            self.consume_right_bracket()?; //  consume ']'

                            DeclareDataType::Bytes(None)
                        }
                    }
                    _ => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Unsupported data type for data.".to_owned(),
                            self.peek_range(0).unwrap().get_position_by_range_start(),
                        ));
                    }
                }
            }
            _ => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect a valid data type".to_owned(),
                    self.peek_range(0).unwrap().get_position_by_range_start(),
                ));
            }
        };

        Ok(data_type)
    }

    fn continue_parse_fixed_declare_data_type(
        &mut self,
    ) -> Result<FixedDeclareDataType, ParserError> {
        // i32 ?  //
        // ^   ^__// to here
        // |------// current token, validated
        //
        // also:
        // - i64, f32, f64
        // - byte[1024], byte[1024, align=8]

        let token = self.peek_token(0).unwrap();
        let data_type = match token {
            // the name of data type.
            // e.g. "i64", "i32", "byte"
            // it does not include the type details, such as
            // the length and alignment of byte, e.g. "byte[1024, align=8]".
            Token::DataTypeName(dt) => {
                match dt.as_str() {
                    "i64" => {
                        self.next_token(); // consume i64
                        FixedDeclareDataType::I64
                    }
                    "i32" => {
                        self.next_token(); // consume i32
                        FixedDeclareDataType::I32
                    }
                    "f64" => {
                        self.next_token(); // consume f64
                        FixedDeclareDataType::F64
                    }
                    "f32" => {
                        self.next_token(); // consume f32
                        FixedDeclareDataType::F32
                    }
                    "byte" => {
                        self.next_token(); // consume 'byte'
                        self.consume_new_line_if_exist();

                        self.consume_left_bracket()?; // consule '['
                        self.consume_new_line_if_exist();

                        let length = self.consume_number_i32()? as usize; // consume i32

                        let found_sep = self.consume_new_line_or_comma_if_exist();
                        let align = if found_sep && self.expect_keyword(0, "align") {
                            self.next_token(); // consume 'align'
                            self.consume_new_line_if_exist();

                            self.consume_equal()?; // consume '='
                            self.consume_new_line_if_exist();

                            let align = self.consume_number_i32()? as usize; // consume i32
                            self.consume_new_line_if_exist();

                            Some(align)
                        } else {
                            None
                        };

                        self.consume_right_bracket()?; //  consume ']'

                        FixedDeclareDataType::FixedBytes(length, align)
                    }
                    _ => {
                        return Err(PreprocessError::MessageWithPosition(
                            "Unsupported data type for data.".to_owned(),
                            self.peek_range(0).unwrap().get_position_by_range_start(),
                        ));
                    }
                }
            }
            _ => {
                return Err(PreprocessError::MessageWithPosition(
                    "Expect a valid data type".to_owned(),
                    self.peek_range(0).unwrap().get_position_by_range_start(),
                ));
            }
        };

        Ok(data_type)
    }

    fn continue_parse_data_value(&mut self) -> Result<DataValue, ParserError> {
        // 123 ?  //
        // ^   ^__// to here
        // |------// current token, validated

        // The possible value of data are:
        // - Numbers: includes decimal, hexadecimal, binary, float-point, hex float-point.
        // - Strings: normal string, multiline string, long string, raw string, raw string with hash symbol, auto-trimmed string.
        // - Hex byte data.
        // - List. The element of list can be numbers, strings, hex byte data and list.

        if let Some(token) = self.peek_token(0) {
            let value = match token {
                Token::Number(number_token) => {
                    let value_num = match number_token {
                        NumberToken::I8(v) => DataValue::I8(*v),
                        NumberToken::I16(v) => DataValue::I16(*v),
                        NumberToken::I32(v) => DataValue::I32(*v),
                        NumberToken::I64(v) => DataValue::I64(*v),
                        NumberToken::F32(v) => DataValue::F32(*v),
                        NumberToken::F64(v) => DataValue::F64(*v),
                    };
                    self.next_token(); // consume number token
                    value_num
                }
                Token::String(s) => {
                    let value_string = DataValue::String(s.to_owned());
                    self.next_token(); // consume string token
                    value_string
                }
                Token::HexByteData(d) => {
                    let value_byte_data = DataValue::ByteData(d.to_owned());
                    self.next_token(); // consume hex byte data token
                    value_byte_data
                }
                Token::LeftBracket => {
                    // list
                    self.next_token(); // consume '['
                    self.consume_new_line_if_exist();

                    let mut values: Vec<DataValue> = vec![];

                    while let Some(value_token) = self.peek_token(0) {
                        if value_token == &Token::RightBracket {
                            break;
                        }

                        let value_element = self.continue_parse_data_value()?;
                        values.push(value_element);

                        let found_sep = self.consume_new_line_or_comma_if_exist();
                        if !found_sep {
                            break;
                        }
                    }

                    self.consume_right_bracket()?; // consume ']'

                    DataValue::List(values)
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Expect a data value.".to_owned(),
                        self.peek_range(0).unwrap().get_position_by_range_start(),
                    ))
                }
            };

            Ok(value)
        } else {
            Err(ParserError::UnexpectedEndOfDocument(
                "Expect a data value.".to_owned(),
            ))
        }
    }

    fn parse_function_node(&mut self, public: bool) -> Result<FunctionNode, ParserError> {
        // fn (...) [-> ...] [...] exp ?  //
        // ^                           ^__// to here
        // |------------------------------// current token, validated

        self.next_token(); // consume 'fn'
        self.consume_new_line_if_exist();

        let name = self.consume_name()?;
        self.consume_new_line_if_exist();

        let params = self.continue_parse_function_params()?;
        self.consume_new_line_if_exist();

        let results: Vec<OperandDataType> = if self.expect_token(0, &Token::RightArrow) {
            self.next_token(); // consume '->'
            self.consume_new_line_if_exist();

            self.continue_parse_function_results()?
        } else {
            vec![]
        };
        self.consume_new_line_if_exist();

        let locals: Vec<LocalVariable> = if self.expect_token(0, &Token::LeftBracket) {
            self.continue_parse_function_local_variables()?
        } else {
            vec![]
        };

        self.consume_new_line_if_exist();

        let body = self.parse_expression_node()?;

        self.consume_new_line_if_exist();

        let node = FunctionNode {
            public,
            name,
            params,
            results,
            locals,
            body: Box::new(body),
        };

        Ok(node)
    }

    fn continue_parse_function_params(&mut self) -> Result<Vec<NamedParameter>, ParserError> {
        // (name:type, name:type, ...) ?  //
        // ^                           ^__// to here
        // |------------------------------// current token, NOT validated

        self.consume_left_paren()?; // consume '('
        self.consume_new_line_if_exist();

        let mut params = vec![];
        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightParen {
                break;
            }

            let name = self.consume_name()?;
            self.consume_new_line_if_exist();

            self.consume_colon()?;
            self.consume_new_line_if_exist();

            let data_type = self.continue_parse_function_data_type()?;

            params.push(NamedParameter { name, data_type });

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_paren()?; // consume ')'

        Ok(params)
    }

    fn continue_parse_function_results(&mut self) -> Result<Vec<OperandDataType>, ParserError> {
        // (type, type, ...) ?  //
        // ^                 ^__// to here
        // |------- ------------// current token, NOT validated
        //
        // also:
        // - "()"
        // - "type_name"

        let mut results = vec![];
        if self.expect_token(0, &Token::LeftParen) {
            self.next_token(); // consume '('
            self.consume_new_line_if_exist();

            while let Some(token) = self.peek_token(0) {
                if token == &Token::RightParen {
                    break;
                }

                let data_type = self.continue_parse_function_data_type()?;
                results.push(data_type);

                let found_sep = self.consume_new_line_or_comma_if_exist();
                if !found_sep {
                    break;
                }
            }

            self.consume_right_paren()?; // consume ')'
        } else {
            let data_type = self.continue_parse_function_data_type()?;
            results.push(data_type);
        }

        Ok(results)
    }

    fn continue_parse_function_local_variables(
        &mut self,
    ) -> Result<Vec<LocalVariable>, ParserError> {
        // [name:type, name:type, ...] ?  //
        // ^                           ^__// to here
        // |------------------------------// current token, validated
        //
        // also:
        // - "[]"

        self.next_token(); // consume '['
        self.consume_new_line_if_exist();

        let mut local_variables = vec![];
        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightBracket {
                break;
            }

            let name = self.consume_name()?;
            self.consume_new_line_if_exist();

            self.consume_colon()?;
            self.consume_new_line_if_exist();

            let data_type = self.continue_parse_fixed_declare_data_type()?;

            local_variables.push(LocalVariable { name, data_type });

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_bracket()?; // consume ']'

        Ok(local_variables)
    }

    fn continue_parse_block_param_values(
        &mut self,
    ) -> Result<Vec<NameValueParameter>, ParserError> {
        // (name:type=value, name:type=value, ...) ?  //
        // ^                                       ^__// to here
        // |------------------------------------------// current token, NOT validated

        self.consume_left_paren()?; // consume '('
        self.consume_new_line_if_exist();

        let mut param_values = vec![];
        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightParen {
                break;
            }

            let name = self.consume_name()?;
            self.consume_new_line_if_exist();

            self.consume_colon()?;
            self.consume_new_line_if_exist();

            let data_type = self.continue_parse_function_data_type()?;
            self.consume_new_line_if_exist();

            self.consume_equal()?;
            self.consume_new_line_if_exist();

            let value = self.parse_expression_node()?;

            param_values.push(NameValueParameter {
                name,
                data_type,
                value: Box::new(value),
            });

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_paren()?; // consume ')'

        Ok(param_values)
    }

    fn parse_expression_node(&mut self) -> Result<ExpressionNode, ParserError> {
        // expression ?  //
        // ^          ^__// to here
        // |-------------// current token, NOT validated

        if let Some(token) = self.peek_token(0) {
            let node = match token {
                Token::LeftBrace => {
                    // group
                    let exps = self.parse_group_expression()?;
                    ExpressionNode::Group(exps)
                }
                Token::Keyword(keyword) if keyword == "when" => {
                    // "when" expression
                    let when_node = self.parse_when_expression()?;
                    ExpressionNode::When(when_node)
                }
                Token::Keyword(keyword) if keyword == "if" => {
                    // "if" expression
                    let if_node = self.parse_if_expression()?;
                    ExpressionNode::If(if_node)
                }
                // Token::Keyword(keyword) if keyword == "branch" => {
                //     // "branch" expression
                //     let branch_node = self.parse_branch_expression()?;
                //     ExpressionNode::Branch(branch_node)
                // }
                Token::Keyword(keyword) if keyword == "block" => {
                    // "block" expression
                    let for_node = self.parse_block_expression()?;
                    ExpressionNode::Block(for_node)
                }
                Token::Keyword(keyword) if (keyword == "break" || keyword == "break_fn") => {
                    // "break*" expression
                    let keyword_ref = &keyword.to_owned();
                    let break_node = self.parse_break_expression(keyword_ref)?;
                    ExpressionNode::Break(break_node)
                }
                Token::Keyword(keyword) if (keyword == "recur" || keyword == "recur_fn") => {
                    // "recur*" expression
                    let keyword_ref = &keyword.to_owned();
                    let recur_node = self.parse_break_expression(keyword_ref)?;
                    ExpressionNode::Recur(recur_node)
                }
                Token::Name(_)
                    if self
                        .expect_token_ignore_newline(1, &Token::LeftParen)
                        .is_some() =>
                {
                    // maybe an instruction expression
                    let instruction_node = self.parse_instruction_expression()?;
                    ExpressionNode::Instruction(instruction_node)
                }
                _ => {
                    return Err(PreprocessError::MessageWithPosition(
                        "Expect an expression.".to_owned(),
                        self.peek_range(0).unwrap().get_position_by_range_start(),
                    ));
                }
            };

            Ok(node)
        } else {
            Err(ParserError::UnexpectedEndOfDocument(
                "Expect an expression.".to_owned(),
            ))
        }
    }

    fn parse_instruction_expression(&mut self) -> Result<InstructionNode, ParserError> {
        // name (arg, ...) ?  //
        // ^    ^          ^__// to here
        // |    |
        // |------------------// current token, validated

        let name = self.consume_name()?;
        self.consume_new_line_if_exist();

        let (positional_args, named_args) = self.continue_parse_calling_arguments()?;

        let node = InstructionNode {
            name,
            positional_args,
            named_args,
        };

        Ok(node)
    }

    fn continue_parse_calling_arguments(
        &mut self,
    ) -> Result<(Vec<ArgumentValue>, Vec<NamedArgument>), ParserError> {
        // (arg, ..., name=value, ...) ?  //
        // ^                           ^__// to here
        // |------------------------------// current token, validated

        self.next_token(); // consume '('
        self.consume_new_line_if_exist();

        let mut positional_args: Vec<ArgumentValue> = vec![];
        let mut named_args: Vec<NamedArgument> = vec![];

        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightParen {
                break;
            }

            if matches!(token, Token::Name(_)) && self.expect_token(1, &Token::Equal) {
                // named arg
                let name = self.consume_name()?; // consume the name
                self.consume_new_line_if_exist();

                self.consume_equal()?; // consume '='
                self.consume_new_line_if_exist();

                let value = self.continue_parse_argument_value()?;
                let arg = NamedArgument { name, value };
                named_args.push(arg);
            } else {
                // positional arg
                let value = self.continue_parse_argument_value()?;
                positional_args.push(value);
            }

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_paren()?; // consume ')'

        Ok((positional_args, named_args))
    }

    fn continue_parse_argument_value(&mut self) -> Result<ArgumentValue, ParserError> {
        // 123 ?  //
        // ^   ^__// to here
        // |------// current token, NOT validated

        // The possible value of data are:
        //
        // - Numbers: includes decimal, hexadecimal, binary, float-point, hex float-point.
        // - Identifiers: name of functions or data.
        // - Expression: an expression.
        //
        // The identifier can be is the name of function or data, name path is not allowed.

        if let Some(token) = self.peek_token(0) {
            let value = match token {
                Token::Number(number_token) => {
                    let value_num = match number_token {
                        NumberToken::I8(v) => ArgumentValue::LiteralNumber(LiteralNumber::I8(*v)),
                        NumberToken::I16(v) => ArgumentValue::LiteralNumber(LiteralNumber::I16(*v)),
                        NumberToken::I32(v) => ArgumentValue::LiteralNumber(LiteralNumber::I32(*v)),
                        NumberToken::I64(v) => ArgumentValue::LiteralNumber(LiteralNumber::I64(*v)),
                        NumberToken::F32(v) => ArgumentValue::LiteralNumber(LiteralNumber::F32(*v)),
                        NumberToken::F64(v) => ArgumentValue::LiteralNumber(LiteralNumber::F64(*v)),
                    };
                    self.next_token(); // consume number token
                    value_num
                }
                Token::Name(name)
                    if self
                        .expect_token_ignore_newline(1, &Token::LeftParen)
                        .is_none() =>
                {
                    let identifier = name.to_owned();
                    self.next_token(); // consume name
                    ArgumentValue::Identifier(identifier)
                }
                // Token::FullName(full_name) => {
                //     let identifier = full_name.to_owned();
                //     self.next_token(); // consume full name
                //     ArgumentValue::Identifier(identifier)
                // }
                _ => {
                    let expression_node = self.parse_expression_node()?;
                    ArgumentValue::Expression(Box::new(expression_node))
                }
            };

            Ok(value)
        } else {
            Err(ParserError::UnexpectedEndOfDocument(
                "Expect a value for argument.".to_owned(),
            ))
        }
    }

    fn parse_break_expression(&mut self, keyword: &str) -> Result<BreakNode, ParserError> {
        // break (value0, value1, ...) ?  //
        // ^                           ^__// to here
        // |------------------------------// current token, validated
        //
        // also:
        // - break_fn (value0, value1, ...)
        // - recur (value0, value1, ...)
        // - recur_fn (value0, value1, ...)

        self.next_token(); // consume 'break' or 'recur'
        self.consume_new_line_if_exist();

        let node = if keyword == "break" || keyword == "recur" {
            let args = self.continue_parse_break_arguments()?;
            BreakNode::Break(args)
        } else {
            let args = self.continue_parse_break_arguments()?;
            BreakNode::BreakFn(args)
        };

        Ok(node)
    }

    fn continue_parse_break_arguments(&mut self) -> Result<Vec<ExpressionNode>, ParserError> {
        // (arg, ...) ?  //
        // ^          ^__// to here
        // |-------------// current token, NOT validated

        self.consume_left_paren()?; // consume '('
        self.consume_new_line_if_exist();

        let mut args: Vec<ExpressionNode> = vec![];

        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightParen {
                break;
            }

            let value = self.parse_expression_node()?;
            args.push(value);

            let found_sep = self.consume_new_line_or_comma_if_exist();
            if !found_sep {
                break;
            }
        }

        self.consume_right_paren()?; // consume ')'

        Ok(args)
    }

    fn parse_block_expression(&mut self) -> Result<BlockNode, ParserError> {
        // block param_values -> results [locals] body ?  //
        // ^                                     ^__// to here
        // |----------------------------------------// current token, validated

        self.next_token(); // consume 'block'
        self.consume_new_line_if_exist();

        let (param_values, results) = if self.expect_token(0, &Token::LeftParen) {
            let param_values = self.continue_parse_block_param_values()?;
            self.consume_new_line_if_exist();

            let results: Vec<OperandDataType> = if self.expect_token(0, &Token::RightArrow) {
                self.next_token(); // consume '->'
                self.consume_new_line_if_exist();

                self.continue_parse_function_results()?
            } else {
                vec![]
            };
            self.consume_new_line_if_exist();

            (param_values, results)
        } else {
            (vec![], vec![])
        };

        let locals: Vec<LocalVariable> = if self.expect_token(0, &Token::LeftBracket) {
            self.continue_parse_function_local_variables()?
        } else {
            vec![]
        };

        self.consume_new_line_if_exist();

        let body = self.parse_expression_node()?;

        self.consume_new_line_if_exist();

        let node = BlockNode {
            param_values,
            results,
            locals,
            body: Box::new(body),
        };

        Ok(node)
    }

    fn parse_if_expression(&mut self) -> Result<IfNode, ParserError> {
        // if -> results tesing consequence alternative ?  //
        // ^                                                   ^__// to here
        // |------------------------------------------------------// current token, validated

        self.next_token(); // consume 'if'
        self.consume_new_line_if_exist();

        // let (params, results) = if self.expect_token(0, &Token::LeftParen) {
        //     let params = self.continue_parse_function_params()?;
        //     self.consume_new_line_if_exist();

        let results = if self.expect_token(0, &Token::RightArrow) {
            self.next_token(); // consume '->'
            self.consume_new_line_if_exist();

            self.continue_parse_function_results()?
        } else {
            vec![]
        };
        self.consume_new_line_if_exist();

        //     (params, results)
        // } else {
        //     (vec![], vec![])
        // };

        let testing = self.parse_expression_node()?;
        self.consume_new_line_if_exist();

        let consequence = self.parse_expression_node()?;
        self.consume_new_line_if_exist();

        let alternative = self.parse_expression_node()?;

        let node = IfNode {
            // params,
            results,
            testing: Box::new(testing),
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        };

        Ok(node)
    }

    fn parse_when_expression(&mut self) -> Result<WhenNode, ParserError> {
        // when [locals] testing consequence ?  //
        // ^                                 ^__// to here
        // |------------------------------------// current token, validated

        self.next_token(); // consume 'when'
        self.consume_new_line_if_exist();

        let locals = if self.expect_token(0, &Token::LeftBracket) {
            self.continue_parse_function_local_variables()?
        } else {
            vec![]
        };
        self.consume_new_line_if_exist();

        let testing = self.parse_expression_node()?;
        self.consume_new_line_if_exist();

        let consequence = self.parse_expression_node()?;

        let node = WhenNode {
            testing: Box::new(testing),
            locals,
            consequence: Box::new(consequence),
        };
        Ok(node)
    }

    // fn parse_branch_expression(&mut self) -> Result<BranchNode, ParserError> {
    //     // branch params -> results
    //     // ^   [locals]
    //     // |   {
    //     // |   case
    //     // |        testing
    //     // |        consequence
    //     // |   case
    //     // |        testing
    //     // |        consequence
    //     // |   default
    //     // |        consequence
    //     // |   } ?  //
    //     // |     ^__// to here
    //     // |________// current token, validated
    // }

    fn parse_group_expression(&mut self) -> Result<Vec<ExpressionNode>, ParserError> {
        // {expression ...} ?  //
        // ^                ^__// to here
        // |-------------------// current token, validated

        self.next_token(); // consume '{'
        self.consume_new_line_if_exist();

        let mut expressions = vec![];
        while let Some(token) = self.peek_token(0) {
            if token == &Token::RightBrace {
                break;
            }

            let expression_node = self.parse_expression_node()?;
            expressions.push(expression_node);

            // let found_sep = self.consume_new_line_if_exist();
            // if !found_sep {
            //     break;
            // }

            // the separators which follows the expression are optional
            self.consume_new_line_if_exist();
        }

        self.consume_right_brace()?; // consume '}'

        Ok(expressions)
    }
}

pub fn parse_from_str(source_code: &str) -> Result<ModuleNode, ParserError> {
    let tokens = lex_from_str(source_code)?;
    let clean_tokens = clean(tokens);
    let normalized_tokens = normalize(clean_tokens)?;
    let mut token_iter = normalized_tokens.into_iter();
    let mut peekable_token_iter = PeekableIter::new(&mut token_iter, PEEK_BUFFER_LENGTH_PREPROCESS);
    let mut parser = Preprocessor::new(&mut peekable_token_iter);
    parser.parse_module_node()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;

    use anc_assembly::printer::print_to_string;

    use super::parse_from_str;

    fn format(s: &str) -> String {
        match parse_from_str(s) {
            Ok(module_node) => print_to_string(&module_node),
            Err(parser_error) => panic!("{}", parser_error.with_source(s)),
        }
    }

    //     #[test]
    //     fn test_parse_use_statement() {
    //         assert_eq!(format("use std::memory::copy"), "use std::memory::copy\n\n");
    //
    //         // test 'as'
    //         assert_eq!(
    //             format("use parent::sub_sub_module::some_data as other_data"),
    //             "use parent::sub_sub_module::some_data as other_data\n\n"
    //         );
    //
    //         // test multiple items
    //         assert_eq!(
    //             format(
    //                 "\
    // use module::sub_module::some_func
    // use self::sub_module::some_func"
    //             ),
    //             "\
    // use module::sub_module::some_func
    // use self::sub_module::some_func\n\n"
    //         );
    //
    //         // test line breaks
    //         assert_eq!(
    //             format(
    //                 "\
    // use
    // std::memory::copy
    // as
    // mem_copy"
    //             ),
    //             "use std::memory::copy as mem_copy\n\n"
    //         );
    //     }

    #[test]
    fn test_parse_import_function_statement() {
        assert_eq!(
            format("import fn foo::bar()->()"),
            "import fn foo::bar() -> ()\n\n"
        );

        // test omit return part
        assert_eq!(
            format("import fn foo::bar()"),
            "import fn foo::bar() -> ()\n\n"
        );

        // test with params
        assert_eq!(
            format("import fn foo::add(i32,i32)->i32"),
            "import fn foo::add(i32, i32) -> i32\n\n"
        );

        // test with params and results
        assert_eq!(
            format("import fn foo::div(i64)->(i32,i32)"),
            "import fn foo::div(i64) -> (i32, i32)\n\n"
        );

        // test 'as'
        assert_eq!(
            format("import fn foo::add(i32,i32)->i32 as add_i32"),
            "import fn foo::add(i32, i32) -> i32 as add_i32\n\n"
        );

        // test no results but has 'as'
        assert_eq!(
            format("import fn foo::bar() as baz"),
            "import fn foo::bar() -> () as baz\n\n"
        );

        // test from
        assert_eq!(
            format("import fn foo::bar() from mymod"),
            "import fn foo::bar() -> () from mymod\n\n"
        );

        // test multiple items
        assert_eq!(
            format(
                "\
import fn foo::bar()
import fn foo::add(i32,i32)->i32"
            ),
            "\
import fn foo::bar() -> ()
import fn foo::add(i32, i32) -> i32\n\n"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
import
fn
foo::add
(
i32
i32
)
->
i32
as
add_i32
from
mymod"
            ),
            "import fn foo::add(i32, i32) -> i32 as add_i32 from mymod\n\n"
        );
    }

    #[test]
    fn test_parse_import_data_statement() {
        assert_eq!(
            format("import data foo::count type i32"),
            "import data foo::count type i32\n\n"
        );

        assert_eq!(
            format("import readonly data foo::PI type f32"),
            "import readonly data foo::PI type f32\n\n"
        );

        assert_eq!(
            format("import uninit data foo::table type byte[]"),
            "import uninit data foo::table type byte[]\n\n"
        );

        // test 'as'
        assert_eq!(
            format("import data foo::bar type byte[] as baz"),
            "import data foo::bar type byte[] as baz\n\n"
        );

        // test from
        assert_eq!(
            format("import data foo::count type i32 from mymod"),
            "import data foo::count type i32 from mymod\n\n"
        );

        // test multiple items
        assert_eq!(
            format(
                "\
import readonly data foo::PI type f32
import data foo::bar type byte[] as baz"
            ),
            "\
import readonly data foo::PI type f32
import data foo::bar type byte[] as baz\n\n"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
import
readonly
data
foo::bar
type
byte
[
]
as
baz
from
mymod"
            ),
            "import readonly data foo::bar type byte[] as baz from mymod\n\n"
        );
    }

    #[test]
    fn test_parse_external_function_statement() {
        assert_eq!(
            format("external fn libfoo::bar()->()"),
            "external fn libfoo::bar() -> ()\n\n"
        );

        // test omit return part
        assert_eq!(
            format("external fn libfoo::bar()"),
            "external fn libfoo::bar() -> ()\n\n"
        );

        // test with params
        assert_eq!(
            format("external fn libfoo::add(i32,i32)->i32"),
            "external fn libfoo::add(i32, i32) -> i32\n\n"
        );

        // test 'as'
        assert_eq!(
            format("external fn libfoo::add(i32,i32)->i32 as add_i32"),
            "external fn libfoo::add(i32, i32) -> i32 as add_i32\n\n"
        );

        // test 'as' without return
        assert_eq!(
            format("external fn libfoo::bar() as baz"),
            "external fn libfoo::bar() -> () as baz\n\n"
        );

        // test multiple items
        assert_eq!(
            format(
                "\
external fn libfoo::bar()
external fn libfoo::add(i32,i32)->i32"
            ),
            "\
external fn libfoo::bar() -> ()
external fn libfoo::add(i32, i32) -> i32\n\n"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
external
fn
libfoo::add
(
i32
i32
)
->
i32
as
add_i32"
            ),
            "external fn libfoo::add(i32, i32) -> i32 as add_i32\n\n"
        );
    }

    #[test]
    fn test_parse_external_data_statement() {
        assert_eq!(
            format("external data libfoo::PI type f32"),
            "external data libfoo::PI type f32\n\n"
        );

        // test 'as'
        assert_eq!(
            format("external data libfoo::bar type byte[] as baz"),
            "external data libfoo::bar type byte[] as baz\n\n"
        );

        // test multiple items
        assert_eq!(
            format(
                "\
external data libfoo::PI type f32
external data libfoo::bar type byte[] as baz"
            ),
            "\
external data libfoo::PI type f32
external data libfoo::bar type byte[] as baz\n\n"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
external
data
libfoo::bar
type
byte
[
]
as
baz"
            ),
            "external data libfoo::bar type byte[] as baz\n\n"
        );
    }

    #[test]
    fn test_parse_data_statement() {
        assert_eq!(format("data foo:i32=11"), "data foo:i32 = 11\n\n");

        // section 'readonly'
        assert_eq!(
            format("pub readonly data bar:i32=13"),
            "pub readonly data bar:i32 = 13\n\n"
        );

        // section 'uninit'
        assert_eq!(
            format("pub uninit data baz:i32"),
            "pub uninit data baz:i32\n\n"
        );

        // data type i64
        assert_eq!(format("data bar:i64=17_i64"), "data bar:i64 = 17_i64\n\n");

        // other data types and values
        assert_eq!(
            format(
                r#"
pub data foo1:byte[32] = h"11 13 17 19" // length is 32
pub data foo1:byte[32,align=8] = [0x11_i32, 0x13_i32, 0x17_i32, 0x19_i32] // length is 32
pub data foo2:byte[align=4] = [0x11_i32, 0x13_i32, 0x17_i32, 0x19_i32] // length is 4
pub data foo3:byte[] = "Hello, World!" // length is 13
pub data foo4:byte[] = "Hello, World!\0" // length is 13+1
pub data foo5:byte[] = ["Hello, World!", 0_i8] // length is 13+1""#
            ),
            "\
pub data foo1:byte[32] = h\"11 13 17 19\"
pub data foo1:byte[32, align=8] = [
    17
    19
    23
    25
]
pub data foo2:byte[align=4] = [
    17
    19
    23
    25
]
pub data foo3:byte[] = \"Hello, World!\"
pub data foo4:byte[] = \"Hello, World!\\0\"
pub data foo5:byte[] = [
    \"Hello, World!\"
    0_i8
]

"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
pub
data
foo
:
byte
[
32
align
=
8
]
=
[
11
\"abc\"
    [
        13
        17
    ]
]
"
            ),
            "\
pub data foo:byte[32, align=8] = [
    11
    \"abc\"
    [
        13
        17
    ]
]\n\n"
        );
    }

    #[test]
    fn test_parse_function_statement() {
        assert_eq!(
            format("fn foo() nop()"),
            "\
fn foo() -> ()
    nop()
"
        );

        // with params and return
        assert_eq!(
            format("fn foo(hi:i32,lo:i32)->i64 nop()"),
            "\
fn foo(hi:i32, lo:i32) -> i64
    nop()
"
        );

        // with results
        assert_eq!(
            format("fn foo(n:i64)->(i32,i32) nop()"),
            "\
fn foo(n:i64) -> (i32, i32)
    nop()
"
        );

        // with empty local variable
        assert_eq!(
            format("fn foo()->() [] nop()"),
            "\
fn foo() -> ()
    nop()
"
        );

        // with local variable
        assert_eq!(
            format("fn foo() [a:i32] nop()"),
            "\
fn foo() -> ()
    [a:i32]
    nop()
"
        );

        // with multiple local variables
        assert_eq!(
            format("fn foo() [a:i32,b:byte[16,align=4]] nop()"),
            "\
fn foo() -> ()
    [a:i32, b:byte[16, align=4]]
    nop()
"
        );

        // with instruction expressions
        assert_eq!(
            format(
                "\
fn foo(left:i32,right:i32)-> i32 []
    add_i32(
        local_load_i32s(left),
        local_load_i32s(right),
    )"
            ),
            "\
fn foo(left:i32, right:i32) -> i32
    add_i32(
        local_load_i32s(left),
        local_load_i32s(right))
"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
pub
fn
foo
(
left
:
i32
right
:
i32
)
->
i32
[
abc
:
i32
]
imm_i32
(
11
)"
            ),
            "\
pub fn foo(left:i32, right:i32) -> i32
    [abc:i32]
    imm_i32(11)
"
        );
    }

    #[test]
    fn test_parse_expression_group() {
        assert_eq!(
            format(
                "\
fn foo() {
    imm_i32(11)
    imm_i32(31)
}"
            ),
            "\
fn foo() -> ()
    {
        imm_i32(11)
        imm_i32(31)
    }
"
        );

        // nested group
        assert_eq!(
            format(
                "\
fn foo()
    {
        imm_i32(11)
        {
            imm_i32(31)
        }
    }"
            ),
            "\
fn foo() -> ()
    {
        imm_i32(11)
        {
            imm_i32(31)
        }
    }
"
        );

        // test without line breaks
        assert_eq!(
            format(
                "\
fn foo() {
    imm_i32(11) imm_i32(13) imm_i32(17)
}"
            ),
            "\
fn foo() -> ()
    {
        imm_i32(11)
        imm_i32(13)
        imm_i32(17)
    }
"
        );
    }

    #[test]
    fn test_parse_expression_when() {
        assert_eq!(
            format(
                "\
fn foo()
    when imm_i32(1)
        local_store_i32(imm_i32(11))"
            ),
            "\
fn foo() -> ()
    when
        imm_i32(1)
        local_store_i32(
            imm_i32(11))
"
        );

        // with local variables
        assert_eq!(
            format(
                "\
fn foo()
    when [left:i32,right:i32] imm_i32(1) nop()
"
            ),
            "\
fn foo() -> ()
    when
        [left:i32, right:i32]
        imm_i32(1)
        nop()
"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
fn foo()
    when
    [
    left
    :
    i32
    right
    :
    i32
    ]
    imm_i32
    (
    1
    )
    nop
    (
    )
"
            ),
            "\
fn foo() -> ()
    when
        [left:i32, right:i32]
        imm_i32(1)
        nop()
"
        );
    }

    #[test]
    fn test_parse_expression_if() {
        assert_eq!(
            format(
                "\
fn foo()
    if
        eqz_i32(local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
        "
            ),
            "\
fn foo() -> ()
    if -> ()
        eqz_i32(
            local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
"
        );

        // with params and return values
        assert_eq!(
            format(
                "\
fn foo()
    if -> (i32,i32)
        eqz_i32(local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
        "
            ),
            "\
fn foo() -> ()
    if -> (i32, i32)
        eqz_i32(
            local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
"
        );

        // without params
        assert_eq!(
            format(
                "\
fn foo()
    if ->i32
        eqz_i32(local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
        "
            ),
            "\
fn foo() -> ()
    if -> i32
        eqz_i32(
            local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
"
        );

        // without params and return values
        assert_eq!(
            format(
                "\
fn foo()
    if eqz_i32(local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
        "
            ),
            "\
fn foo() -> ()
    if -> ()
        eqz_i32(
            local_load_i32_s(num))
        imm_i32(11)
        imm_i32(13)
"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
fn
foo()
if
->
(
i32
i32
)
imm_i32(11)
imm_i32(13)
imm_i32(17)
        "
            ),
            "\
fn foo() -> ()
    if -> (i32, i32)
        imm_i32(11)
        imm_i32(13)
        imm_i32(17)
"
        );
    }

    #[test]
    fn test_parse_expression_block() {
        assert_eq!(
            format(
                "\
fn foo()
    block (num:i32=
        data_load_extend_i32_s(buffer, local_load_i32_s(offset))
        )
        imm_i32(11)
        "
            ),
            "\
fn foo() -> ()
    block (num:i32=data_load_extend_i32_s(buffer,
        local_load_i32_s(offset))) -> ()
        imm_i32(11)
"
        );

        // with params and return values
        assert_eq!(
            format(
                "\
fn foo()
    block (left:i32=imm_i32(11), right:i32=imm_i32(13))->i32
        imm_i32(11)
        "
            ),
            "\
fn foo() -> ()
    block (left:i32=imm_i32(11), right:i32=imm_i32(13)) -> i32
        imm_i32(11)
"
        );

        // without params
        assert_eq!(
            format(
                "\
fn foo()
    block ()->i32
        imm_i32(11)
        "
            ),
            "\
fn foo() -> ()
    block () -> i32
        imm_i32(11)
"
        );

        // omits params and return values
        assert_eq!(
            format(
                "\
fn foo()
    block imm_i32(11)
        "
            ),
            "\
fn foo() -> ()
    block () -> ()
        imm_i32(11)
"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
fn foo()
block
(
num
:
i32
=
imm_i32
(
11
)
)
->
i32
imm_i32
(
13
)
"
            ),
            "\
fn foo() -> ()
    block (num:i32=imm_i32(11)) -> i32
        imm_i32(13)
"
        );
    }

    #[test]
    fn test_parse_expression_break() {
        assert_eq!(
            format(
                "\
fn foo()
    block {
        break(imm_i32(11), imm_i32(13))
        break_fn(imm_i32(29))
    }"
            ),
            "\
fn foo() -> ()
    block () -> ()
        {
            break(
                imm_i32(11)
                imm_i32(13)
            )
            break_fn(
                imm_i32(29)
            )
        }
"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
fn foo()
block
break
(
imm_i32
(
17
)
imm_i32
(
23
)
)
    "
            ),
            "\
fn foo() -> ()
    block () -> ()
        break(
            imm_i32(17)
            imm_i32(23)
        )
"
        );
    }

    #[test]
    fn test_parse_expression_recur() {
        assert_eq!(
            format(
                "\
fn foo()
    block {
        recur(imm_i32(11), imm_i32(13))
        recur_fn(imm_i32(29))
    }"
            ),
            "\
fn foo() -> ()
    block () -> ()
        {
            recur(
                imm_i32(11)
                imm_i32(13)
            )
            recur_fn(
                imm_i32(29)
            )
        }
"
        );

        // test line breaks
        assert_eq!(
            format(
                "\
fn foo()
block
recur
(
imm_i32
(
17
)
imm_i32
(
23
)
)
    "
            ),
            "\
fn foo() -> ()
    block () -> ()
        recur(
            imm_i32(17)
            imm_i32(23)
        )
"
        );
    }
}
