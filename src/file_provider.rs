// Copyright (c) 2025 Hemashushu <hippospark@gmail.com>, All rights reserved.
//
// This Source Code Form is subject to the terms of
// the Mozilla Public License version 2.0 and additional exceptions.
// For more details, see the LICENSE, LICENSE.additional, and CONTRIBUTING files.

pub trait FileProvider {
    fn resolve_user_file(&self, file_path: &str) -> Option<String>;
    fn resolve_system_file(&self, file_path: &str) -> Option<String>;
    fn load_file(&self, canonical_path: &str) -> Result<String, std::io::Error>;
}
