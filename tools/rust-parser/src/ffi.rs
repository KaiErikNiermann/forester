// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! FFI interface for OCaml integration
//!
//! This module provides a C-compatible interface for calling the Rust parser
//! from OCaml. Communication is done via JSON serialization of the AST.

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use crate::json::{ErrorInfo, ParseResult};
use crate::parse;

/// Parse input and return JSON result
///
/// # Safety
/// - `input` must be a valid null-terminated UTF-8 string
/// - The returned string must be freed with `rust_parser_free_string`
#[no_mangle]
pub unsafe extern "C" fn rust_parser_parse(input: *const c_char) -> *mut c_char {
    rust_parser_parse_with_filename(input, std::ptr::null())
}

/// Parse input and return JSON result (with filename for error reporting)
///
/// # Safety
/// - `input` and `filename` must be valid null-terminated UTF-8 strings
/// - The returned string must be freed with `rust_parser_free_string`
#[no_mangle]
pub unsafe extern "C" fn rust_parser_parse_with_filename(
    input: *const c_char,
    filename: *const c_char,
) -> *mut c_char {
    let c_str = unsafe { CStr::from_ptr(input) };
    let input_str = match c_str.to_str() {
        Ok(s) => s,
        Err(_) => {
            let result = ParseResult::Error {
                errors: vec![ErrorInfo {
                    message: "Invalid UTF-8 input".to_string(),
                    start_offset: 0,
                    end_offset: 0,
                    report: "Error: Invalid UTF-8 input".to_string(),
                }],
            };
            let json = serde_json::to_string(&result).unwrap();
            return CString::new(json).unwrap().into_raw();
        }
    };

    let filename_str = if filename.is_null() {
        "<input>"
    } else {
        let filename_c = unsafe { CStr::from_ptr(filename) };
        filename_c.to_str().unwrap_or("<unknown>")
    };

    let result = match parse(input_str) {
        Ok(doc) if filename.is_null() => ParseResult::Ok { document: doc },
        Ok(doc) => ParseResult::Ok {
            document: doc.with_source_path(filename_str.to_string()),
        },
        Err(errors) => ParseResult::from_parse_result(Err(errors), filename_str, input_str),
    };

    let json = serde_json::to_string(&result).unwrap();
    CString::new(json).unwrap().into_raw()
}

/// Free a string returned by the parser
///
/// # Safety
/// - `s` must be a pointer returned by `rust_parser_parse` or similar
/// - Must not be called more than once for the same pointer
#[no_mangle]
pub unsafe extern "C" fn rust_parser_free_string(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            drop(CString::from_raw(s));
        }
    }
}

/// Get version string
///
/// # Safety
/// - The returned string must be freed with `rust_parser_free_string`
#[no_mangle]
pub extern "C" fn rust_parser_version() -> *mut c_char {
    let version = env!("CARGO_PKG_VERSION");
    CString::new(version).unwrap().into_raw()
}

/// Check if the Rust parser is available (always returns 1)
#[no_mangle]
pub extern "C" fn rust_parser_available() -> i32 {
    1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_ffi_parse() {
        let input = CString::new("\\title{Hello}").unwrap();
        unsafe {
            let result = rust_parser_parse(input.as_ptr());
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            assert!(result_str.contains("\"status\":\"ok\""));
            rust_parser_free_string(result);
        }
    }

    #[test]
    fn test_ffi_parse_error() {
        let input = CString::new("\\title{").unwrap();
        let filename = CString::new("test.tree").unwrap();
        unsafe {
            let result = rust_parser_parse_with_filename(input.as_ptr(), filename.as_ptr());
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            assert!(result_str.contains("\"status\":\"error\""));
            assert!(result_str.contains("\"report\":")); // Contains ariadne report
            rust_parser_free_string(result);
        }
    }

    #[test]
    fn test_ffi_version() {
        unsafe {
            let version = rust_parser_version();
            let version_str = CStr::from_ptr(version).to_str().unwrap();
            assert!(!version_str.is_empty());
            rust_parser_free_string(version);
        }
    }
}
