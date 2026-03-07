// SPDX-FileCopyrightText: 2024 The Forester Project Contributors
// SPDX-License-Identifier: GPL-3.0-or-later

//! FFI interface for OCaml integration
//!
//! This module provides a C-compatible interface for calling the Rust parser
//! from OCaml. Communication is done via JSON serialization of the AST.

use std::ffi::{CStr, CString};
use std::os::raw::c_char;

use crate::json::{ErrorInfo, ParseResult};
use crate::{parse_with_mode, ParseError, ParseMode};

fn parse_result_json(input_str: &str, filename: Option<&str>, mode: ParseMode) -> ParseResult {
    let filename_str = filename.unwrap_or("<input>");
    let mut result = parse_with_mode(input_str, mode);

    if let Some(source_path) = filename {
        if let Some(document) = result.output.take() {
            result.output = Some(document.with_source_path(source_path.to_string()));
        }
    }

    ParseResult::from_recovery_result(result, mode, filename_str, input_str)
}

fn error_json(message: &str, mode: ParseMode) -> String {
    let error = ParseError::Custom {
        message: message.to_string(),
        span: 0..0,
    };
    let result = ParseResult::Error {
        mode,
        errors: vec![ErrorInfo::from_error(&error, "<input>", "")],
    };

    serde_json::to_string(&result).unwrap()
}

unsafe fn decode_parse_mode(mode: *const c_char) -> Result<ParseMode, String> {
    if mode.is_null() {
        return Ok(ParseMode::Strict);
    }

    let mode_cstr = unsafe { CStr::from_ptr(mode) };
    let mode_str = mode_cstr
        .to_str()
        .map_err(|_| "Invalid UTF-8 parse mode".to_string())?;

    mode_str.parse()
}

/// Parse input and return JSON result
///
/// # Safety
/// - `input` must be a valid null-terminated UTF-8 string
/// - The returned string must be freed with `rust_parser_free_string`
#[no_mangle]
pub unsafe extern "C" fn rust_parser_parse(input: *const c_char) -> *mut c_char {
    unsafe { rust_parser_parse_with_filename_and_mode(input, std::ptr::null(), std::ptr::null()) }
}

/// Parse input and return JSON result using an explicit parse mode.
///
/// # Safety
/// - `input` and `mode` must be valid null-terminated UTF-8 strings when non-null
/// - The returned string must be freed with `rust_parser_free_string`
#[no_mangle]
pub unsafe extern "C" fn rust_parser_parse_with_mode(
    input: *const c_char,
    mode: *const c_char,
) -> *mut c_char {
    unsafe { rust_parser_parse_with_filename_and_mode(input, std::ptr::null(), mode) }
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
    unsafe { rust_parser_parse_with_filename_and_mode(input, filename, std::ptr::null()) }
}

/// Parse input and return JSON result (with filename and explicit mode).
///
/// # Safety
/// - `input`, `filename`, and `mode` must be valid null-terminated UTF-8 strings when non-null
/// - The returned string must be freed with `rust_parser_free_string`
#[no_mangle]
pub unsafe extern "C" fn rust_parser_parse_with_filename_and_mode(
    input: *const c_char,
    filename: *const c_char,
    mode: *const c_char,
) -> *mut c_char {
    let parse_mode = match unsafe { decode_parse_mode(mode) } {
        Ok(parse_mode) => parse_mode,
        Err(message) => {
            return CString::new(error_json(&message, ParseMode::Strict))
                .unwrap()
                .into_raw()
        }
    };

    let c_str = unsafe { CStr::from_ptr(input) };
    let input_str = match c_str.to_str() {
        Ok(s) => s,
        Err(_) => {
            let json = error_json("Invalid UTF-8 input", parse_mode);
            return CString::new(json).unwrap().into_raw();
        }
    };

    let filename_str = if filename.is_null() {
        None
    } else {
        let filename_c = unsafe { CStr::from_ptr(filename) };
        Some(filename_c.to_str().unwrap_or("<unknown>"))
    };

    let result = parse_result_json(input_str, filename_str, parse_mode);
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
            let json: serde_json::Value = serde_json::from_str(result_str).expect("parse ffi json");
            assert_eq!(json["status"], "ok");
            assert_eq!(json["mode"], "strict");
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
            let json: serde_json::Value = serde_json::from_str(result_str).expect("parse ffi json");
            assert_eq!(json["status"], "error");
            assert_eq!(json["mode"], "strict");
            assert!(json["errors"][0].get("report").is_some());
            rust_parser_free_string(result);
        }
    }

    #[test]
    fn test_ffi_parse_recovery_mode() {
        let input = CString::new("\\p{]}\n\\p{tail}").unwrap();
        let mode = CString::new("recovery").unwrap();
        unsafe {
            let result = rust_parser_parse_with_mode(input.as_ptr(), mode.as_ptr());
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let json: serde_json::Value = serde_json::from_str(result_str).expect("parse ffi json");
            assert_eq!(json["status"], "recovered");
            assert_eq!(json["mode"], "recovery");
            assert!(json.get("document").is_some());
            assert!(json.get("errors").is_some());
            rust_parser_free_string(result);
        }
    }

    #[test]
    fn test_ffi_parse_invalid_mode() {
        let input = CString::new("\\title{Hello}").unwrap();
        let mode = CString::new("bogus").unwrap();
        unsafe {
            let result = rust_parser_parse_with_mode(input.as_ptr(), mode.as_ptr());
            let result_str = CStr::from_ptr(result).to_str().unwrap();
            let json: serde_json::Value = serde_json::from_str(result_str).expect("parse ffi json");
            assert_eq!(json["status"], "error");
            assert!(json["errors"][0]["message"]
                .as_str()
                .expect("error message")
                .contains("Unknown parse mode"));
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
