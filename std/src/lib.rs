//! Scribe standard library runtime
//!
//! This crate provides the native runtime functions that back Scribe's
//! standard library intrinsics.

use std::ffi::{CStr, CString};
use std::fs;
use std::os::raw::c_char;
use std::ptr;

// ============================================================================
// Print functions
// ============================================================================

/// Print a null-terminated string to stdout
///
/// # Safety
/// The caller must ensure `ptr` points to a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn scribe_print(ptr: *const c_char) {
    if ptr.is_null() {
        return;
    }
    let c_str = CStr::from_ptr(ptr);
    if let Ok(s) = c_str.to_str() {
        print!("{}", s);
    }
}

/// Print a null-terminated string to stdout followed by a newline
///
/// # Safety
/// The caller must ensure `ptr` points to a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn scribe_println(ptr: *const c_char) {
    if ptr.is_null() {
        println!();
        return;
    }
    let c_str = CStr::from_ptr(ptr);
    if let Ok(s) = c_str.to_str() {
        println!("{}", s);
    }
}

/// Print a floating point number to stdout
#[no_mangle]
pub extern "C" fn scribe_print_number(value: f64) {
    use std::io::Write;
    print!("{}", value);
    let _ = std::io::stdout().flush();
}

/// Print a floating point number to stdout followed by a newline
#[no_mangle]
pub extern "C" fn scribe_println_number(value: f64) {
    use std::io::Write;
    println!("{}", value);
    let _ = std::io::stdout().flush();
}

// ============================================================================
// File I/O functions
// ============================================================================

/// Read the contents of a file as a string
///
/// Returns a pointer to a null-terminated string allocated with malloc.
/// The caller is responsible for freeing the returned pointer.
/// Returns null on error.
///
/// # Safety
/// The caller must ensure `path` points to a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn scribe_file_read(path: *const c_char) -> *mut c_char {
    if path.is_null() {
        return ptr::null_mut();
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return ptr::null_mut(),
    };
    match fs::read_to_string(path_str) {
        Ok(contents) => match CString::new(contents) {
            Ok(c_string) => c_string.into_raw(),
            Err(_) => ptr::null_mut(),
        },
        Err(_) => ptr::null_mut(),
    }
}

/// Write a string to a file (overwrites existing content)
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
/// The caller must ensure both pointers point to valid null-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn scribe_file_write(path: *const c_char, data: *const c_char) -> i32 {
    if path.is_null() || data.is_null() {
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };
    let data_str = match CStr::from_ptr(data).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };
    match fs::write(path_str, data_str) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// Append a string to a file
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
/// The caller must ensure both pointers point to valid null-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn scribe_file_append(path: *const c_char, data: *const c_char) -> i32 {
    if path.is_null() || data.is_null() {
        return -1;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };
    let data_str = match CStr::from_ptr(data).to_str() {
        Ok(s) => s,
        Err(_) => return -1,
    };
    use std::io::Write;
    match fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(path_str)
    {
        Ok(mut file) => match file.write_all(data_str.as_bytes()) {
            Ok(_) => 0,
            Err(_) => -1,
        },
        Err(_) => -1,
    }
}

/// Check if a file exists
///
/// Returns 1 if the file exists, 0 otherwise.
///
/// # Safety
/// The caller must ensure `path` points to a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn scribe_file_exists(path: *const c_char) -> i32 {
    if path.is_null() {
        return 0;
    }
    let path_str = match CStr::from_ptr(path).to_str() {
        Ok(s) => s,
        Err(_) => return 0,
    };
    if std::path::Path::new(path_str).exists() {
        1
    } else {
        0
    }
}

/// Free a string allocated by scribe_file_read
///
/// # Safety
/// The caller must ensure `ptr` was allocated by scribe_file_read.
#[no_mangle]
pub unsafe extern "C" fn scribe_free_string(ptr: *mut c_char) {
    if !ptr.is_null() {
        let _ = CString::from_raw(ptr);
    }
}

// ============================================================================
// Time functions
// ============================================================================

/// Sleep for the specified number of milliseconds
#[no_mangle]
pub extern "C" fn scribe_time_sleep(milliseconds: f64) {
    let duration = std::time::Duration::from_millis(milliseconds as u64);
    std::thread::sleep(duration);
}

/// Get the current time as milliseconds since Unix epoch
#[no_mangle]
pub extern "C" fn scribe_time_now() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as f64)
        .unwrap_or(0.0)
}

/// Get the current time as seconds since Unix epoch
#[no_mangle]
pub extern "C" fn scribe_time_now_seconds() -> f64 {
    use std::time::{SystemTime, UNIX_EPOCH};
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0)
}

// ============================================================================
// String functions
// ============================================================================

/// Get the length of a string
///
/// # Safety
/// The caller must ensure `ptr` points to a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn scribe_string_length(ptr: *const c_char) -> f64 {
    if ptr.is_null() {
        return 0.0;
    }
    let c_str = CStr::from_ptr(ptr);
    c_str.to_bytes().len() as f64
}

/// Concatenate two strings
///
/// Returns a pointer to a new string. The caller is responsible for freeing it.
///
/// # Safety
/// The caller must ensure both pointers point to valid null-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn scribe_string_concat(
    a: *const c_char,
    b: *const c_char,
) -> *mut c_char {
    if a.is_null() && b.is_null() {
        return ptr::null_mut();
    }
    let a_str = if a.is_null() {
        ""
    } else {
        CStr::from_ptr(a).to_str().unwrap_or("")
    };
    let b_str = if b.is_null() {
        ""
    } else {
        CStr::from_ptr(b).to_str().unwrap_or("")
    };
    let result = format!("{}{}", a_str, b_str);
    match CString::new(result) {
        Ok(s) => s.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

/// Convert a number to a string
///
/// Returns a pointer to a new string. The caller is responsible for freeing it.
#[no_mangle]
pub extern "C" fn scribe_number_to_string(value: f64) -> *mut c_char {
    let s = if value.fract() == 0.0 {
        format!("{}", value as i64)
    } else {
        format!("{}", value)
    };
    match CString::new(s) {
        Ok(c_string) => c_string.into_raw(),
        Err(_) => ptr::null_mut(),
    }
}

/// Parse a string to a number
///
/// Returns the parsed number, or 0.0 on error.
///
/// # Safety
/// The caller must ensure `ptr` points to a valid null-terminated C string.
#[no_mangle]
pub unsafe extern "C" fn scribe_string_to_number(ptr: *const c_char) -> f64 {
    if ptr.is_null() {
        return 0.0;
    }
    let c_str = CStr::from_ptr(ptr);
    c_str
        .to_str()
        .ok()
        .and_then(|s| s.trim().parse::<f64>().ok())
        .unwrap_or(0.0)
}

// ============================================================================
// HTTP functions (when http feature is enabled)
// ============================================================================

/// HTTP response structure for FFI
#[repr(C)]
pub struct ScribeHttpResponse {
    pub status: i32,
    pub body: *mut c_char,
}

/// Perform an HTTP GET request
///
/// Returns a ScribeHttpResponse with status code and body.
/// The body pointer must be freed with scribe_free_string.
/// Returns status -1 on error.
///
/// # Safety
/// The caller must ensure `url` points to a valid null-terminated C string.
#[no_mangle]
#[cfg(feature = "http")]
pub unsafe extern "C" fn scribe_http_get(url: *const c_char) -> ScribeHttpResponse {
    if url.is_null() {
        return ScribeHttpResponse {
            status: -1,
            body: ptr::null_mut(),
        };
    }
    let url_str = match CStr::from_ptr(url).to_str() {
        Ok(s) => s,
        Err(_) => {
            return ScribeHttpResponse {
                status: -1,
                body: ptr::null_mut(),
            }
        }
    };
    match ureq::get(url_str).call() {
        Ok(response) => {
            let status = response.status() as i32;
            let body = response.into_string().unwrap_or_default();
            let body_ptr = match CString::new(body) {
                Ok(s) => s.into_raw(),
                Err(_) => ptr::null_mut(),
            };
            ScribeHttpResponse {
                status,
                body: body_ptr,
            }
        }
        Err(_) => ScribeHttpResponse {
            status: -1,
            body: ptr::null_mut(),
        },
    }
}

/// Perform an HTTP POST request
///
/// # Safety
/// The caller must ensure both pointers point to valid null-terminated C strings.
#[no_mangle]
#[cfg(feature = "http")]
pub unsafe extern "C" fn scribe_http_post(
    url: *const c_char,
    body: *const c_char,
) -> ScribeHttpResponse {
    if url.is_null() || body.is_null() {
        return ScribeHttpResponse {
            status: -1,
            body: ptr::null_mut(),
        };
    }
    let url_str = match CStr::from_ptr(url).to_str() {
        Ok(s) => s,
        Err(_) => {
            return ScribeHttpResponse {
                status: -1,
                body: ptr::null_mut(),
            }
        }
    };
    let body_str = match CStr::from_ptr(body).to_str() {
        Ok(s) => s,
        Err(_) => {
            return ScribeHttpResponse {
                status: -1,
                body: ptr::null_mut(),
            }
        }
    };
    match ureq::post(url_str)
        .set("Content-Type", "application/json")
        .send_string(body_str)
    {
        Ok(response) => {
            let status = response.status() as i32;
            let response_body = response.into_string().unwrap_or_default();
            let body_ptr = match CString::new(response_body) {
                Ok(s) => s.into_raw(),
                Err(_) => ptr::null_mut(),
            };
            ScribeHttpResponse {
                status,
                body: body_ptr,
            }
        }
        Err(_) => ScribeHttpResponse {
            status: -1,
            body: ptr::null_mut(),
        },
    }
}

// Stub implementations when http feature is not enabled
#[no_mangle]
#[cfg(not(feature = "http"))]
pub unsafe extern "C" fn scribe_http_get(_url: *const c_char) -> ScribeHttpResponse {
    ScribeHttpResponse {
        status: -1,
        body: ptr::null_mut(),
    }
}

#[no_mangle]
#[cfg(not(feature = "http"))]
pub unsafe extern "C" fn scribe_http_post(
    _url: *const c_char,
    _body: *const c_char,
) -> ScribeHttpResponse {
    ScribeHttpResponse {
        status: -1,
        body: ptr::null_mut(),
    }
}

/// Marker function to ensure this crate is compiled when referenced
/// This is used to eagerly compile the static library during `cargo build -p scribe-cli`
pub fn build_marker() {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_print_string() {
        let s = CString::new("Hello, Scribe!").unwrap();
        unsafe {
            scribe_println(s.as_ptr());
        }
    }

    #[test]
    fn test_print_number() {
        scribe_println_number(42.0);
        scribe_println_number(3.14159);
    }

    #[test]
    fn test_file_operations() {
        let test_path = CString::new("/tmp/scribe_test.txt").unwrap();
        let test_data = CString::new("Hello from Scribe!").unwrap();

        unsafe {
            // Write file
            let result = scribe_file_write(test_path.as_ptr(), test_data.as_ptr());
            assert_eq!(result, 0);

            // Check exists
            let exists = scribe_file_exists(test_path.as_ptr());
            assert_eq!(exists, 1);

            // Read file
            let contents = scribe_file_read(test_path.as_ptr());
            assert!(!contents.is_null());
            let contents_str = CStr::from_ptr(contents).to_str().unwrap();
            assert_eq!(contents_str, "Hello from Scribe!");
            scribe_free_string(contents);

            // Append to file
            let append_data = CString::new("\nAppended line").unwrap();
            let result = scribe_file_append(test_path.as_ptr(), append_data.as_ptr());
            assert_eq!(result, 0);

            // Clean up
            let _ = std::fs::remove_file("/tmp/scribe_test.txt");
        }
    }
}

