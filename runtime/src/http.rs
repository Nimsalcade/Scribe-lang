//! HTTP client provider for Scribe runtime

use futures::future::BoxFuture;
use std::collections::HashMap;
use std::sync::Arc;

/// HTTP response structure
#[derive(Debug, Clone)]
pub struct HttpResponse {
    pub status: u16,
    pub headers: HashMap<String, String>,
    pub body: String,
}

impl HttpResponse {
    pub fn new(status: u16, body: String) -> Self {
        Self {
            status,
            headers: HashMap::new(),
            body,
        }
    }

    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    pub fn is_success(&self) -> bool {
        (200..300).contains(&self.status)
    }
}

/// HTTP client provider trait
pub trait HttpProvider: Send + Sync + 'static {
    /// Perform a GET request
    fn get(&self, url: &str) -> BoxFuture<'static, Result<HttpResponse, String>>;

    /// Perform a POST request with a body
    fn post(&self, url: &str, body: String) -> BoxFuture<'static, Result<HttpResponse, String>>;

    /// Perform a PUT request with a body
    fn put(&self, url: &str, body: String) -> BoxFuture<'static, Result<HttpResponse, String>>;

    /// Perform a DELETE request
    fn delete(&self, url: &str) -> BoxFuture<'static, Result<HttpResponse, String>>;
}

/// Null HTTP provider that returns errors
#[derive(Clone, Default)]
pub struct NullHttp;

impl HttpProvider for NullHttp {
    fn get(&self, _url: &str) -> BoxFuture<'static, Result<HttpResponse, String>> {
        Box::pin(async { Err("HTTP not available".into()) })
    }

    fn post(&self, _url: &str, _body: String) -> BoxFuture<'static, Result<HttpResponse, String>> {
        Box::pin(async { Err("HTTP not available".into()) })
    }

    fn put(&self, _url: &str, _body: String) -> BoxFuture<'static, Result<HttpResponse, String>> {
        Box::pin(async { Err("HTTP not available".into()) })
    }

    fn delete(&self, _url: &str) -> BoxFuture<'static, Result<HttpResponse, String>> {
        Box::pin(async { Err("HTTP not available".into()) })
    }
}

/// Standard HTTP provider using reqwest (when the http feature is enabled)
#[cfg(feature = "http")]
#[derive(Clone, Default)]
pub struct StdHttp;

#[cfg(feature = "http")]
impl HttpProvider for StdHttp {
    fn get(&self, url: &str) -> BoxFuture<'static, Result<HttpResponse, String>> {
        let url = url.to_string();
        Box::pin(async move {
            let response = reqwest::blocking::get(&url).map_err(|e| e.to_string())?;
            let status = response.status().as_u16();
            let body = response.text().map_err(|e| e.to_string())?;
            Ok(HttpResponse::new(status, body))
        })
    }

    fn post(&self, url: &str, body: String) -> BoxFuture<'static, Result<HttpResponse, String>> {
        let url = url.to_string();
        Box::pin(async move {
            let client = reqwest::blocking::Client::new();
            let response = client
                .post(&url)
                .header("Content-Type", "application/json")
                .body(body)
                .send()
                .map_err(|e| e.to_string())?;
            let status = response.status().as_u16();
            let body = response.text().map_err(|e| e.to_string())?;
            Ok(HttpResponse::new(status, body))
        })
    }

    fn put(&self, url: &str, body: String) -> BoxFuture<'static, Result<HttpResponse, String>> {
        let url = url.to_string();
        Box::pin(async move {
            let client = reqwest::blocking::Client::new();
            let response = client
                .put(&url)
                .header("Content-Type", "application/json")
                .body(body)
                .send()
                .map_err(|e| e.to_string())?;
            let status = response.status().as_u16();
            let body = response.text().map_err(|e| e.to_string())?;
            Ok(HttpResponse::new(status, body))
        })
    }

    fn delete(&self, url: &str) -> BoxFuture<'static, Result<HttpResponse, String>> {
        let url = url.to_string();
        Box::pin(async move {
            let client = reqwest::blocking::Client::new();
            let response = client.delete(&url).send().map_err(|e| e.to_string())?;
            let status = response.status().as_u16();
            let body = response.text().map_err(|e| e.to_string())?;
            Ok(HttpResponse::new(status, body))
        })
    }
}

pub fn default_http() -> Arc<dyn HttpProvider> {
    #[cfg(feature = "http")]
    {
        Arc::new(StdHttp)
    }
    #[cfg(not(feature = "http"))]
    {
        Arc::new(NullHttp)
    }
}

