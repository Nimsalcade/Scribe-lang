use std::fs;
use std::io::{self, Read, Write};
use std::path::Path;
use std::sync::Arc;
use std::time::Duration;

use futures::future::{self, BoxFuture};
use futures::FutureExt;
use futures_timer::Delay;

pub trait TimerProvider: Send + Sync + 'static {
    fn sleep(&self, duration: Duration) -> BoxFuture<'static, ()>;
}

pub trait IoProvider: Send + Sync + 'static {
    /// Read bytes from a resource (file path, URL, etc.)
    fn read(&self, resource: &str) -> BoxFuture<'static, Vec<u8>>;
    /// Write bytes to a resource
    fn write(&self, resource: &str, data: Vec<u8>) -> BoxFuture<'static, ()>;
    /// Read a file as text
    fn read_file(&self, path: &str) -> BoxFuture<'static, Result<String, io::Error>>;
    /// Write text to a file
    fn write_file(&self, path: &str, data: String) -> BoxFuture<'static, Result<(), io::Error>>;
    /// Append text to a file
    fn append_file(&self, path: &str, data: String) -> BoxFuture<'static, Result<(), io::Error>>;
    /// Check if a file exists
    fn file_exists(&self, path: &str) -> BoxFuture<'static, bool>;
}

#[derive(Clone, Default)]
pub struct StdTimer;

impl TimerProvider for StdTimer {
    fn sleep(&self, duration: Duration) -> BoxFuture<'static, ()> {
        Delay::new(duration).boxed()
    }
}

#[derive(Clone, Default)]
pub struct NullIo;

impl IoProvider for NullIo {
    fn read(&self, _resource: &str) -> BoxFuture<'static, Vec<u8>> {
        Box::pin(future::ready(Vec::new()))
    }

    fn write(&self, _resource: &str, _data: Vec<u8>) -> BoxFuture<'static, ()> {
        Box::pin(future::ready(()))
    }

    fn read_file(&self, _path: &str) -> BoxFuture<'static, Result<String, io::Error>> {
        Box::pin(future::ready(Ok(String::new())))
    }

    fn write_file(&self, _path: &str, _data: String) -> BoxFuture<'static, Result<(), io::Error>> {
        Box::pin(future::ready(Ok(())))
    }

    fn append_file(&self, _path: &str, _data: String) -> BoxFuture<'static, Result<(), io::Error>> {
        Box::pin(future::ready(Ok(())))
    }

    fn file_exists(&self, _path: &str) -> BoxFuture<'static, bool> {
        Box::pin(future::ready(false))
    }
}

/// Standard file system I/O provider
#[derive(Clone, Default)]
pub struct StdIo;

impl IoProvider for StdIo {
    fn read(&self, resource: &str) -> BoxFuture<'static, Vec<u8>> {
        let path = resource.to_string();
        Box::pin(async move { fs::read(&path).unwrap_or_default() })
    }

    fn write(&self, resource: &str, data: Vec<u8>) -> BoxFuture<'static, ()> {
        let path = resource.to_string();
        Box::pin(async move {
            let _ = fs::write(&path, data);
        })
    }

    fn read_file(&self, path: &str) -> BoxFuture<'static, Result<String, io::Error>> {
        let path = path.to_string();
        Box::pin(async move { fs::read_to_string(&path) })
    }

    fn write_file(&self, path: &str, data: String) -> BoxFuture<'static, Result<(), io::Error>> {
        let path = path.to_string();
        Box::pin(async move { fs::write(&path, data) })
    }

    fn append_file(&self, path: &str, data: String) -> BoxFuture<'static, Result<(), io::Error>> {
        let path = path.to_string();
        Box::pin(async move {
            let mut file = fs::OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)?;
            file.write_all(data.as_bytes())
        })
    }

    fn file_exists(&self, path: &str) -> BoxFuture<'static, bool> {
        let exists = Path::new(path).exists();
        Box::pin(future::ready(exists))
    }
}

pub fn default_timer() -> Arc<dyn TimerProvider> {
    Arc::new(StdTimer)
}

pub fn default_io() -> Arc<dyn IoProvider> {
    Arc::new(StdIo)
}
