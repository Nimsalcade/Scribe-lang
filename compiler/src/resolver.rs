//! Module resolver for Scribe
//!
//! Resolves module paths to file system paths, handling:
//! - Project-local modules (src/)
//! - Standard library modules (std/)
//! - Dependencies from scribe.toml

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::ast::ModulePath;

/// Errors that can occur during module resolution
#[derive(Debug, Clone)]
pub enum ResolveError {
    ModuleNotFound { path: String },
    CyclicDependency { path: String },
    IoError { path: String, message: String },
}

impl std::fmt::Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::ModuleNotFound { path } => {
                write!(f, "module not found: {}", path)
            }
            ResolveError::CyclicDependency { path } => {
                write!(f, "cyclic dependency detected: {}", path)
            }
            ResolveError::IoError { path, message } => {
                write!(f, "IO error reading {}: {}", path, message)
            }
        }
    }
}

impl std::error::Error for ResolveError {}

/// Resolved module information
#[derive(Debug, Clone)]
pub struct ResolvedModule {
    /// The original module path
    pub module_path: String,
    /// The file system path to the module source
    pub file_path: PathBuf,
    /// Dependencies of this module (other module paths)
    pub dependencies: Vec<String>,
}

/// Module resolver that maps module paths to file system paths
pub struct ModuleResolver {
    /// Project root directory (where scribe.toml lives)
    project_root: PathBuf,
    /// Standard library root directory
    std_root: Option<PathBuf>,
    /// Cache of resolved modules
    resolved: HashMap<String, ResolvedModule>,
    /// Currently resolving modules (for cycle detection)
    resolving: HashSet<String>,
}

impl ModuleResolver {
    /// Create a new resolver for a project
    pub fn new(project_root: impl AsRef<Path>) -> Self {
        let project_root = project_root.as_ref().to_path_buf();

        // Try to find std library relative to project or in known locations
        let std_root = Self::find_std_root(&project_root);

        Self {
            project_root,
            std_root,
            resolved: HashMap::new(),
            resolving: HashSet::new(),
        }
    }

    /// Set the standard library root explicitly
    pub fn with_std_root(mut self, std_root: impl AsRef<Path>) -> Self {
        self.std_root = Some(std_root.as_ref().to_path_buf());
        self
    }

    /// Resolve a module path to a file system path
    pub fn resolve(&mut self, module_path: &ModulePath) -> Result<ResolvedModule, ResolveError> {
        let path_str = module_path.to_string();
        self.resolve_path(&path_str)
    }

    /// Resolve a module path string to a file system path
    pub fn resolve_path(&mut self, module_path: &str) -> Result<ResolvedModule, ResolveError> {
        // Check cache first
        if let Some(resolved) = self.resolved.get(module_path) {
            return Ok(resolved.clone());
        }

        // Check for cycles
        if self.resolving.contains(module_path) {
            return Err(ResolveError::CyclicDependency {
                path: module_path.to_string(),
            });
        }

        self.resolving.insert(module_path.to_string());

        // Try to find the module
        let file_path = self.find_module_file(module_path)?;

        // For now, we don't parse dependencies - that would require parsing the file
        let resolved = ResolvedModule {
            module_path: module_path.to_string(),
            file_path,
            dependencies: Vec::new(),
        };

        self.resolving.remove(module_path);
        self.resolved.insert(module_path.to_string(), resolved.clone());

        Ok(resolved)
    }

    /// Get all resolved modules
    pub fn all_resolved(&self) -> Vec<&ResolvedModule> {
        self.resolved.values().collect()
    }

    /// Find the file path for a module
    fn find_module_file(&self, module_path: &str) -> Result<PathBuf, ResolveError> {
        let segments: Vec<&str> = module_path.split('.').collect();

        // Try project-local modules first (src/)
        if let Some(path) = self.try_project_module(&segments) {
            return Ok(path);
        }

        // Try standard library modules
        if let Some(path) = self.try_std_module(&segments) {
            return Ok(path);
        }

        // Module not found
        Err(ResolveError::ModuleNotFound {
            path: module_path.to_string(),
        })
    }

    /// Try to find module in project src/ directory
    fn try_project_module(&self, segments: &[&str]) -> Option<PathBuf> {
        let mut path = self.project_root.join("src");

        // Navigate through segments as directories
        for (i, segment) in segments.iter().enumerate() {
            if i == segments.len() - 1 {
                // Last segment - try as file
                let file_path = path.join(format!("{}.scribe", segment));
                if file_path.exists() {
                    return Some(file_path);
                }
                // Also try as directory with mod.scribe
                let mod_path = path.join(segment).join("mod.scribe");
                if mod_path.exists() {
                    return Some(mod_path);
                }
            } else {
                path = path.join(segment);
            }
        }

        // Also check root of project
        let root_file = self.project_root.join(format!("{}.scribe", segments.join("/")));
        if root_file.exists() {
            return Some(root_file);
        }

        None
    }

    /// Try to find module in standard library
    fn try_std_module(&self, segments: &[&str]) -> Option<PathBuf> {
        let std_root = self.std_root.as_ref()?;

        // Check if first segment is 'std' and skip it
        let segments = if segments.first() == Some(&"std") {
            &segments[1..]
        } else {
            segments
        };

        if segments.is_empty() {
            return None;
        }

        let mut path = std_root.clone();

        for (i, segment) in segments.iter().enumerate() {
            if i == segments.len() - 1 {
                // Last segment - try as file
                let file_path = path.join(format!("{}.scribe", segment));
                if file_path.exists() {
                    return Some(file_path);
                }
                // Also try as directory with mod.scribe
                let mod_path = path.join(segment).join("mod.scribe");
                if mod_path.exists() {
                    return Some(mod_path);
                }
            } else {
                path = path.join(segment);
            }
        }

        None
    }

    /// Find the standard library root
    fn find_std_root(project_root: &Path) -> Option<PathBuf> {
        // Check if std/ exists in project root (for development)
        let local_std = project_root.join("std");
        if local_std.exists() {
            return Some(local_std);
        }

        // Check parent directories (for workspace setup)
        let mut current = project_root.to_path_buf();
        while current.pop() {
            let std_path = current.join("std");
            if std_path.exists() {
                return Some(std_path);
            }
        }

        None
    }
}

/// Discover all .scribe files in a project
pub fn discover_project_files(project_root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();

    // Check src/ directory
    let src_dir = project_root.join("src");
    if src_dir.exists() {
        discover_files_recursive(&src_dir, &mut files);
    }

    // Check for main.scribe in project root
    let main_file = project_root.join("main.scribe");
    if main_file.exists() {
        files.push(main_file);
    }

    files
}

fn discover_files_recursive(dir: &Path, files: &mut Vec<PathBuf>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                discover_files_recursive(&path, files);
            } else if path.extension().map_or(false, |ext| ext == "scribe") {
                files.push(path);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_path_to_string() {
        let path = ModulePath::new(vec![
            crate::ast::Identifier::new("std"),
            crate::ast::Identifier::new("io"),
            crate::ast::Identifier::new("file"),
        ]);
        assert_eq!(path.to_string(), "std.io.file");
    }
}

