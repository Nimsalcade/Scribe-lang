//! Core entry points for the Scribe compiler.

pub mod ast;
pub mod diagnostics;
mod error;
pub mod ir;
pub mod lexer;
pub mod lower;
pub mod parser;
pub mod resolver;
pub mod span;
pub mod typeck;

use std::collections::HashMap;
use std::path::Path;

pub use error::CompilerError;
pub use resolver::{discover_project_files, ModuleResolver, ResolveError, ResolvedModule};

/// Compiled module with its AST and metadata
#[derive(Debug)]
pub struct CompiledModule {
    pub path: String,
    pub ast: ast::Module,
}

/// High-level compiler driver that orchestrates lexing and parsing passes.
pub struct Compiler {
    /// Compiled modules cache
    modules: HashMap<String, CompiledModule>,
}

impl Compiler {
    /// Create a new compiler instance with default configuration.
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Compile a source string into an AST module.
    pub fn compile_source(&self, source: &str) -> Result<ast::Module, CompilerError> {
        let mut parser = parser::Parser::new(source);
        Ok(parser.parse_module()?)
    }

    /// Compile a file into an AST module
    pub fn compile_file(&mut self, path: &Path) -> Result<ast::Module, CompilerError> {
        let source = std::fs::read_to_string(path).map_err(|e| {
            CompilerError::Io(format!("failed to read {}: {}", path.display(), e))
        })?;
        self.compile_source(&source)
    }

    /// Compile a project with all its modules
    pub fn compile_project(
        &mut self,
        project_root: &Path,
        entry_file: &Path,
    ) -> Result<Vec<CompiledModule>, CompilerError> {
        let mut resolver = ModuleResolver::new(project_root);
        let mut compiled = Vec::new();

        // Compile entry file
        let entry_ast = self.compile_file(entry_file)?;

        // Resolve and compile imported modules
        for use_stmt in &entry_ast.uses {
            let path_str = use_stmt.path.to_string();
            if !self.modules.contains_key(&path_str) {
                if let Ok(resolved) = resolver.resolve(&use_stmt.path) {
                    if let Ok(module_ast) = self.compile_file(&resolved.file_path) {
                        let compiled_module = CompiledModule {
                            path: path_str.clone(),
                            ast: module_ast,
                        };
                        self.modules.insert(path_str, compiled_module);
                    }
                }
            }
        }

        // Add entry module
        let entry_path = entry_file.to_string_lossy().to_string();
        compiled.push(CompiledModule {
            path: entry_path,
            ast: entry_ast,
        });

        // Add all resolved modules
        for (_, module) in self.modules.drain() {
            compiled.push(module);
        }

        Ok(compiled)
    }

    /// Get a reference to compiled modules
    pub fn get_module(&self, path: &str) -> Option<&CompiledModule> {
        self.modules.get(path)
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
