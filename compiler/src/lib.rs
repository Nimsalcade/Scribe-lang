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
use std::path::{Path, PathBuf};

pub use error::CompilerError;
pub use resolver::{discover_project_files, ModuleResolver, ResolveError, ResolvedModule};

/// Compiled module with its AST and metadata
#[derive(Debug, Clone)]
pub struct CompiledModule {
    pub path: String,
    pub file_path: PathBuf,
    pub ast: ast::Module,
}

/// Result of compiling a project, containing entry module info and all compiled modules
#[derive(Debug)]
pub struct ProjectCompileResult {
    /// The entry module (main file) AST
    pub entry_module: ast::Module,
    /// Path to the entry module source file
    pub entry_path: PathBuf,
    /// All compiled modules including the entry module, keyed by module path
    pub modules: HashMap<String, CompiledModule>,
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
    /// Returns a result containing the entry module and a map of all compiled modules
    pub fn compile_project(
        &mut self,
        project_root: &Path,
        entry_file: &Path,
    ) -> Result<ProjectCompileResult, CompilerError> {
        let mut resolver = ModuleResolver::new(project_root);

        // Compile entry file
        let entry_ast = self.compile_file(entry_file)?;
        let mut modules = HashMap::new();

        // Add entry module to map
        let entry_path_str = entry_file.to_string_lossy().to_string();
        modules.insert(
            entry_path_str.clone(),
            CompiledModule {
                path: entry_path_str,
                file_path: entry_file.to_path_buf(),
                ast: entry_ast.clone(),
            },
        );

        // Resolve and compile imported modules
        self.compile_dependencies(
            project_root,
            &entry_ast,
            &mut resolver,
            &mut modules,
        )?;

        Ok(ProjectCompileResult {
            entry_module: entry_ast,
            entry_path: entry_file.to_path_buf(),
            modules,
        })
    }

    /// Recursively compile all dependencies of a module
    fn compile_dependencies(
        &mut self,
        project_root: &Path,
        module: &ast::Module,
        resolver: &mut ModuleResolver,
        compiled: &mut HashMap<String, CompiledModule>,
    ) -> Result<(), CompilerError> {
        for use_stmt in &module.uses {
            let path_str = use_stmt.path.to_string();

            // Skip if already compiled
            if compiled.contains_key(&path_str) {
                continue;
            }

            // Resolve the module
            if let Ok(resolved) = resolver.resolve(&use_stmt.path) {
                // Compile the module file
                if let Ok(module_ast) = self.compile_file(&resolved.file_path) {
                    let resolved_path = resolved.file_path.to_string_lossy().to_string();

                    // Store by both module path and file path for lookup flexibility
                    let compiled_module = CompiledModule {
                        path: path_str.clone(),
                        file_path: resolved.file_path.clone(),
                        ast: module_ast.clone(),
                    };
                    compiled.insert(path_str, compiled_module.clone());

                    // Also store by file path
                    compiled.insert(resolved_path, compiled_module);

                    // Recursively compile dependencies of this module
                    self.compile_dependencies(project_root, &module_ast, resolver, compiled)?;
                }
            }
        }
        Ok(())
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
