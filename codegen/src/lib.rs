pub mod llvm;

use thiserror::Error;

use scribe_compiler::ir::IrModule;

#[derive(Debug, Error)]
pub enum CodegenError {
    #[error("LLVM backend error: {0}")]
    Llvm(String),
    #[error("unsupported IR feature: {0}")]
    Unsupported(String),
    #[error("missing value for id {0}")]
    MissingValue(u32),
}

#[derive(Debug, Default)]
pub struct CodegenArtifact {
    pub object: Vec<u8>,
}

pub trait Backend {
    fn emit(&mut self, module_name: &str, ir: &IrModule) -> Result<CodegenArtifact, CodegenError>;
}
