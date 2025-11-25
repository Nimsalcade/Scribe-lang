use std::fmt::Write as FmtWrite;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{self, Command};

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand, ValueEnum};
use scribe_codegen::{llvm::LlvmBackend, Backend};
use scribe_compiler::{
    ast::Module,
    diagnostics::{Diagnostic, Diagnostics},
    discover_project_files,
    ir::{self, IrModule},
    lower, typeck, Compiler, ProjectCompileResult,
};
use scribe_runtime::Runtime;
use serde::Deserialize;

// Ensure scribe_std is compiled (staticlib) by referencing it
use scribe_std;

#[derive(Debug, Clone, Copy, Default, ValueEnum)]
pub enum EmitKind {
    /// Emit only the object file (.o)
    Obj,
    /// Emit a linked executable (default)
    #[default]
    Exe,
}

#[derive(Parser)]
#[command(name = "scribe", about = "Scribe language CLI", version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Type-check a Scribe source file
    Check {
        #[arg(long, default_value = "examples/hello-world/main.scribe")]
        file: PathBuf,
    },
    /// Build a Scribe project
    Build {
        #[arg(long, default_value = ".")]
        project: PathBuf,
        #[arg(long)]
        release: bool,
        /// What to emit: obj (object file only) or exe (linked executable)
        #[arg(long, value_enum, default_value = "exe")]
        emit: EmitKind,
    },
    /// Build then run a Scribe project (placeholder)
    Run {
        #[arg(long, default_value = ".")]
        project: PathBuf,
        #[arg(long)]
        release: bool,
    },
    /// Format Scribe sources (placeholder)
    Fmt {
        #[arg(long)]
        check: bool,
    },
}

fn main() {
    // Ensure scribe_std static library is linked
    scribe_std::build_marker();
    
    let cli = Cli::parse();
    if let Err(err) = dispatch(cli.command) {
        eprintln!("error: {err}");
        for cause in err.chain().skip(1) {
            eprintln!("  caused by: {cause}");
        }
        process::exit(1);
    }
}

fn dispatch(command: Commands) -> Result<()> {
    match command {
        Commands::Check { file } => handle_check(&file),
        Commands::Build {
            project,
            release,
            emit,
        } => handle_build(&project, release, emit),
        Commands::Run { project, release } => handle_run(&project, release),
        Commands::Fmt { check } => handle_fmt(check),
    }
}

fn handle_check(file: &Path) -> Result<()> {
    compile_and_check(file)?;
    println!("{}: syntax + types OK", file.display());
    Ok(())
}

fn handle_build(project: &Path, release: bool, emit: EmitKind) -> Result<()> {
    let root = find_project_root(project).ok_or_else(|| {
        anyhow!(
            "unable to locate scribe.toml starting from {}",
            project.display()
        )
    })?;

    // Discover all .scribe files in the project
    let project_files = discover_project_files(&root);
    if !project_files.is_empty() {
        println!("discovered {} source file(s)", project_files.len());
    }

    // Compile the entire project with module resolution
    let entry = resolve_entry(&root)?;
    let compile_result = compile_project(&root, &entry)?;

    println!(
        "compiled {} module(s) (entry: {})",
        compile_result.modules.len(),
        compile_result.entry_path.display()
    );

    // Type-check all modules and collect diagnostics
    let mut has_errors = false;
    let mut module_sources = std::collections::HashMap::new();

    for (module_key, compiled_module) in &compile_result.modules {
        // Try to read source file for diagnostics
        let source = std::fs::read_to_string(&compiled_module.file_path).ok();
        if let Some(ref src) = source {
            module_sources.insert(compiled_module.file_path.to_string_lossy().to_string(), src.clone());
        }

        // Type check the module
        if let Err(errors) = typeck::check_module(&compiled_module.ast) {
            has_errors = true;
            let mut diagnostics = Diagnostics::new();
            for error in errors {
                diagnostics.push(Diagnostic::from(error));
            }

            // Print diagnostics with correct file path
            if let Some(source) = module_sources.get(&compiled_module.file_path.to_string_lossy().to_string()) {
                print_diagnostics(
                    &compiled_module.file_path,
                    source,
                    &diagnostics,
                );
            } else {
                eprintln!("{}: type checking failed", module_key);
            }
        }
    }

    if has_errors {
        return Err(anyhow!("type checking failed"));
    }

    // Lower the entry module to IR
    let ir = lower::lower_module(&compile_result.entry_module);

    // Write human-readable IR for debugging
    let ir_path = write_ir_stub(&root, &compile_result.entry_path, &ir)?;
    println!("wrote IR -> {}", ir_path.display());

    // Emit object file
    let obj_path = emit_object(&root, &ir)?;
    println!("emitted object -> {}", obj_path.display());

    // Link to executable if requested
    if matches!(emit, EmitKind::Exe) {
        let exe_path = link_executable(&root, &obj_path, release)?;
        println!("linked executable -> {}", exe_path.display());
    }

    Ok(())
}

fn handle_run(project: &Path, release: bool) -> Result<()> {
    handle_build(project, release, EmitKind::Exe)?;

    let root = find_project_root(project).ok_or_else(|| {
        anyhow!(
            "unable to locate scribe.toml starting from {}",
            project.display()
        )
    })?;
    let exe_path = root.join("target/scribe/module");

    if exe_path.exists() {
        println!("running {}", exe_path.display());
        let status = Command::new(&exe_path)
            .status()
            .with_context(|| format!("failed to run {}", exe_path.display()))?;
        if !status.success() {
            return Err(anyhow!("program exited with {}", status));
        }
    } else {
        // Fallback to runtime stub if executable wasn't produced
        let rt = Runtime::cooperative();
        let task = rt.spawn(async { println!("[scribectl] runtime stub executing main") });
        let _ = task.into_result();
        println!("run command using runtime stub (no native executable)");
    }
    Ok(())
}

fn handle_fmt(check: bool) -> Result<()> {
    if check {
        println!("fmt --check has nothing to validate yet");
    } else {
        println!("fmt command will reformat .scribe files once formatter lands");
    }
    Ok(())
}

fn resolve_entry(project_root: &Path) -> Result<PathBuf> {
    let manifest_path = project_root.join("scribe.toml");
    let data = fs::read_to_string(&manifest_path)
        .with_context(|| format!("reading {}", manifest_path.display()))?;
    let manifest: Manifest =
        toml::from_str(&data).with_context(|| format!("parsing {}", manifest_path.display()))?;
    let mut relative = PathBuf::from(manifest.build.main.trim());
    if relative.as_os_str().is_empty() {
        return Err(anyhow!("manifest build.main entry cannot be empty"));
    }
    if relative.extension().is_none() {
        relative.set_extension("scribe");
    }
    let candidate = project_root.join(&relative);
    if candidate.exists() {
        Ok(candidate)
    } else {
        Err(anyhow!("entry source {} not found", candidate.display()))
    }
}

fn find_project_root(start: &Path) -> Option<PathBuf> {
    let mut current = match start.canonicalize() {
        Ok(path) => path,
        Err(_) => start.to_path_buf(),
    };
    if current.is_file() {
        current = current.parent()?.to_path_buf();
    }

    loop {
        if current.join("scribe.toml").exists() {
            return Some(current.clone());
        }
        if !current.pop() {
            break;
        }
    }
    None
}

#[derive(Debug, Deserialize, Default)]
struct Manifest {
    #[serde(default)]
    build: BuildSection,
}

#[derive(Debug, Deserialize)]
struct BuildSection {
    #[serde(default = "default_main_entry")]
    main: String,
}

impl Default for BuildSection {
    fn default() -> Self {
        Self {
            main: default_main_entry(),
        }
    }
}

fn default_main_entry() -> String {
    "main".to_string()
}

fn compile_project(project_root: &Path, entry_file: &Path) -> Result<ProjectCompileResult> {
    let mut compiler = Compiler::new();
    compiler
        .compile_project(project_root, entry_file)
        .map_err(|err| anyhow!("compilation failed: {}", err))
}

fn compile_and_check(file: &Path) -> Result<Module> {
    let source =
        std::fs::read_to_string(file).with_context(|| format!("reading {}", file.display()))?;
    let compiler = Compiler::new();
    let mut diagnostics = Diagnostics::new();
    let module = match compiler.compile_source(&source) {
        Ok(module) => module,
        Err(err) => {
            diagnostics.push(Diagnostic::from(err));
            print_diagnostics(file, &source, &diagnostics);
            return Err(anyhow!("parsing failed"));
        }
    };
    if let Err(errors) = typeck::check_module(&module) {
        diagnostics.extend(errors.into_iter().map(Diagnostic::from));
        print_diagnostics(file, &source, &diagnostics);
        return Err(anyhow!("type checking failed"));
    }
    Ok(module)
}

fn print_diagnostics(file: &Path, source: &str, diagnostics: &Diagnostics) {
    for diagnostic in diagnostics.iter() {
        if let Some(span) = diagnostic.span {
            let line_idx = span.line.saturating_sub(1) as usize;
            let line_text = source.lines().nth(line_idx).unwrap_or("");
            eprintln!(
                "{}:{}:{}: {}",
                file.display(),
                span.line,
                span.column,
                diagnostic.message
            );
            eprintln!("  {}", line_text);
            let mut indicator = String::from("  ");
            let padding = span.column.saturating_sub(1) as usize;
            indicator.push_str(&" ".repeat(padding));
            indicator.push('^');
            eprintln!("{indicator}");
        } else {
            eprintln!("{}: {}", file.display(), diagnostic.message);
        }
    }
}

fn write_ir_stub(project: &Path, entry: &Path, ir_module: &IrModule) -> Result<PathBuf> {
    let target_dir = project.join("target/scribe");
    fs::create_dir_all(&target_dir)
        .with_context(|| format!("creating {}", target_dir.display()))?;
    let ir_name = entry
        .file_stem()
        .map(|stem| stem.to_string_lossy().into_owned())
        .unwrap_or_else(|| "module".to_string());
    let output_path = target_dir.join(format!("{ir_name}.ir"));
    let contents = format_ir(ir_module);
    fs::write(&output_path, contents)
        .with_context(|| format!("writing {}", output_path.display()))?;
    Ok(output_path)
}

fn emit_object(project: &Path, ir_module: &IrModule) -> Result<PathBuf> {
    let mut backend =
        LlvmBackend::new(None).map_err(|err| anyhow!("failed to initialize LLVM: {err}"))?;
    let artifact = backend
        .emit("scribe", ir_module)
        .map_err(|err| anyhow!("codegen error: {err}"))?;
    let target_dir = project.join("target/scribe");
    fs::create_dir_all(&target_dir)
        .with_context(|| format!("creating {}", target_dir.display()))?;
    let obj_path = target_dir.join("module.o");
    fs::write(&obj_path, &artifact.object)
        .with_context(|| format!("writing {}", obj_path.display()))?;
    Ok(obj_path)
}

fn link_executable(project: &Path, obj_path: &Path, _release: bool) -> Result<PathBuf> {
    let target_dir = project.join("target/scribe");
    let exe_path = target_dir.join("module");

    // Find the Scribe standard library
    let std_lib_path = find_scribe_std_lib();

    // Try clang first (available on macOS), then cc
    let linkers = ["clang", "cc", "gcc"];
    let mut last_err = None;

    for linker in &linkers {
        let mut cmd = Command::new(linker);
        cmd.arg(obj_path)
            .arg("-o")
            .arg(&exe_path)
            .arg("-lc"); // Link against libc

        // Link against Scribe standard library if found
        if let Some(ref lib_path) = std_lib_path {
            cmd.arg(lib_path);
        }

        let result = cmd.output();

        match result {
            Ok(output) if output.status.success() => {
                return Ok(exe_path);
            }
            Ok(output) => {
                last_err = Some(anyhow!(
                    "{} failed: {}",
                    linker,
                    String::from_utf8_lossy(&output.stderr)
                ));
            }
            Err(e) => {
                last_err = Some(anyhow!("{} not found: {}", linker, e));
            }
        }
    }

    Err(last_err.unwrap_or_else(|| anyhow!("no suitable linker found")))
}

/// Find the Scribe standard library static archive
/// Returns the full path to the libscribe_std.a file
fn find_scribe_std_lib() -> Option<PathBuf> {
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .map(|p| p.to_path_buf());

    // Try to find the library in deps directory (where Cargo puts versioned artifacts)
    if let Some(root) = workspace_root {
        // Check debug deps first
        let debug_deps = root.join("target/debug/deps");
        if debug_deps.exists() {
            // Look for any libscribe_std-*.a file
            if let Ok(entries) = std::fs::read_dir(&debug_deps) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if let Some(name) = path.file_name() {
                        let name_str = name.to_string_lossy();
                        if name_str.starts_with("libscribe_std-") && name_str.ends_with(".a") {
                            return Some(path);
                        }
                    }
                }
            }
        }

        // Check release deps
        let release_deps = root.join("target/release/deps");
        if release_deps.exists() {
            if let Ok(entries) = std::fs::read_dir(&release_deps) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if let Some(name) = path.file_name() {
                        let name_str = name.to_string_lossy();
                        if name_str.starts_with("libscribe_std-") && name_str.ends_with(".a") {
                            return Some(path);
                        }
                    }
                }
            }
        }
    }

    // Fallback to installed location
    dirs::data_local_dir().map(|p| p.join("scribe/lib/libscribe_std.a"))

}

fn format_ir(ir_module: &IrModule) -> String {
    let mut out = String::new();
    for function in &ir_module.functions {
        let _ = writeln!(out, "fn {} {{", function.name);
        for block in &function.blocks {
            let _ = writeln!(out, "  block {}:", block.id.0);
            for inst in &block.instructions {
                match inst {
                    ir::Instruction::Const { dest, value } => {
                        let _ = writeln!(out, "    v{} = const {:?}", dest.0, value);
                    }
                    ir::Instruction::Binary { dest, op, lhs, rhs } => {
                        let op_str = match op {
                            ir::IrBinaryOp::Add => "add",
                            ir::IrBinaryOp::Sub => "sub",
                            ir::IrBinaryOp::Mul => "mul",
                            ir::IrBinaryOp::Div => "div",
                            ir::IrBinaryOp::And => "and",
                            ir::IrBinaryOp::Or => "or",
                        };
                        let _ =
                            writeln!(out, "    v{} = {} v{}, v{}", dest.0, op_str, lhs.0, rhs.0);
                    }
                    ir::Instruction::Call { dest, callee, args } => {
                        let dest_str = dest.map(|id| format!("v{} = ", id.0)).unwrap_or_default();
                        let args_str = args
                            .iter()
                            .map(|arg| format!("v{}", arg.0))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let _ = writeln!(out, "    {}call {}({})", dest_str, callee, args_str);
                    }
                    ir::Instruction::Compare { dest, op, lhs, rhs } => {
                        let op_str = match op {
                            ir::IrCompareOp::Equal => "eq",
                            ir::IrCompareOp::NotEqual => "ne",
                            ir::IrCompareOp::Less => "lt",
                            ir::IrCompareOp::LessEqual => "le",
                            ir::IrCompareOp::Greater => "gt",
                            ir::IrCompareOp::GreaterEqual => "ge",
                        };
                        let _ = writeln!(
                            out,
                            "    v{} = cmp_{} v{}, v{}",
                            dest.0, op_str, lhs.0, rhs.0
                        );
                    }
                    ir::Instruction::Intrinsic { dest, name, args } => {
                        let dest_str = dest.map(|id| format!("v{} = ", id.0)).unwrap_or_default();
                        let args_str = args
                            .iter()
                            .map(|arg| format!("v{}", arg.0))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let _ = writeln!(out, "    {}intrinsic {}({})", dest_str, name, args_str);
                    }
                }
            }
            if let Some(term) = &block.terminator {
                match term {
                    ir::Terminator::Return(value) => {
                        if let Some(id) = value {
                            let _ = writeln!(out, "    return v{}", id.0);
                        } else {
                            let _ = writeln!(out, "    return");
                        }
                    }
                    ir::Terminator::Jump(target) => {
                        let _ = writeln!(out, "    jump block {}", target.0);
                    }
                    ir::Terminator::Branch {
                        cond,
                        then_bb,
                        else_bb,
                    } => {
                        let _ = writeln!(
                            out,
                            "    branch v{}, block {}, block {}",
                            cond.0, then_bb.0, else_bb.0
                        );
                    }
                }
            }
        }
        let _ = writeln!(out, "}}\n");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    #[test]
    fn resolves_example_project_entry() {
        let project = Path::new("../examples/hello-world");
        let entry = resolve_entry(project).expect("entry path");
        assert!(
            entry.ends_with("examples/hello-world/main.scribe"),
            "entry was {}",
            entry.display()
        );
    }

    #[test]
    fn finds_project_root_from_nested_dir() {
        let project = Path::new("../examples/hello-world/src");
        let root = find_project_root(project).expect("project root");
        assert!(
            root.ends_with("examples/hello-world"),
            "root was {}",
            root.display()
        );
    }
}
