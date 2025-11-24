# Scribe v0.1 Roadmap

## Phase 1 – Core Front-End
- Implement lexer covering natural + concise syntax spellings
- Build parser producing an AST shared with tooling
- Prototype type checker with symbol tables and basic inference
- ✅ Lexer and parser scaffolding live in `compiler/` with smoke tests

## Phase 2 – Codegen & Runtime
- Lower typed AST into SSA IR
- Integrate LLVM backend for native binaries
- Build minimal runtime for allocation, tasks, and panic handling
- ✅ Runtime crate created with inline + cooperative executors and task handles

## Phase 3 – Async + HTTP
- Lower `async` functions to state machines
- Implement `start`, task scheduler, and reactor
- Deliver HTTP client/server API used by examples

## Phase 4 – Safety & Interop
- Add `Tainted[T]` analysis and sink validation
- Provide `extern c` and shared library targets

## Phase 5 – Tooling
- `scribe` CLI commands: build, run, check, fmt
- Minimal language server (diagnostics, go-to-definition)
- CI pipeline across Linux/macOS/Windows
- ✅ `scribe` CLI skeleton available with `check`, `build`, `run`, and `fmt` stubs

Each phase should land with automated tests and sample programs in `examples/` demonstrating the new capability.
