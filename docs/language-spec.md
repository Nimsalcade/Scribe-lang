# Scribe Language Specification (Working Draft)

This document tracks the v0.1 surface area defined in `prd.md`.

## 1. Lexical Structure
- Python-style significant indentation with colons (`:`) to start blocks
- Line comments start with `#`
- UTF-8 source encoding
- No explicit `end` keywords; indentation controls block scope

## 2. Types
- Primitive numeric, text, bytes, time, duration
- Records (simple form: `record Name(field: Type, ...)`)
- Lists and maps
- Ownership model (move by default, borrow syntax TBD)

## 3. Functions and Modules
- `fn` keyword for function declarations
- Mandatory type annotations on params/returns in v0.1
- Modules map to file paths, `use` statements follow dotted names

## 4. Control Flow
- Natural-language comparisons: `is greater than`, `is less than`, `is at least`, `is at most`, `is equal to`, `is not`
- Concise operators: `>`, `<`, `>=`, `<=`, `==`, `!=`
- `if` / `elif` / `else` or `otherwise`
- `for i in a to b:` and `for each item in items:`
- `break` / `continue`

## 5. Async and Parallelism
- `async fn`, `await`, `start`
- Parallel loops with `in parallel`

## 6. Safety Model
- Ownership and borrowing rules (simplified Rust semantics)
- `Result[T, E]` with `check` sugar
- `Tainted[T]` propagation rules (planned)

## 7. Interop
- `extern c` declarations for C ABI bridging (planned)
- Shared library output for Python/JS integration hooks (planned)

## 8. Tooling
- `scribe` CLI supports `check`, `build`, `run`, and `fmt` scaffolds
- Project manifests (`scribe.toml`) provide `[build] main = "<entry>"` to pick the entry `.scribe` file

This draft will evolve alongside the reference implementation.
