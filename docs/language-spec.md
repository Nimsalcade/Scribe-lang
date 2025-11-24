# Scribe Language Specification (Working Draft)

This document tracks the v0.1 surface area defined in `prd.md`.

## 1. Lexical Structure
- Significant indentation
- Line comments start with `#`
- UTF-8 source encoding

## 2. Types
- Primitive numeric, text, bytes, time, duration
- Records, lists, maps
- Ownership model (move by default, borrow syntax TBD)

## 3. Functions and Modules
- `fn` and `function` spellings
- Mandatory type annotations on params/returns in v0.1
- Modules map to file paths, `use` statements follow dotted names

## 4. Control Flow
- Verbose and concise `if` / `elif` / `else`
- `for i in a to b:` and `for each item in items:`
- `break` / `continue`

## 5. Async and Parallelism
- `async fn`, `await`, `start`
- Parallel loops with `in parallel`

## 6. Safety Model
- Ownership and borrowing rules (simplified Rust semantics)
- `Result[T, E]` with `check` sugar
- `Tainted[T]` propagation rules

## 7. Interop
- `extern c` declarations for C ABI bridging
- Shared library output for Python/JS integration hooks

## 8. Tooling
- `scribe` CLI currently supports `check`, `build`, `run`, and `fmt` scaffolds
- Project manifests (`scribe.toml`) provide `[build] main = "<entry>"` to pick the entry `.scribe` file

This draft will evolve alongside the reference implementation.
