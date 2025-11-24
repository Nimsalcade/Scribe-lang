# Scribe Compiler

This directory will host the lexer, parser, type checker, IR, and code generation logic for the Scribe language.

## Planned crates/modules
- `lexer`: tokenization for both concise and natural syntax
- `parser`: produce AST nodes shared with tooling
- `typer`: symbol tables, type inference, ownership checks
- `ir`: SSA-style intermediate representation
- `codegen`: LLVM (or alternative backend) integration

## Tests
Place parser/type checker regression tests under `tests/`.
