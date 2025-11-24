
````markdown
# Scribe Programming Language

Scribe is a new programming language that aims to:

- Read close to clear English for everyday code.
- Compile to fast native code (C/Rust class performance).
- Enforce memory safety and secure defaults at compile time.
- Make async and parallel work a normal part of the language.
- Interact with C, Python, and JavaScript.

This repo contains the Scribe compiler, runtime, standard library, and examples for the **v0.1** implementation.

---

## Vision

Scribe is built around a few ideas:

1. **Plain-language syntax**  
   You can write either:

   ```scribe
   if count is greater than 10 then
       print("too many")
   end
````

or the shorter form:

```scribe
if count > 10:
    print("too many")
```

2. **Performance without surprises**
   Scribe compiles to native code through an IR and a backend like LLVM.
   High-level constructs (records, generics, async, actors, parallel loops) compile down to code that is close to hand-written C/Rust.

3. **Safety built in**

   * Static typing with inference.
   * Ownership and borrowing to prevent use-after-free and data races in safe code.
   * Tainted types for untrusted input, with compile-time checks on dangerous sinks.
   * Checked errors (`Result[T, E]` and the `check` keyword).

4. **Modern concurrency**

   * `async` / `await` as first-class.
   * Lightweight `task`s and `start` for parallel work.
   * Parallel loops: `for each item in items in parallel: ...`.
   * Actors for isolated, message-based state.

5. **Interoperability**

   * `extern c` for C FFI.
   * Shared library output for Python/JS integration.
   * WebAssembly target later.

---

## Status

Scribe v0.1 is **in development**.
The main short-term goal is a usable core language:

* Basic syntax (functions, records, conditionals, loops).
* Static type checker.
* Native code generation.
* Minimal stdlib (text, list, map, file I/O, time).
* Async/await and a small async runtime.
* Simple HTTP client/server and JSON.
* C FFI.

---

## Core Language Concepts

### Types and variables

```scribe
let count be number = 0
let name = "Scribe"
let mutable total: int64 = 0
```

Records:

```scribe
record User(id: int64, name: text, email: text)

let u = User(
    id = 1,
    name = "Alice",
    email = "alice@example.com"
)
```

Lists and maps:

```scribe
let nums: list of int64 = [1, 2, 3]
let ages: map from text to int64 =
    {
        "alice": 30,
        "bob": 28
    }
```

### Functions and modules

```scribe
fn add(a: number, b: number) -> number:
    a + b

module example.math

export fn mean(values: list of number) -> number:
    if length of values is 0:
        error ValueError("empty list")
    end
    let s = sum(values)
    s / length of values
```

Imports:

```scribe
use example.math
use http.server as http
```

### Errors

```scribe
fn read_config(path: text) -> Result[Config, IoError]:
    let data = check file.read_text(path)
    let cfg = check json.parse[Config](data)
    Ok(cfg)
```

### Async and parallel

```scribe
async fn fetch_user(id: int64) -> Result[User, NetError]:
    let url = "https://api/users/" + to_text(id)
    let resp = await http.get(url)
    check resp.ensure_status(200)
    json.parse[User](resp.body)

async fn fetch_two(a_id: int64, b_id: int64) -> Result[(User, User), NetError]:
    let t1 = start fetch_user(a_id)
    let t2 = start fetch_user(b_id)
    let u1 = await t1
    let u2 = await t2
    Ok((u1, u2))
```

Simple server:

```scribe
module example.api

use http.server
use json
use time

record HealthStatus(ok: bool, now: time, version: text)

async fn handle_health(req: Request) -> Response:
    let status = HealthStatus(
        ok = true,
        now = now(),
        version = "0.1.0-dev"
    )
    let body = json.stringify(status)
    Response.ok_json(body)

fn main() -> int32:
    let app = server.new()
    app.get("/health", handle_health)
    print("listening on http://localhost:8080")
    app.listen(port = 8080)
    0
```

---

## Repository Layout (planned)

Suggested layout for the AI engineer to follow:

```text
.
├── compiler/            # Scribe compiler front-end, type checker, IR, codegen
│   ├── src/
│   └── tests/
├── runtime/             # Runtime support: allocator hooks, async runtime, task scheduler
├── std/                 # Standard library written in Scribe
│   ├── core/            # Option, Result, basic traits
│   ├── text/
│   ├── file/
│   ├── time/
│   ├── http/
│   └── json/
├── examples/
│   ├── hello-world/
│   │   ├── scribe.toml
│   │   └── main.scribe
│   └── web-api/
│       ├── scribe.toml
│       └── main.scribe
├── docs/
│   ├── language-spec.md
│   ├── syntax.md
│   └── examples.md
├── scribe.toml          # Skeleton project manifest (for tooling, std, examples)
└── README.md
```

Implementation language for the compiler and runtime can be Rust (suggested), but that part is up to the engineering setup.

---

## Building (target plan)

These steps describe the intended UX once the compiler exists.

```bash
# Build the compiler
cargo build -p scribe-compiler --release

# Check a Scribe project
scribe check

# Build a Scribe project
scribe build

# Run a Scribe project (build + run main)
scribe run
```

For v0.1, the AI engineer should:

* Provide a `scribe` CLI that supports at least: `check`, `build`, `run`, and `fmt`.
* Let these commands work from the project root that has `scribe.toml`.

---

## `scribe.toml` (skeleton)

This is a template manifest for a Scribe project.
You can use it for:

* `examples/hello-world`
* `examples/web-api`
* or any new Scribe app.

```toml
[package]
name = "example-api"
version = "0.1.0"
description = "Example HTTP JSON API written in Scribe"
authors = ["Your Name <you@example.com>"]
license = "MIT"

[build]
# The entry point module (without .scribe)
main = "main"

[toolchain]
# Minimal Scribe compiler version this project expects
min_version = "0.1.0"

[dependencies]
# Core standard library is always available, but can be versioned explicitly later.
core = "0.1"
text = "0.1"
file = "0.1"
time = "0.1"
http = "0.1"
json = "0.1"
```

You can adjust `name`, `description`, and dependencies per project.

If you want a manifest for the **language repo** itself, you can keep it as a meta-package:

```toml
[package]
name = "scribe"
version = "0.1.0"
description = "Scribe programming language: compiler, runtime, and standard library"
authors = ["Your Name <you@example.com>"]
license = "MIT"

[toolchain]
min_version = "0.1.0"

[dependencies]
# Internal Scribe modules once they are bootstrapped
std = "0.1"
```

---

## Suggested task breakdown for the AI engineer

Short, concrete tasks the agent can turn into issues:

1. **Set up repo and scaffolding**

   * Create folder layout from the tree above.
   * Add this `README.md`.
   * Add `scribe.toml` to `examples/hello-world` and `examples/web-api`.

2. **Compiler front-end**

   * Implement lexer and parser for:

     * literals, identifiers, `let`, `fn`, `record`, `if`, `for`, `module`, `use`.
   * Build AST structures.
   * Add basic error reporting with file/line/column.

3. **Type checker**

   * Implement symbol tables and type resolution.
   * Support built-in types, records, lists, and maps.
   * Enforce function signatures and basic type rules.

4. **IR and codegen**

   * Design a simple SSA-style IR.
   * Lower typed AST to IR.
   * Hook into LLVM or another backend to emit native code.
   * Implement runtime entry and `main`.

5. **Stdlib core**

   * Implement `core`, `text`, `file`, and `time` modules.
   * Provide `print`, `read_text`, `write_text`, `now`.

6. **Async and HTTP**

   * Implement `async` / `await` lowering to futures/state machines.
   * Build a simple async runtime with an event loop.
   * Provide `http.get` and an HTTP server with routing.

7. **Basic tests and examples**

   * Make `examples/hello-world` compile and run.
   * Make `examples/web-api` compile and serve `/health` as in the README.

