Here’s a full PRD you can hand to an AI software engineer agent to build the **Scribe** language.

---

# Product Requirements Document (PRD)

**Product:** Scribe Programming Language – v0.1 Core Implementation
**Owner:** <you>
**Target Users:** Experienced developers, system programmers, backend engineers, AI/ML infra engineers
**Platforms:** Linux, macOS, Windows (x86_64; ARM later)

---

## 1. Product Overview

**Scribe** is a statically typed, native-compiled language that:

* Reads close to clear English.
* Compiles to fast machine code (comparable to C/C++).
* Enforces memory safety and security at compile time.
* Has async and parallel features built in from day one.
* Interops with C, Python, and JavaScript.

This PRD defines **v0.1**: a usable core language, a compiler, a minimal runtime, and basic tooling.

---

## 2. Goals and Non-Goals

### 2.1 Goals

1. **Natural but strict syntax**

   * Allow both verbose and concise forms:

     * `if count is greater than 10 then ...`
     * `if count > 10: ...`
   * One unambiguous grammar. Natural forms are syntax sugar, not a separate language.

2. **High performance**

   * Native binaries via LLVM (or similar).
   * Performance in the range of C/Rust for typical workloads.
   * Zero-cost abstractions for functions, records, and generics.

3. **Safety**

   * No null pointer dereference.
   * No use-after-free, double free.
   * No data races in safe code.
   * Taint system for untrusted input.
   * Checked error handling (`Result`-style).

4. **Modern concurrency**

   * `async` / `await`.
   * Lightweight `task`s.
   * Simple `in parallel` loops.
   * Actor-style components.

5. **Interop & tooling**

   * C FFI.
   * Python and JS bindings (minimal for v0.1).
   * CLI compiler.
   * Simple package manager.
   * Basic language server support.

### 2.2 Non-Goals (for v0.1)

* No full AI/ML standard library yet (only hooks for pipelines).
* No full-blown optimizer pipeline competing with top-tier compilers.
* No GUI toolkit.
* No Windows ARM support in v0.1 (nice-to-have later).
* No self-hosting in v0.1 (compiler can be written in Rust/Go/C++ etc.).

---

## 3. Target Users and Use Cases

### 3.1 Users

1. **System programmer**
   Wants C-like control and speed, but with safety and better syntax.

2. **Backend / API engineer**
   Wants fast, safe web services with strong typing and built-in async.

3. **AI infra / data engineer**
   Wants to write efficient data processing and pipeline orchestration with better concurrency.

### 3.2 Primary Use Cases (v0.1)

1. CLI tools and services.
2. HTTP JSON APIs.
3. Data processing workers (file / network IO, CPU-bound loops).
4. Native modules called from Python/JS for performance-critical paths.

---

## 4. Functional Requirements

### 4.1 Language Core

#### 4.1.1 Basic syntax

**Blocks**

* Indentation-based blocks (similar to Python).
* No `{}` for blocks.
* End markers (`end`) allowed in long form, optional in concise form.

Examples:

```scribe
# Long form
if count is greater than 10 then
    print("too many")
end

# Short form
if count > 10:
    print("too many")
```

**Comments**

* Line comments start with `#`.
* No block comments for v0.1.

```scribe
# This is a comment
```

#### 4.1.2 Types and variables

Supported built-in types for v0.1:

* `bool`
* `int32`, `int64`
* `float64` (alias `number` in surface syntax)
* `text`
* `bytes`
* `time` (opaque, from std library)
* `duration` (opaque, from std library)

Variables:

```scribe
# Long form
let count be number = 0

# Short form
let count: int64 = 0
let name = "Scribe"      # inferred as text
```

Rules:

* `let` defines an immutable binding by default.
* `let mutable` for mutable binding:

```scribe
let mutable counter = 0
counter = counter + 1
```

Type inference:

* Required for simple expressions.
* Type annotation required on function parameters and return types in v0.1.

#### 4.1.3 Records (structs)

Syntax:

```scribe
record User:
    id: int64
    name: text
    email: text
end
```

Short form:

```scribe
record User(id: int64, name: text, email: text)
```

Construction:

```scribe
let u = User(
    id = 1,
    name = "Alice",
    email = "alice@example.com"
)
```

Requirement:

* Records compile to plain structs with a known memory layout.
* No inheritance for v0.1. Only composition.

#### 4.1.4 Collections

For v0.1:

* `list[T]` or `list of T`
* `map[K, V]` or `map from K to V`

Surface syntax:

```scribe
let nums: list of int64 = [1, 2, 3]
let ages: map from text to int64 =
    {
        "alice": 30,
        "bob": 28
    }
```

Compiler maps these to standard library types:

* `std::List[T]`
* `std::Map[K, V]`

#### 4.1.5 Control flow

**Conditionals**

Support both forms:

```scribe
# Verbose
if count is greater than 10 then
    print("too many")
else if count is 0 then
    print("none")
otherwise
    print("some")
end

# Concise
if count > 10:
    print("too many")
elif count == 0:
    print("none")
else:
    print("some")
```

Mapping of natural operators:

* `is greater than` → `>`
* `is less than` → `<`
* `is at least` → `>=`
* `is at most` → `<=`
* `is equal to` / `is` → `==`
* `is not` → `!=`

**Loops**

```scribe
for i in 0 to 10:
    print(i)
end

for each name in names:
    print(name)
end
```

Spec:

* `for i in 0 to N:` iterates from start (inclusive) to end (exclusive).
* `for each x in collection:` iterates over values.
* Break/continue with `break` / `continue`.

#### 4.1.6 Functions and modules

**Functions**

```scribe
# Long form
function add(a: number, b: number) gives number:
    return a + b
end

# Short form
fn add(a: number, b: number) -> number:
    a + b    # last expression returns
```

Rules:

* Must declare parameter and return types.
* Implicit return: last expression if no `return` keyword.
* `main` function:

```scribe
fn main() -> int32:
    print("Hello")
    0
```

**Modules**

* Each file is a module.
* Optional header:

```scribe
module example.math
```

Exports:

```scribe
export fn mean(values: list of number) -> number:
    ...
```

Imports:

```scribe
use example.math
use http.server as http
```

Requirement:

* Simple module system based on file path and module name mapping.
* No circular module support in v0.1 (can be prevented or partially supported).

---

### 4.2 Type System and Safety

#### 4.2.1 Static typing and inference

* All variables and expressions have static types.
* Local inference allowed:

```scribe
let x = 1          # int64
let y = 1.0        # float64 / number
let msg = "hi"     # text
```

* Function parameters and return types must be annotated in v0.1.

#### 4.2.2 Ownership and borrowing (simplified)

Goal: memory safety and data race freedom in safe code.

v0.1 minimal model:

* Values are owned by one variable at a time.
* Move semantics by default, copy only for simple types (`int`, `bool`, etc.).
* References:

  * Immutable borrow: `&T`
  * Mutable borrow: `&mut T`

User-facing syntax can stay implicit:

* For v0.1 the API design should be ownership-safe but we can keep reference syntax mostly behind type checker rules to keep surface simple.
* Compiler enforces:

  * Only one mutable reference at a time.
  * Either many shared immutable references or one mutable reference, not both.

This can be stricter and simplified for v0.1 (even if it means some extra copies).

#### 4.2.3 Errors and `Result` type

Introduce:

* `Result[T, E]` standard type.

Surface syntax:

```scribe
fn read_config(path: text) -> Result[Config, IoError]:
    let data = file.read_text(path)
    if data is Err:
        return data
    end
    ...
```

Sugar with `check` (v0.1 feature):

```scribe
fn read_config(path: text) -> Result[Config, IoError]:
    let data = check file.read_text(path)
    let cfg = check json.parse[Config](data)
    Ok(cfg)
```

`check` behavior:

* If expression is `Ok(value)` → use `value`.
* If expression is `Err(e)` → return `Err(e)` from current function.

#### 4.2.4 Taint tracking (minimal v0.1)

Goal: basic scaffolding in v0.1.

* Type wrapper: `Tainted[T]`.
* Functions that read from external sources (HTTP body, stdin, raw socket) return `Tainted[text]` or `Tainted[bytes]`.

Example:

```scribe
fn handle(req: Request) -> Response:
    let body: Tainted[text] = req.body
    let clean = security.expect_valid_utf8(body)
    # clean: text (un-tainted) after validation
```

Requirements for v0.1:

* Implement `Tainted[T]` as a generic wrapper and tag a few standard inputs as `Tainted`.
* A small set of “sink” functions (like `db.exec`, `process.exec`) should reject `Tainted` types at compile time without validation.

---

### 4.3 Concurrency and Async

#### 4.3.1 Async functions

Syntax:

```scribe
async fn fetch_user(id: int64) -> Result[User, NetError]:
    let url = "https://api/users/" + to_text(id)
    let resp = await http.get(url)
    check resp.ensure_status(200)
    json.parse[User](resp.body)
```

Requirements:

* `async` functions are lowered to state machines / futures internally.
* `await` requires an `async` context.

#### 4.3.2 Tasks

Syntax:

```scribe
async fn fetch_both(a_id: int64, b_id: int64) -> Result[(User, User), NetError]:
    let t1 = start fetch_user(a_id)
    let t2 = start fetch_user(b_id)
    let u1 = await t1
    let u2 = await t2
    Ok((u1, u2))
```

Requirements:

* `start f(args)` returns a `Task[T]` where `T` is the async function’s return type.
* Runtime uses a worker thread pool for tasks.

#### 4.3.3 Parallel loops (v0.1 limited)

Syntax:

```scribe
for each url in urls in parallel:
    let resp = await http.get(url)
    print(length of resp.body)
end
```

v0.1:

* Implement only for `async` blocks with independent iterations.
* Disallow mutation of captured outer variables unless they are thread-safe wrappers (not fully required in v0.1; may be partially enforced).

---

### 4.4 Security Features

For v0.1, focus on:

1. **Memory safety**

   * No raw pointer arithmetic in safe code.
   * All allocations managed by runtime / ownership model.

2. **Taint wrappers**

   * Basic `Tainted[T]` type and a couple of validation functions.

3. **Secure defaults**

   * File IO functions default to safe variants.
   * Network APIs default to TLS where reasonable (API placeholders).

Unsafe operations exist but need explicit syntax:

```scribe
unsafe fn do_raw_stuff(...):
    ...
```

And must be marked in type system (`raises unsafe` is optional for later).

---

### 4.5 Standard Library (v0.1 Scope)

Key modules to ship:

1. `core`

   * Option, Result, basic traits.
2. `text`

   * String manipulation, split, join, trim.
3. `file`

   * `read_text(path) -> Result[text, IoError>`
   * `write_text(path, text) -> Result[Unit, IoError>`
4. `time`

   * `now() -> time`
   * `sleep(duration) -> async`
5. `http`

   * Minimal client:

     * `async fn get(url: text) -> Result[Response, NetError]`
   * Minimal server:

     * `fn new_server() -> Server`
     * `server.get(path, handler)`
     * `server.listen(port: int32)`
6. `json`

   * `fn parse[T](text: text) -> Result[T, JsonError>`
   * `fn stringify[T](value: T) -> text`
7. `security`

   * `fn expect_valid_utf8(Tainted[bytes]) -> Result[text, ValidationError]`
   * `fn validate_email(Tainted[text]) -> Result[text, ValidationError]` (placeholder)

---

### 4.6 Interoperability

#### 4.6.1 C FFI (v0.1)

Syntax:

```scribe
extern c fn c_sin(x: float64) -> float64

fn demo() -> float64:
    c_sin(1.0)
```

Requirements:

* Support `extern c` with basic types (`int32`, `int64`, `float64`, `bool`, `*void` equivalent).
* No varargs or complex structs in v0.1.

#### 4.6.2 Python / JS (minimal hooks)

v0.1:

* CLI option to compile a shared library.
* Basic C ABI so Python/JS can call Scribe functions if the user writes glue code.
* Full automatic binding generation can be later.

---

### 4.7 Tooling

#### 4.7.1 Compiler CLI

Binary: `scribe`

Commands:

* `scribe build`

  * Compiles project (from `scribe.toml` or similar).
* `scribe run`

  * Build + run main.
* `scribe fmt`

  * Format all `.scribe` files.
* `scribe check`

  * Typecheck without full build.

Flags:

* `--release` for optimized build.
* `--target` for platform triple (basic support).

#### 4.7.2 Package manager

File: `scribe.toml` example:

```toml
[package]
name = "example-api"
version = "0.1.0"

[dependencies]
http = "0.1"
json = "0.1"
```

v0.1:

* Local dependencies (workspace).
* Simple version pinning.
* No remote index needed initially (can be mocked or left as local path deps).

#### 4.7.3 Language server (MVP)

Features:

* Syntax highlighting (via tokenization).
* Go-to-definition.
* Basic completion for local identifiers.
* Diagnostics from compiler.

It can call the compiler front-end as a library.

---

## 5. Non-Functional Requirements

### 5.1 Performance

* “Hello world” binary size comparable to a simple C program with minimal runtime.
* Simple numeric loop performance within 2x of equivalent C/Rust for v0.1.
* Async HTTP server can handle thousands of idle connections without large overhead.

### 5.2 Portability

* Must compile and run on:

  * Linux x86_64
  * macOS x86_64
  * Windows x86_64
* Build system should be testable in CI on all three.

### 5.3 Reliability and Testing

* Unit tests for:

  * Parser.
  * Type checker.
  * Codegen for basic constructs.
* Integration tests:

  * Compile and run small programs (CLI, HTTP server, async tasks).
* Fuzzing hooks for parser.

### 5.4 Developer Experience

* Error messages must be clear and human-friendly.
  Example:

  ```text
  error[E1001]: type mismatch in addition
    --> main.scribe:5:12
     |
   5 |     let x = "hi" + 1
     |              ^^^ cannot add text and int64
  ```

* Suggest possible fixes when easy:

  * “Did you mean to convert 1 to text with to_text(1)?”

---

## 6. Milestones / Phases

The AI agent can implement this in phases.

### Phase 1: Core Front-End

* Lexer and parser for:

  * Variables, types, records, lists, maps, functions, modules, imports.
  * Control flow (if/elif/else, loops).
* AST design.
* Basic type checker.

**Acceptance:**

* Can compile a simple pure program:

  ```scribe
  fn main() -> int32:
      let xs: list of int64 = [1, 2, 3]
      let mutable sum = 0
      for each x in xs:
          sum = sum + x
      print(sum)
      0
  ```

### Phase 2: Codegen & Runtime (AOT only)

* Lower typed AST to IR.
* Use LLVM or other backend to emit native code.
* Implement runtime:

  * Startup.
  * Basic allocation.
  * Panic handler.
* Implement core std modules (print, file IO, simple list/map types).

**Acceptance:**

* `scribe run` works on small programs with IO and basic collections.

### Phase 3: Async, Tasks, HTTP Client/Server

* Implement `async`, `await`, and `start`.
* Implement minimal async runtime with event loop and worker pool.
* Implement `http.get` and a basic HTTP server.

**Acceptance:**

* Sample HTTP API from earlier can compile and run:

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
          version = "1.0.0"
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

### Phase 4: Taint System (basic) and C FFI

* Implement `Tainted[T]` type.
* Mark certain APIs as returning tainted values.
* Add compile-time checks for sinks.
* Add `extern c` support.

**Acceptance:**

* Program that passes `Tainted[text]` directly into `db.exec` fails to compile.
* Program that validates then passes succeeds.
* Simple C math function can be called from Scribe.

### Phase 5: Tooling and Language Server MVP

* `scribe fmt`
* `scribe check`
* Minimal language server with:

  * Diagnostics.
  * Go-to-definition (within module).
* Start of test framework.

**Acceptance:**

* Works in a simple editor integration (VS Code or similar) with basic feedback.

---

## 7. Risks and Open Items

* Ownership model:
  For v0.1, implementation may be simpler than full Rust-level ownership.
  It’s acceptable to start with:

  * Move semantics for non-copy types.
  * Borrowing rules enforced only for obvious cases.
  * Gradually tighten rules.

* GC vs no-GC:
  For v0.1, it’s fine to:

  * Use reference counting + cycle breaker or a simple tracing collector.
  * Later optimize to more advanced schemes or escape analysis.

* Taint and effects system:
  v0.1 can start with a minimal, library-based system plus a few checks, then deepen integration.

---

## 8. Example Programs (Acceptance Tests)

The following should compile and run correctly in v0.1.

### 8.1 Simple numeric program

```scribe
fn sum_list(values: list of int64) -> int64:
    let mutable s = 0
    for each v in values:
        s = s + v
    s

fn main() -> int32:
    let xs = [1, 2, 3, 4, 5]
    let result = sum_list(xs)
    print("sum = " + to_text(result))
    0
```

### 8.2 Async HTTP fetch

```scribe
use http
use json

record Post(id: int64, title: text)

async fn fetch_post(id: int64) -> Result[Post, NetError]:
    let url = "https://example.com/posts/" + to_text(id)
    let resp = await http.get(url)
    check resp.ensure_status(200)
    json.parse[Post](resp.body)

async fn main_async() -> Result[Unit, NetError]:
    let p = check fetch_post(1)
    print("title: " + p.title)
    Ok(())

fn main() -> int32:
    match run_async(main_async()):
        case Ok(_): 0
        case Err(e):
            print("error: " + e.message)
            1
```

---

If you want, I can next:

* Turn this into a `README` + `scribe.toml` skeleton for the repo.
* Or break down Phase 1 into concrete tasks/issues your agent can pick up one by one (lexer tasks, parser rules, AST structs, etc.).
