# Scribe Runtime

Hosts the async executor, memory management hooks, and platform services required by generated binaries.

## Components

- `InlineExecutor` – executes each future immediately on the caller thread (useful for tests and bootstrap tooling).
- `CooperativeExecutor` – prototype executor backed by a FIFO scheduler that can be evolved into the real async runtime.
- `task` module – defines `TaskId`, `TaskState`, and the `Task<T>` handle returned by the runtime API.
- `scheduler` module – initial cooperative scheduler that polls queued futures in order and requeues pending work.

## Roadmap

- Replace the `noop_waker` placeholder with a proper waker that reschedules tasks when ready.
- Integrate IO reactors and timers behind the `Executor` trait.
- Extend `TaskState` to track cancellation and panic information.

The current design deliberately keeps the executor APIs small so the Scribe compiler can link against them before the full async runtime exists.
