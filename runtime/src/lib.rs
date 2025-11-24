//! Runtime scaffolding for Scribe async/task execution.

use std::future::Future;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::task::Poll;
use std::time::Duration;

use futures::executor;
use futures::future::BoxFuture;

pub mod http;
pub mod io;
pub mod scheduler;
pub mod task;

pub use http::{HttpProvider, HttpResponse, NullHttp};
pub use io::{IoProvider, StdIo, TimerProvider};
pub use scheduler::CooperativeScheduler;
use scheduler::ScheduledTask;
pub use task::{Task, TaskId, TaskState};

/// Trait implemented by task executors capable of running Scribe async work.
pub trait Executor: Send + Sync + 'static {
    /// Spawn a future onto the executor.
    fn spawn<F>(&self, future: F) -> Task<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static;

    /// Block the current thread until the future completes.
    fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static;
}

/// Inline executor that runs every task immediately on the calling thread.
#[derive(Clone)]
pub struct InlineExecutor {
    next_id: Arc<AtomicU64>,
}

impl InlineExecutor {
    pub fn new() -> Self {
        Self {
            next_id: Arc::new(AtomicU64::new(1)),
        }
    }

    fn next_task_id(&self) -> TaskId {
        TaskId(self.next_id.fetch_add(1, Ordering::SeqCst))
    }
}

impl Default for InlineExecutor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor for InlineExecutor {
    fn spawn<F>(&self, future: F) -> Task<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        let id = self.next_task_id();
        let output = executor::block_on(future);
        Task::completed(id, output)
    }

    fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        executor::block_on(future)
    }
}

/// Cooperative executor backed by the FIFO scheduler.
#[derive(Clone)]
pub struct CooperativeExecutor {
    scheduler: Arc<CooperativeScheduler>,
    next_id: Arc<AtomicU64>,
}

impl CooperativeExecutor {
    pub fn new() -> Self {
        Self {
            scheduler: Arc::new(CooperativeScheduler::new()),
            next_id: Arc::new(AtomicU64::new(1)),
        }
    }

    fn next_task_id(&self) -> TaskId {
        TaskId(self.next_id.fetch_add(1, Ordering::SeqCst))
    }

    fn run_until_idle(&self) {
        while let Some(task) = self.scheduler.next_task() {
            if matches!(task.poll(), Poll::Pending) {
                // task will requeue itself when its waker fires
            }
        }
    }
}

impl Default for CooperativeExecutor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor for CooperativeExecutor {
    fn spawn<F>(&self, future: F) -> Task<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        let id = self.next_task_id();
        let result_cell = Arc::new(Mutex::new(None));
        let completion = Arc::clone(&result_cell);
        let queue = self.scheduler.task_queue();
        let available = self.scheduler.available();
        let scheduled = ScheduledTask::new(
            id,
            Box::pin(async move {
                let output = future.await;
                if let Ok(mut slot) = completion.lock() {
                    *slot = Some(output);
                }
            }),
            queue,
            available,
        );
        self.scheduler.enqueue(scheduled);
        loop {
            self.run_until_idle();
            if let Some(value) = result_cell.lock().expect("result lock poisoned").take() {
                return Task::completed(id, value);
            }
            self.scheduler.wait_for_task(Duration::from_millis(1));
        }
    }

    fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        executor::block_on(future)
    }
}

/// Public runtime handle that owns an executor implementation.
pub struct Runtime<E: Executor = InlineExecutor> {
    executor: E,
    timer: Arc<dyn TimerProvider>,
    io: Arc<dyn IoProvider>,
}

impl Runtime<InlineExecutor> {
    /// Create a runtime with the default inline executor.
    pub fn new() -> Self {
        Runtime {
            executor: InlineExecutor::new(),
            timer: io::default_timer(),
            io: io::default_io(),
        }
    }
}

impl Runtime<CooperativeExecutor> {
    /// Create a runtime backed by the cooperative scheduler executor.
    pub fn cooperative() -> Self {
        Runtime {
            executor: CooperativeExecutor::new(),
            timer: io::default_timer(),
            io: io::default_io(),
        }
    }
}

impl<E: Executor> Runtime<E> {
    /// Create a runtime from a custom executor implementation.
    pub fn with_executor(executor: E) -> Self {
        Runtime {
            executor,
            timer: io::default_timer(),
            io: io::default_io(),
        }
    }

    /// Spawn a future using the underlying executor.
    pub fn spawn<F>(&self, future: F) -> Task<F::Output>
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        self.executor.spawn(future)
    }

    /// Block on a future using the executor's `block_on` implementation.
    pub fn block_on<F>(&self, future: F) -> F::Output
    where
        F: Future + Send + 'static,
        F::Output: Send + 'static,
    {
        self.executor.block_on(future)
    }

    /// Sleep for the specified duration using the runtime's timer provider.
    pub fn sleep(&self, duration: Duration) -> BoxFuture<'static, ()> {
        self.timer.sleep(duration)
    }

    /// Access the timer provider.
    pub fn timer(&self) -> Arc<dyn TimerProvider> {
        Arc::clone(&self.timer)
    }

    /// Access the IO provider.
    pub fn io(&self) -> Arc<dyn IoProvider> {
        Arc::clone(&self.io)
    }

    /// Replace the timer provider.
    pub fn with_timer_provider(mut self, timer: Arc<dyn TimerProvider>) -> Self {
        self.timer = timer;
        self
    }

    /// Replace the IO provider.
    pub fn with_io_provider(mut self, io: Arc<dyn IoProvider>) -> Self {
        self.io = io;
        self
    }
}

impl<E: Executor> Clone for Runtime<E>
where
    E: Clone,
{
    fn clone(&self) -> Self {
        Runtime {
            executor: self.executor.clone(),
            timer: Arc::clone(&self.timer),
            io: Arc::clone(&self.io),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Runtime;
    use futures::executor;
    use std::time::Duration;

    #[test]
    fn inline_executor_runs_future() {
        let rt = Runtime::new();
        let task = rt.spawn(async { 42 });
        assert!(task.is_completed());
        assert_eq!(task.into_result(), Some(42));
    }

    #[test]
    fn cooperative_executor_runs_future() {
        let rt = Runtime::cooperative();
        let task = rt.spawn(async { 7 });
        assert!(task.is_completed());
        assert_eq!(task.into_result(), Some(7));
    }

    #[test]
    fn default_services_are_available() {
        let rt = Runtime::new();
        let timer = rt.timer();
        executor::block_on(timer.sleep(Duration::from_millis(0)));
        let io = rt.io();
        executor::block_on(io.write("stdout", Vec::new()));
    }

    #[test]
    fn sleep_future_completes() {
        let rt = Runtime::cooperative();
        let timer = rt.timer();
        let task = rt.spawn(async move {
            timer.sleep(Duration::from_millis(1)).await;
            99
        });
        assert_eq!(task.into_result(), Some(99));
    }

    #[test]
    fn runs_multiple_sleeping_tasks() {
        let rt = Runtime::cooperative();
        let timer_a = rt.timer();
        let timer_b = rt.timer();
        let task_a = rt.spawn(async move {
            timer_a.sleep(Duration::from_millis(1)).await;
            1
        });
        let task_b = rt.spawn(async move {
            timer_b.sleep(Duration::from_millis(2)).await;
            2
        });
        assert_eq!(task_a.into_result(), Some(1));
        assert_eq!(task_b.into_result(), Some(2));
    }
}
