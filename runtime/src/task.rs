//! Task metadata structures shared between the runtime and executors.

/// Opaque identifier assigned to each spawned task.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct TaskId(pub(crate) u64);

impl TaskId {
    /// Return the numeric representation of the identifier.
    pub fn as_u64(&self) -> u64 {
        self.0
    }
}

/// Represents the current lifecycle state of a task.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TaskState<T> {
    Pending,
    Completed(T),
}

/// Handle returned to callers after spawning async work.
#[derive(Debug)]
pub struct Task<T> {
    id: TaskId,
    state: TaskState<T>,
}

impl<T> Task<T> {
    pub(crate) fn completed(id: TaskId, value: T) -> Self {
        Self {
            id,
            state: TaskState::Completed(value),
        }
    }

    /// Return the identifier of the task.
    pub fn id(&self) -> TaskId {
        self.id
    }

    /// Whether the task has finished executing.
    pub fn is_completed(&self) -> bool {
        matches!(self.state, TaskState::Completed(_))
    }

    /// Consume the task handle and take the completed value if ready.
    pub fn into_result(self) -> Option<T> {
        match self.state {
            TaskState::Completed(value) => Some(value),
            TaskState::Pending => None,
        }
    }
}
