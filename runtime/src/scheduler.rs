use std::collections::VecDeque;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, Condvar, Mutex};
use std::task::{Context, Poll};
use std::time::Duration;

use futures::task::{waker_ref, ArcWake};

use crate::task::TaskId;

type TaskQueue = Arc<Mutex<VecDeque<Arc<ScheduledTask>>>>;

pub struct CooperativeScheduler {
    queue: TaskQueue,
    available: Arc<Condvar>,
}

impl CooperativeScheduler {
    pub fn new() -> Self {
        Self {
            queue: Arc::new(Mutex::new(VecDeque::new())),
            available: Arc::new(Condvar::new()),
        }
    }

    pub fn enqueue(&self, task: Arc<ScheduledTask>) {
        let mut queue = self.queue.lock().expect("scheduler queue poisoned");
        queue.push_back(task);
        self.available.notify_one();
    }

    pub fn next_task(&self) -> Option<Arc<ScheduledTask>> {
        let mut queue = self.queue.lock().expect("scheduler queue poisoned");
        queue.pop_front()
    }

    pub fn is_idle(&self) -> bool {
        let queue = self.queue.lock().expect("scheduler queue poisoned");
        queue.is_empty()
    }

    pub fn task_queue(&self) -> TaskQueue {
        self.queue.clone()
    }

    pub fn available(&self) -> Arc<Condvar> {
        self.available.clone()
    }

    pub fn wait_for_task(&self, timeout: Duration) {
        let guard = self.queue.lock().expect("scheduler queue poisoned");
        let _ = self
            .available
            .wait_timeout(guard, timeout)
            .expect("scheduler condvar poisoned");
    }
}

pub struct ScheduledTask {
    id: TaskId,
    future: Mutex<Option<Pin<Box<dyn Future<Output = ()> + Send>>>>,
    queue: TaskQueue,
    available: Arc<Condvar>,
}

impl ScheduledTask {
    pub fn new(
        id: TaskId,
        future: Pin<Box<dyn Future<Output = ()> + Send>>,
        queue: TaskQueue,
        available: Arc<Condvar>,
    ) -> Arc<Self> {
        Arc::new(Self {
            id,
            future: Mutex::new(Some(future)),
            queue,
            available,
        })
    }

    pub fn poll(self: &Arc<Self>) -> Poll<()> {
        let mut future_slot = self.future.lock().expect("task future poisoned");
        if let Some(mut future) = future_slot.take() {
            let waker = waker_ref(self);
            let mut ctx = Context::from_waker(&waker);
            match future.as_mut().poll(&mut ctx) {
                Poll::Pending => {
                    *future_slot = Some(future);
                    Poll::Pending
                }
                Poll::Ready(()) => Poll::Ready(()),
            }
        } else {
            Poll::Ready(())
        }
    }

    pub fn id(&self) -> TaskId {
        self.id
    }
}

impl ArcWake for ScheduledTask {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        {
            let mut queue = arc_self.queue.lock().expect("scheduler queue poisoned");
            queue.push_back(arc_self.clone());
        }
        arc_self.available.notify_one();
    }
}
