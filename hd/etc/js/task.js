// Promises are not cancelable in vanilla javascript.
// This wrapper around native promises attempts to mitigate this caveat as
// follows:
// - a task is cancelable and throw an exception if it has been canceled.
// - top level tasks have to been wrapped by an exception handler that
// catch this exception.
export class Task {
  constructor(promise) {
    this.canceled = false;
    this.promise = promise.then(v => {
      if (this.canceled) {
        throw { isCanceled : true };
      }
    });
  }

  then(resolve, reject) {
    this.promise.then(resolve, reject);
  }

  cancel() {
    this.canceled = true;
  }
}

export class TaskQueue {
  constructor(concurrency = 10) {
    this.queue = [];
    this.concurrency = concurrency;
    this.running = 0;
  }

  async run(task) {
    // If we reach the maximum of concurrency tasks, we push the resolver of a
    // fresh promise at the end of the queue. This promise will be resolved
    // only after finishing all the tasks already in the queue.
    if (this.running >= this.concurrency) {
      const promise = new Promise(resolve => this.queue.push(resolve));
      await new Task(promise);
    }

    this.running++;

    try {
      await task();
    } finally {
      this.running--;
      if (this.queue.length > 0) {
        this.queue.shift()();
      }
    }
  }

  cancel_all() {
    this.queue.forEach((task) => task.cancel());
  }
}
