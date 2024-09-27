// Source: https://github.com/c3lang/c3c/blob/master/lib/std/threads/pool.c3
// Permalink: https://github.com/c3lang/c3c/blob/65bea1c/lib/std/threads/pool.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::thread::pool(<SIZE>);
import std::thread;

struct ThreadPool
{
	Mutex mu;
	QueueItem[SIZE] queue;
	usz qindex;
	usz num_threads;
	bitstruct : char {
		bool initialized;
		bool stop;
		bool stop_now;
	}

	Thread[SIZE] pool;
	ConditionVariable notify;
}

struct QueueItem
{
	ThreadFn func;
	void* arg;
}

/**
 * @require !self.initialized "ThreadPool must not be already initialized"
 **/
fn void! ThreadPool.init(&self)
{
	defer catch @ok(self.destroy());
	*self = { .num_threads = SIZE, .initialized = true };
	self.mu.init()!;
	self.notify.init()!;
	foreach (&thread : self.pool)
	{
		thread.create(&process_work, self)!;
		// The thread resources will be cleaned up when the thread exits.
		thread.detach()!;
	}
}

/*
 * Stop all the threads and cleanup the pool.
 * Any pending work will be dropped.
 */
fn void! ThreadPool.destroy(&self)
{
	return self.@shutdown(stop_now);
}

/*
 * Stop all the threads and cleanup the pool.
 * Any pending work will be processed.
 */
fn void! ThreadPool.stop_and_destroy(&self)
{
	return self.@shutdown(stop);
}

macro void! ThreadPool.@shutdown(&self, #stop) @private
{
	if (self.initialized)
	{
		self.mu.lock()!;
		self.#stop = true;
		self.notify.broadcast()!;
		self.mu.unlock()!;
		// Wait for all threads to shutdown.
		while (true)
		{
			self.mu.lock()!;
			defer self.mu.unlock()!!;
			if (self.num_threads == 0)
			{
				break;
			}
			self.notify.signal()!;
		}
		self.mu.destroy()!;
		self.initialized = false;
	}
}

/*
 * Push a new job to the pool.
 * Returns whether the queue is full, in which case the job is ignored.
 */
fn void! ThreadPool.push(&self, ThreadFn func, void* arg)
{
	while (true)
	{
		self.mu.lock()!;
		defer self.mu.unlock()!!;
		if (self.qindex < SIZE)
		{
			self.queue[self.qindex] = { .func = func, .arg = arg };
			self.qindex++;
			// Notify the threads that work is available.
			self.notify.broadcast()!;
			return;
		}
	}
}

fn int process_work(void* arg) @private
{
	ThreadPool* self = arg;
	while (true)
	{
		self.mu.lock()!!;
		// Wait for work.
		while (self.qindex == 0)
		{
			if (self.stop)
			{
				// Shutdown requested.
				self.num_threads--;
				self.mu.unlock()!!;
				return 0;
			}
			self.notify.wait(&self.mu)!!;
			if (self.stop_now)
			{
				// Shutdown requested.
				self.num_threads--;
				self.mu.unlock()!!;
				return 0;
			}
		}
		// Process the job.
		self.qindex--;
		QueueItem item = self.queue[self.qindex];
		self.mu.unlock()!!;
		item.func(item.arg);
	}
}