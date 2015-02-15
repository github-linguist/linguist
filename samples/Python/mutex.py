import threading
from time import sleep

# res: max number of resources. If changed to 1, it functions
# identically to a mutex/lock object
res = 2
sema = threading.Semaphore(res)

class res_thread(threading.Thread):
	def run(self):
		global res
		n = self.getName()
		for i in range(1, 4):
			# acquire a resource if available and work hard
			# for 2 seconds.  if all res are occupied, block
			# and wait
			sema.acquire()
			res = res - 1
			print n, "+  res count", res
			sleep(2)

                        # after done with resource, return it to pool and flag so
			res = res + 1
			print n, "-  res count", res
			sema.release()

# create 4 threads, each acquire resorce and work
for i in range(1, 5):
	t = res_thread()
	t.start()
