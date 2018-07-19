import time
import threading

# Only 4 workers can run in the same time
sem = threading.Semaphore(4)

workers = []
running = 1


def worker():
    me = threading.currentThread()
    while 1:
        sem.acquire()
        try:
            if not running:
                break
            print '%s acquired semaphore' % me.getName()
            time.sleep(2.0)
        finally:
            sem.release()
        time.sleep(0.01) # Let others acquire

# Start 10 workers
for i in range(10):
    t = threading.Thread(name=str(i), target=worker)
    workers.append(t)
    t.start()

# Main loop
try:
    while 1:
        time.sleep(0.1)
except KeyboardInterrupt:
    running = 0
    for t in workers:
        t.join()
