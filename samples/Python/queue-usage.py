import Queue
my_queue = Queue.Queue()
my_queue.put("foo")
my_queue.put("bar")
my_queue.put("baz")
print my_queue.get()  # foo
print my_queue.get()  # bar
print my_queue.get()  # baz
