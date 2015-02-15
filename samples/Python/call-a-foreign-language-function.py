import ctypes
libc = ctypes.CDLL("/lib/libc.so.6")
libc.strcmp("abc", "def")     # -1
libc.strcmp("hello", "hello") #  0
