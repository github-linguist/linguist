import ctypes

user32_dll = ctypes.cdll.LoadLibrary('User32.dll')
print user32_dll.GetDoubleClickTime()
