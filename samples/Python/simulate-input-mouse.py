import ctypes

def click():
    ctypes.windll.user32.mouse_event(0x2, 0,0,0,0)    # Mouse LClick Down, relative coords, dx=0, dy=0
    ctypes.windll.user32.mouse_event(0x4, 0,0,0,0)    # Mouse LClick Up, relative coords, dx=0, dy=0

click()
