import os

def get_windows_terminal():
    from ctypes import windll, create_string_buffer
    h = windll.kernel32.GetStdHandle(-12)
    csbi = create_string_buffer(22)
    res = windll.kernel32.GetConsoleScreenBufferInfo(h, csbi)

    #return default size if actual size can't be determined
    if not res: return 80, 25

    import struct
    (bufx, bufy, curx, cury, wattr, left, top, right, bottom, maxx, maxy)\
    = struct.unpack("hhhhHhhhhhh", csbi.raw)
    width = right - left + 1
    height = bottom - top + 1

    return width, height

def get_linux_terminal():
    width = os.popen('tput cols', 'r').readline()
    height = os.popen('tput lines', 'r').readline()

    return int(width), int(height)

print get_linux_terminal() if os.name == 'posix' else get_windows_terminal()
