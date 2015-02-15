>>> import sys, math
>>> int(round(math.log(sys.maxint,2)+1)) # this only works in Python 2.x
32
>>> import struct
>>> struct.calcsize('i') * 8
32
>>> sys.byteorder
little
>>> import socket
>>> socket.gethostname()
'PADDY3118-RESTING'
>>>
