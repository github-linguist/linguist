   coinsert'jsocket' [ require 'socket'             NB.  Sockets library
   socket =.  >{.sdcheck sdsocket''                 NB.  Open a socket
   host   =. sdcheck sdgethostbyname 'localhost'    NB.  Resolve host
   sdcheck sdconnect socket ; host ,< 256           NB.  Create connection to port 256
   sdcheck 'hello socket world' sdsend socket , 0   NB.  Send msg
