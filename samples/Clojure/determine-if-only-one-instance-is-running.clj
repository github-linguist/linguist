(import (java.net ServerSocket InetAddress))

(def *port* 12345) ; random large port number
(try (new ServerSocket *port* 10 (. InetAddress getLocalHost))
     (catch IOException e (System/exit 0))) ; port taken, so app is already running
