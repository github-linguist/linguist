(ns socket-example
  (:import (java.net Socket)
           (java.io PrintWriter)))

(defn send-data [host msg]
  (with-open [sock (Socket. host 256)
              printer (PrintWriter. (.getOutputStream sock))]
    (.println printer msg)))

(send-data "localhost" "hello socket world")
