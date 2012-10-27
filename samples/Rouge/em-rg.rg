(ns em-rg
  (:require eventmachine)
  (:use rouge.core ruby))

(defmacro with [& body]
  `(.run EM | []
     ~@body))

(defn stop []
  (.stop_event_loop EM))

(defmacro add-timer [after & body]
  `(.add_timer EM ~after | [] ~@body))

; vim: set ft=clojure:
