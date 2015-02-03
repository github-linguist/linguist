;;;; -*- lisp -*-

(in-package :foo)

;;; Header comment.
(defvar *foo*)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defun add (x &optional y &key z)
    (declare (ignore z))
    ;; Inline comment.
    (+ x (or y 1))))

#|
Multi-line comment.
|#

(defmacro foo (x &body b)
  (if x
      `(1+ ,x)   ;After-line comment.
      42))
