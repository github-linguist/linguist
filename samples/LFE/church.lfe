;; Copyright (c) 2013 Duncan McGreggor <oubiwann@cogitat.io>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : church.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating church numerals from the lambda calculus

;; The code below was used to create the section of the user guide here:
;;    http://lfe.github.io/user-guide/recursion/5.html
;;
;; Here is some example usage:
;;
;; > (slurp '"church.lfe")
;; #(ok church)
;; > (zero)
;; #Fun<lfe_eval.10.53503600>
;; > (church->int1 (zero))
;; 0
;; > (church->int1 (three))
;; 3
;; > (church->int1 (five))
;; 5
;; > (church->int2 #'five/0)
;; 5
;; > (church->int2 (lambda () (get-church 25)))
;; 25

(defmodule church
  (export all))

(defun zero ()
  (lambda (s)
    (lambda (x) x)))

(defun one ()
  (lambda (s)
    (lambda (x)
      (funcall s x))))

(defun two ()
  (lambda (s)
    (lambda (x)
      (funcall s
        (funcall s x)))))

(defun three ()
  (lambda (s)
    (lambda (x)
      (funcall s
        (funcall s
          (funcall s x))))))

(defun four ()
  (lambda (s)
    (lambda (x)
      (funcall s
        (funcall s
          (funcall s
            (funcall s x)))))))

(defun five ()
  (get-church 5))

(defun int-successor (n)
  (+ n 1))

(defun church->int1 (church-numeral)
  "
  Converts a called church numeral to an integer, e.g.:
  > (church->int1 (five))
  "
  (funcall
    (funcall church-numeral #'int-successor/1) 0))

(defun church->int2 (church-numeral)
  "
  Converts a non-called church numeral to an integer, e.g.:
  > (church->int2 #'five/0)
  "
  (funcall
    (funcall
      (funcall church-numeral) #'int-successor/1) 0))

(defun church-successor (church-numeral)
  (lambda (s)
    (lambda (x)
      (funcall s
        (funcall
          (funcall church-numeral s) x)))))

(defun get-church (church-numeral count limit)
  (cond ((== count limit) church-numeral)
        ((/= count limit)
         (get-church
           (church-successor church-numeral)
           (+ 1 count)
           limit))))

(defun get-church (integer)
  (get-church (zero) 0 integer))
