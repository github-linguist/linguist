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

;; File    : object.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating simple OOP with closures

;; The simple object system demonstrated below shows how to do the following:
;;  * create objects
;;  * call methods on those objects
;;  * have methods which can call other methods
;;  * update the state of an instance variable
;;
;; Note, however, that his example does not demonstrate inheritance.
;;
;; To use the code below in LFE, do the following:
;;
;;  $ cd examples
;;  $ ../bin/lfe -pa ../ebin
;;
;; Load the file and create a fish-class instance:
;;
;; > (slurp '"object.lfe")
;; #(ok object)
;; > (set mommy-fish (fish-class '"Carp"))
;; #Fun<lfe_eval.10.91765564>
;;
;; Execute some of the basic methods:
;;
;; > (get-species mommy-fish)
;; "Carp"
;; > (move mommy-fish 17)
;; The Carp swam 17 feet!
;; ok
;; > (get-id mommy-fish)
;; "47eebe91a648f042fc3fb278df663de5"
;;
;; Now let's look at "modifying" state data (e.g., children counts):
;;
;; > (get-children mommy-fish)
;; ()
;; > (get-children-count mommy-fish)
;; 0
;; > (set (mommy-fish baby-fish-1) (reproduce mommy-fish))
;; (#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
;; > (get-id mommy-fish)
;; "47eebe91a648f042fc3fb278df663de5"
;; > (get-id baby-fish-1)
;; "fdcf35983bb496650e558a82e34c9935"
;; > (get-children-count mommy-fish)
;; 1
;; > (set (mommy-fish baby-fish-2) (reproduce mommy-fish))
;; (#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
;; > (get-id mommy-fish)
;; "47eebe91a648f042fc3fb278df663de5"
;; > (get-id baby-fish-2)
;; "3e64e5c20fb742dd88dac1032749c2fd"
;; > (get-children-count mommy-fish)
;; 2
;; > (get-info mommy-fish)
;; id: "47eebe91a648f042fc3fb278df663de5"
;; species: "Carp"
;; children: ["fdcf35983bb496650e558a82e34c9935",
;;            "3e64e5c20fb742dd88dac1032749c2fd"]
;; ok

(defmodule object
 (export all))

(defun fish-class (species)
  "
  This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list.
  "
  (fish-class species ()))

(defun fish-class (species children)
  "
  This contructor is mostly useful as a way of abstracting out the id
  generation from the larger constructor. Nothing else uses fish-class/2
  besides fish-class/1, so it's not strictly necessary.

  When the id isn't know, generate one."
  (let* (((binary (id (size 128))) (: crypto rand_bytes 16))
         (formatted-id (car
                         (: io_lib format
                           '"~32.16.0b" (list id)))))
    (fish-class species children formatted-id)))

(defun fish-class (species children id)
  "
  This is the constructor used internally, once the children and fish id are
  known.
  "
  (let ((move-verb '"swam"))
    (lambda (method-name)
      (case method-name
        ('id
          (lambda (self) id))
        ('species
          (lambda (self) species))
        ('children
          (lambda (self) children))
        ('info
          (lambda (self)
            (: io format
              '"id: ~p~nspecies: ~p~nchildren: ~p~n"
              (list (get-id self)
                    (get-species self)
                    (get-children self)))))
        ('move
          (lambda (self distance)
            (: io format
              '"The ~s ~s ~p feet!~n"
              (list species move-verb distance))))
        ('reproduce
          (lambda (self)
            (let* ((child (fish-class species))
                   (child-id (get-id child))
                   (children-ids (: lists append
                                   (list children (list child-id))))
                   (parent-id (get-id self))
                   (parent (fish-class species children-ids parent-id)))
              (list parent child))))
        ('children-count
          (lambda (self)
            (: erlang length children)))))))

(defun get-method (object method-name)
  "
  This is a generic function, used to call into the given object (class
  instance).
  "
  (funcall object method-name))

; define object methods
(defun get-id (object)
  (funcall (get-method object 'id) object))

(defun get-species (object)
  (funcall (get-method object 'species) object))

(defun get-info (object)
  (funcall (get-method object 'info) object))

(defun move (object distance)
  (funcall (get-method object 'move) object distance))

(defun reproduce (object)
  (funcall (get-method object 'reproduce) object))

(defun get-children (object)
  (funcall (get-method object 'children) object))

(defun get-children-count (object)
  (funcall (get-method object 'children-count) object))