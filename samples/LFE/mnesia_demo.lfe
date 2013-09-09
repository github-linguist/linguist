;; Copyright (c) 2008-2013 Robert Virding
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

;; File    : mnesia_demo.lfe
;; Author  : Robert Virding
;; Purpose : A simple Mnesia demo file for LFE.

;; This file contains a simple demo of using LFE to access Mnesia
;; tables.  It shows how to use the emp-XXXX macro (ETS match pattern)
;; together with mnesia:match_object, match specifications with
;; mnesia:select and Query List Comprehensions.

(defmodule mnesia_demo
  (export (new 0) (by_place 1) (by_place_ms 1) (by_place_qlc 1)))

(defrecord person name place job)

(defun new ()
  ;; Start mnesia and create a table, we will get an in memory only schema.
  (: mnesia start)
  (: mnesia create_table 'person '(#(attributes (name place job))))
  ;; Initialise the table.
  (let ((people '(
          ;; First some people in London.
          #(fred london waiter)
          #(bert london waiter)
          #(john london painter)
          #(paul london driver)
          ;; Now some in Paris.
          #(jean paris waiter)
          #(gerard paris driver)
          #(claude paris painter)
          #(yves paris waiter)
          ;; And some in Rome.
          #(roberto rome waiter)
          #(guiseppe rome driver)
          #(paulo rome painter)
          ;; And some in Berlin.
          #(fritz berlin painter)
          #(kurt berlin driver)
          #(hans berlin waiter)
          #(franz berlin waiter)
          )))
    (: lists foreach (match-lambda
               ([(tuple n p j)]
            (: mnesia transaction
              (lambda ()
                (let ((new (make-person name n place p job j)))
                  (: mnesia write new))))))
       people)))

;; Match records by place using match_object and the emp-XXXX macro.
(defun by_place (place)
  (: mnesia transaction
    (lambda () (: mnesia match_object (emp-person place place)))))

;; Use match specifications to match records
(defun by_place_ms (place)
  (let ((f (lambda () (: mnesia select 'person
             (match-spec ([(match-person name n place p job j)]
                      (when (=:= p place))
                      (tuple n j)))))))
    (: mnesia transaction f)))

;; Use Query List Comprehensions to match records
(defun by_place_qlc (place)
  (let ((f (lambda ()
         (let ((q (qlc (lc ((<- person (: mnesia table 'person))
                (=:= (person-place person) place))
                 person))))
           (: qlc e q)))))
    (: mnesia transaction f)))
