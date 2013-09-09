;;; -*- Mode: LFE; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File gps1.lisp: First version of GPS (General Problem Solver)

;;;; Converted to LFE by Robert Virding

;; Define macros for global variable access. This is a hack and very naughty!
(defsyntax defvar
  ([name val] (let ((v val)) (put 'name v) v)))

(defsyntax setvar
  ([name val] (let ((v val)) (put 'name v) v)))

(defsyntax getvar
  ([name] (get 'name)))

;; Module definition.

(defmodule gps1
  (export (gps 2) (gps 3) (school-ops 0))
  (import (from lists (member 2) (all 2) (any 2))
      ;; Rename lists functions to be more CL like.
      (rename lists ((all 2) every) ((any 2) some) ((filter 2) find-all))))

;; An operation.
(defrecord op
  action preconds add-list del-list)

;; General Problem Solver: achieve all goals using *ops*.
(defun gps (state goals ops)
  ;; Set global variables
  (defvar *state* state)    ;The current state: a list of conditions.
  (defvar *ops* ops)        ;A list of available operators.
  (if (every (fun achieve 1) goals) 'solved))

(defun gps (state goals)
  ;; Set global variables, but use existing *ops*
  (defvar *state* state)    ;The current state: a list of conditions.
  (if (every (fun achieve 1) goals) 'solved))

;; A goal is achieved if it already holds or if there is an
;; appropriate op for it that is applicable."
(defun achieve (goal)
  (orelse (member goal (getvar *state*))
      (some (fun apply-op 1)
        (find-all (lambda (op) (appropriate-p goal op))
              (getvar *ops*)))))

;; An op is appropriate to a goal if it is in its add list.
(defun appropriate-p (goal op)
  (member goal (op-add-list op)))

;; Print a message and update *state* if op is applicable.
(defun apply-op (op)
  (if (every (fun achieve 1) (op-preconds op))
    (progn
      (: io fwrite '"executing ~p\n" (list (op-action op)))
      (setvar *state* (set-difference (getvar *state*) (op-del-list op)))
      (setvar *state* (union (getvar *state*) (op-add-list op)))
      'true)))

;; Define the set functions to work on list, a listsets module really.
(defun set-difference
  ([(cons e es) s2]
   (if (member e s2)
     (set-difference es s2)
     (cons e (set-difference es s2))))
  ([() s2] ()))

(defun union
  ([(cons e es) s2]
   (if (member e s2) (union es s2) (cons e (union es s2))))
  ([() s2] ()))

;;; ==============================

(defun school-ops ()
  (list
    (make-op action 'drive-son-to-school
         preconds '(son-at-home car-works)
         add-list '(son-at-school)
         del-list '(son-at-home))
    (make-op action 'shop-installs-battery
         preconds '(car-needs-battery shop-knows-problem shop-has-money)
         add-list '(car-works)
         del-list ())
    (make-op action 'tell-shop-problem
         preconds '(in-communication-with-shop)
         add-list '(shop-knows-problem)
         del-list ())
    (make-op action 'telephone-shop
         preconds '(know-phone-number)
         add-list '(in-communication-with-shop)
         del-list ())
    (make-op action 'look-up-number
         preconds '(have-phone-book)
         add-list '(know-phone-number)
         del-list ())
    (make-op action 'give-shop-money
         preconds '(have-money)
         add-list '(shop-has-money)
         del-list '(have-money))))
