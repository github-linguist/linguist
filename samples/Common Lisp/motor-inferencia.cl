#|
ESCUELA POLITECNICA SUPERIOR - UNIVERSIDAD AUTONOMA DE MADRID
INTELIGENCIA ARTIFICIAL

Motor de inferencia
Basado en parte en "Paradigms of AI Programming: Case Studies
in Common Lisp", de Peter Norvig, 1992
|#


;;;;;;;;;;;;;;;;;;;;;
;;;; Global variables
;;;;;;;;;;;;;;;;;;;;;


(defvar *hypothesis-list*)
(defvar *rule-list*)
(defvar *fact-list*)

;;;;;;;;;;;;;;;;;;;;;
;;;; Constants
;;;;;;;;;;;;;;;;;;;;;

(defconstant +fail+ nil "Indicates unification failure")

(defconstant +no-bindings+ '((nil))
  "Indicates unification success, with no variables.")

(defconstant *mundo-abierto* nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions for the user
;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Resets *fact-list* to NIL
(defun erase-facts () (setq *fact-list* nil))

(defun set-hypothesis-list (h) (setq *hypothesis-list* h))


;; Returns a list of solutions, each one satisfying all the hypothesis contained
;; in *hypothesis-list*
(defun motor-inferencia ()
  (consulta *hypothesis-list*))



;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auxiliary functions
;;;;;;;;;;;;;;;;;;;;;;;;

#|____________________________________________________________________________
FUNCTION: CONSULTA

COMMENTS:
CONSULTA receives a list of hypothesis (variable <hypotheses>), and returns
a list of binding lists (each binding list being a solution).

EXAMPLES:
hypotheses is:
((brothers ?x ?y) (neighbours juan ?x)).

That is, we are searching the brothers of the possible neighbors of Juan.

The function can return in this case:
 
(((?x . sergio) (?y . javier)) ((?x . julian) (?y . mario)) ((?x . julian) (?y . pedro))).
That is, the neighbors of Juan (Sergio and Julian) have 3 brothers in total(Javier, Mario, Pedro)
____________________________________________________________________________|#

(defun consulta (hypotheses)
  (if (null hypotheses) (list +no-bindings+)
    (mapcan #'(lambda (b)
                (mapcar #'(lambda (x) (une-bindings-con-bindings b x))
                  (consulta (subst-bindings b (rest hypotheses)))))
      (find-hypothesis-value (first hypotheses)))))



#|____________________________________________________________________________
FUNCTION: FIND-HYPOTHESIS-VALUE

COMMENTS:
This function manages the query a single query (only one hypothesis) given a binding list.
It tries (in the following order) to:
- Answer the query from *fact-list*
- Answer the query from the rules in *rule-list*
- Ask the user

The function returns a list of solutions (list of binding lists).

EXAMPLES:
If hypothesis is (brothers ?x ?y)
and the function returns:
(((?x . sergio) (?y . javier)) ((?x . julian) (?y . maria)) ((?x . alberto) (?y . pedro))).

Means that Sergio and Javier and brothers, Julian and Mario are brothers, and Alberto and Pedro are brothers.
____________________________________________________________________________|#

(defun find-hypothesis-value (hypothesis)
  (let (rules)
   (cond
    ((equality? hypothesis) 
     (value-from-equality hypothesis))
    ((value-from-facts hypothesis))
    ((setq good-rules (find-rules hypothesis)) 
     (value-from-rules hypothesis good-rules))
    (t (ask-user hypothesis)))))



; une-bindings-con-bindings takes two binding lists and returns a binding list
; Assumes that b1 and b2 are not +fail+
(defun une-bindings-con-bindings (b1 b2)
  (cond
   ((equal b1 +no-bindings+) b2)
   ((equal b2 +no-bindings+) b1)
   (T (append b1 b2))))



#|____________________________________________________________________________
FUNCTION: VALUE-FROM-FACTS

COMMENTS:
Returns all the solutions of <hypothesis> obtained directly from *fact-list*

EXAMPLES:
> (setf *fact-list* '((man luis) (man pedro)(woman mart)(man daniel)(woman laura)))

> (value-from-facts '(man ?x))
returns:

(((?X . LUIS)) ((?X . PEDRO)) ((?X . DANIEL)))
____________________________________________________________________________|#

(defun value-from-facts (hypothesis)
  (mapcan #'(lambda(x) (let ((aux (unify hypothesis x)))
                         (when aux (list aux)))) *fact-list*))




#|____________________________________________________________________________
FUNCTION: FIND-RULES

COMMENTS:
Returns the rules in *rule-list* whose THENs unify with the term given in <hypothesis>
The variables in the rules that satisfy this requirement are renamed.

EXAMPLES:
> (setq *rule-list*
      '((R1 (pertenece ?E (?E . ?_)))
        (R2 (pertenece ?E (?_ . ?Xs)) :- ((pertenece ?E ?Xs)))))

Then:
> (FIND-RULES (PERTENECE 1 (2 5)))
returns:
((R2 (PERTENECE ?E.1 (?_ . ?XS.2)) :- ((PERTENECE ?E.1 ?XS.2))))
That is, only the THEN of rule R2 unify with <hypothesis>

However,
> (FIND-RULES (PERTENECE 1 (1 6 7)))

returns:
((R1 (PERTENECE ?E.6 (?E.6 . ?_)))
 (R2 (PERTENECE ?E.7 (?_ . ?XS.8)) :- ((PERTENECE ?E.7 ?XS.8))))
So the THEN of both rules unify with <hypothesis>
____________________________________________________________________________|#

(defun find-rules (hypothesis)
  (mapcan #'(lambda(b) (let ((renamed-rule (rename-variables b)))
                         (when (in-then? hypothesis renamed-rule)
                           (list renamed-rule)))) *rule-list*))

(defun in-then? (hypothesis rule)
  (unless (null (rule-then rule))
    (not (equal +fail+ (unify hypothesis (rule-then rule))))))



#|____________________________________________________________________________
FUNCTION: VALUE-FROM-RULES

COMMENTS:
Returns all the solutions to <hypothesis> found using all the rules given in
the list <rules>. Note that a single rule can have multiple solutions.
____________________________________________________________________________|#
(defun value-from-rules (hypothesis rules)
  (mapcan #'(lambda (r) (eval-rule hypothesis r)) rules))

(defun limpia-vinculos (termino bindings)
  (unify termino (subst-bindings bindings termino)))


#|____________________________________________________________________________
FUNCTION: EVAL-RULE

COMMENTS:
Returns all the solutions found using the rule given as input argument.

EXAMPLES:
> (setq *rule-list*
      '((R1 (pertenece ?E (?E . ?_)))
        (R2 (pertenece ?E (?_ . ?Xs)) :- ((pertenece ?E ?Xs)))))
Then:
> (EVAL-RULE 
   (PERTENECE 1 (1 6 7)) 
   (R1 (PERTENECE ?E.42 (?E.42 . ?_))))
returns:
(((NIL)))
That is, the query (PERTENECE 1 (1 6 7)) can be proven from the given rule, and
no binding in the variables in the query is necessary (in fact, the query has no variables).
On the other hand:
> (EVAL-RULE 
   (PERTENECE 1 (7)) 
   (R2 (PERTENECE ?E.49 (?_ . ?XS.50)) :- ((PERTENECE ?E.49 ?XS.50))))
returns:
NIL
That is, the query can not be proven from the rule R2.
____________________________________________________________________________|#

(defun eval-rule (hypothesis rule)
  (let ((bindings-then 
          (unify (rule-then rule) hypothesis)))
    (unless (equal +fail+ bindings-then)
      (if (rule-ifs rule)
          (mapcar #'(lambda(b) (limpia-vinculos hypothesis (append bindings-then b)))
            (consulta (subst-bindings bindings-then (rule-ifs rule))))
        (list (limpia-vinculos hypothesis bindings-then))))))


(defun ask-user (hypothesis)
  (let ((question hypothesis))
    (cond
     ((variables-in question) +fail+)
     ((not-in-fact-list? question) +fail+)
     (*mundo-abierto*
      (format t "~%Es cierto el hecho ~S? (T/nil)" question)
      (cond
       ((read) (add-fact question) +no-bindings+)
       (T (add-fact (list 'NOT question)) +fail+)))
     (T +fail+))))


; value-from-equality:
(defun value-from-equality (hypothesis)
  (let ((new-bindings (unify (second hypothesis) (third hypothesis))))
    (if (not (equal +fail+ new-bindings)) 
	(list new-bindings))))



#|____________________________________________________________________________
FUNCTION: UNIFY

COMMENTS:
Finds the most general unifier of two input expressions, taking into account the
bindings specified in the input <bingings>
In case the two expressions can unify, the function returns the total bindings necessary
for that unification. Otherwise, returns +fail+

EXAMPLES:
> (unify '1 '1)
((NIL)) ;; which is the constant +no-bindings+
> (unify 1 '2)
nil     ;; which is the constant +fail+
> (unify '?x 1)
((?x . 1))
> (unify '(1 1) ?x)
((? x 1 1))
> (unify '?_ '?x)
((NIL))
> (unify '(p ?x 1 2) '(p ?y ?_ ?_))
((?x . ?y))
> (unify '(?a . ?_) '(1 2 3)) 
((?a . 1)) 
> (unify '(?_ ?_) '(1 2))
((nil))
> (unify '(?a . ?b) '(1 2 3)) 
((?b 2 3) (?a . 1)) 
> (unify '(?a . ?b) '(?v . ?d)) 
((?b . ?d) (?a . ?v)) 
> (unify '(?eval (+ 1 1)) '1) 
nil
> (unify '(?eval (+ 1 1)) '2) 
(nil)) 
____________________________________________________________________________|#

(defun unify (x y &optional (bindings +no-bindings+))
  "See if x and y match with given bindings.  If they do,
  return a binding list that would make them equal [p 303]."
  (cond ((eq bindings +fail+) +fail+)
        ((eql x y) bindings)
        ((eval? x) (unify-eval x y bindings))
        ((eval? y) (unify-eval y x bindings))
        ((variable? x) (unify-var x y bindings))
        ((variable? y) (unify-var y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y) 
                (unify (first x) (first y) bindings)))
        (t +fail+)))


;; rename-variables: renombra ?X por ?X.1, ?Y por ?Y.2 etc. salvo ?_ que no se renombra
(defun rename-variables (x)
  "Replace all variables in x with new ones. Excepto ?_"
  (sublis (mapcar #'(lambda (var) 
		      (if (anonymous-var? var)
			  (make-binding var var)
			(make-binding var (new-variable var))))
		  (variables-in x))
	  x))



;;;; Auxiliary Functions

(defun unify-var (var x bindings)
  "Unify var with x, using (and maybe extending) bindings [p 303]."
  (cond ((or (anonymous-var? var)(anonymous-var? x)) bindings)
	((get-binding var bindings)
	 (unify (lookup var bindings) x bindings))
	((and (variable? x) (get-binding x bindings))
	 (unify var (lookup x bindings) bindings))
	((occurs-in? var x bindings)
	 +fail+)
	(t (extend-bindings var x bindings))))

(defun variable? (x)
  "Is x a variable (a symbol starting with ?)?"
  (and (symbolp x) (eql (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (append 
   (unless (eq bindings +no-bindings+) bindings)
   (list (make-binding var val))))

(defun occurs-in? (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (variable? x) (get-binding x bindings))
         (occurs-in? var (lookup x bindings) bindings))
        ((consp x) (or (occurs-in? var (first x) bindings)
                       (occurs-in? var (rest x) bindings)))
        (t nil)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings +fail+) +fail+)
        ((eq bindings +no-bindings+) x)
        ((and (listp x) (eq '?eval (car x)))
         (subst-bindings-quote bindings x))
        ((and (variable? x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (cons (subst-bindings bindings (car x)) ;; s/reuse-cons/cons
		 (subst-bindings bindings (cdr x))))))

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (subst-bindings (unify x y) x))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-find-anywhere-if #'variable? exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (pushnew tree found-so-far)
          found-so-far)
    (unique-find-anywhere-if
        predicate
        (first tree)
        (unique-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

(defun find-anywhere-if (predicate tree)
  "Does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (find-anywhere-if predicate (first tree))
          (find-anywhere-if predicate (rest tree)))))

(defun new-variable (var)
  "Create a new variable.  Assumes user never types variables of form ?X.9"
  (gentemp (format nil "~S." var)))
;  (gentemp "?") )
;;;

(defun anonymous-var? (x)
  (eq x '?_))

(defun subst-bindings-quote (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings +fail+) +fail+)
        ((eq bindings +no-bindings+) x)
        ((and (variable? x) (get-binding x bindings))
         (if (variable? (lookup x bindings))
             (subst-bindings-quote bindings (lookup x bindings))
             (subst-bindings-quote bindings (list 'quote (lookup x bindings)))
         )
        )     
        ((atom x) x)
        (t (cons (subst-bindings-quote bindings (car x)) ;; s/reuse-cons/cons
		 (subst-bindings-quote bindings (cdr x))))))

(defun eval? (x)
  (and (consp x) (eq (first x) '?eval)))

(defun unify-eval (x y bindings)
  (let ((exp (subst-bindings-quote bindings (second x))))
    (if (variables-in exp)
	+fail+
      (unify (eval exp) y bindings))))



(defun rule-ifs (rule) (fourth rule))
(defun rule-then (rule) (second rule))


(defun equality? (term)
  (and (consp term) (eql (first term) '?=)))


(defun in-fact-list? (expresion)
  (some #'(lambda(x) (equal x expresion)) *fact-list*))
                     
(defun not-in-fact-list? (expresion)
  (if (eq (car expresion) 'NOT)
      (in-fact-list? (second expresion))
    (in-fact-list? (list 'NOT expresion))))


;; add-fact:

(defun add-fact (fact)
  (setq *fact-list* (cons fact *fact-list*)))


(defun variable? (x)
  "Is x a variable (a symbol starting with ?) except ?eval and ?="
  (and (not (equal x '?eval)) (not (equal x '?=)) 
       (symbolp x) (eql (char (symbol-name x) 0) #\?)))


;; EOF