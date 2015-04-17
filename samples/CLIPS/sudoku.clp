;;; http://www.angusj.com/sudoku/hints
;;; http://www.scanraid.com/BasicStrategies.htm
;;; http://www.sudokuoftheday.com/pages/techniques-overview
;;; http://www.sudokuonline.us/sudoku_solving_techniques
;;; http://www.sadmansoftware.com/sudoku/techniques.htm
;;; http://www.krazydad.com/blog/2005/09/29/an-index-of-sudoku-strategies/

;;; #######################
;;; DEFTEMPLATES & DEFFACTS
;;; #######################

(deftemplate possible
   (slot row)
   (slot column)
   (slot value)
   (slot group)
   (slot id))
   
(deftemplate impossible
   (slot id)
   (slot value)
   (slot priority)
   (slot reason))
   
(deftemplate technique-employed
   (slot reason)
   (slot priority))

(deftemplate technique
   (slot name)
   (slot priority))
   
(deffacts startup
   (phase grid-values))

(deftemplate size-value
   (slot size)
   (slot value))
   
(deffacts values
   (size-value (size 1) (value 1))
   (size-value (size 2) (value 2))
   (size-value (size 2) (value 3))
   (size-value (size 2) (value 4))
   (size-value (size 3) (value 5))
   (size-value (size 3) (value 6))
   (size-value (size 3) (value 7))
   (size-value (size 3) (value 8))
   (size-value (size 3) (value 9))
   (size-value (size 4) (value 10))
   (size-value (size 4) (value 11))
   (size-value (size 4) (value 12))
   (size-value (size 4) (value 13))
   (size-value (size 4) (value 14))
   (size-value (size 4) (value 15))
   (size-value (size 4) (value 16))
   (size-value (size 5) (value 17))
   (size-value (size 5) (value 18))
   (size-value (size 5) (value 19))
   (size-value (size 5) (value 20))
   (size-value (size 5) (value 21))
   (size-value (size 5) (value 22))
   (size-value (size 5) (value 23))
   (size-value (size 5) (value 24))
   (size-value (size 5) (value 25)))
   
;;; ###########
;;; SETUP RULES
;;; ###########

;;; ***********
;;; stress-test
;;; ***********

(defrule stress-test
   
   (declare (salience 10))
   
   (phase match)
   
   (stress-test)
   
   (priority ?last)
   
   (not (priority ?p&:(> ?p ?last)))
   
   (technique (priority ?next&:(> ?next ?last)))
   
   (not (technique (priority ?p&:(> ?p ?last)&:(< ?p ?next))))
   
   =>
   
   (assert (priority ?next)))
   
;;; *****************
;;; enable-techniques
;;; *****************

(defrule enable-techniques

   (declare (salience 10))
   
   (phase match)
   
   (size ?)
   
   (not (possible (value any)))
   
   =>
   
   (assert (priority 1)))

;;; **********
;;; expand-any
;;; **********

(defrule expand-any

   (declare (salience 10))

   (phase expand-any)
   
   ?f <- (possible (row ?r) (column ?c) (value any) (group ?g) (id ?id))
  
   (not (possible (value any) (id ?id2&:(< ?id2 ?id))))
   
   (size ?s)
   
   (size-value (size ?as&:(<= ?as ?s)) (value ?v))
   
   (not (possible (row ?r) (column ?c) (value ?v)))
  
   (not (and (size-value (value ?v2&:(< ?v2 ?v)))
               
             (not (possible (row ?r) (column ?c) (value ?v2)))))
   
   =>
   
   (assert (possible (row ?r) (column ?c) (value ?v) (group ?g) (id ?id))))
   
;;; *****************
;;; position-expanded
;;; *****************

(defrule position-expanded

   (declare (salience 10))

   (phase expand-any)
   
   ?f <- (possible (row ?r) (column ?c) (value any) (group ?g) (id ?id))
     
   (size ?s)
   
   (not (and (size-value (size ?as&:(<= ?as ?s)) (value ?v))
   
             (not (possible (row ?r) (column ?c) (value ?v)))))

   =>
   
   (retract ?f))
   
;;; ###########
;;; PHASE RULES
;;; ###########

;;; ***************
;;; expand-any-done
;;; ***************

(defrule expand-any-done

   (declare (salience 10))

   ?f <- (phase expand-any)

   (not (possible (value any)))
   
   =>
   
   (retract ?f)
   
   (assert (phase initial-output))
   (assert (print-position 1 1)))
   
;;; ***********
;;; begin-match
;;; ***********

(defrule begin-match

   (declare (salience -20))
   
   ?f <- (phase initial-output)
      
   =>
   
   (retract ?f)
   
   (assert (phase match)))

;;; *****************
;;; begin-elimination
;;; *****************

(defrule begin-elimination

   (declare (salience -20))
   
   ?f <- (phase match)
   
   (not (not (impossible)))
   
   =>
   
   (retract ?f)
   
   (assert (phase elimination)))

;;; *************
;;; next-priority
;;; *************

(defrule next-priority

   (declare (salience -20))
   
   (phase match)
   
   (not (impossible))
   
   (priority ?last)
   
   (not (priority ?p&:(> ?p ?last)))
   
   (technique (priority ?next&:(> ?next ?last)))
   
   (not (technique (priority ?p&:(> ?p ?last)&:(< ?p ?next))))
   
   =>
   
   (assert (priority ?next)))

;;; ************
;;; begin-output
;;; ************

(defrule begin-output

   (declare (salience -20))
   
   ?f <- (phase match)
   
   (not (impossible))
   
   (priority ?last)
   
   (not (priority ?p&:(> ?p ?last)))

   (not (technique (priority ?next&:(> ?next ?last))))
   
   =>
   
   (retract ?f)
   
   (assert (phase final-output))
   (assert (print-position 1 1)))

   

  
    
   
   
   
   
   
   
   
   
   
