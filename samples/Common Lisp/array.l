;;; -*- Mode: Lisp; Package: LISP -*-
;;;
;;; This file is part of xyzzy.
;;;

(provide "array")

(in-package "lisp")

(export '(make-vector make-array vector array-dimensions array-in-bounds-p
	  upgraded-array-element-type adjust-array))

(defun upgraded-array-element-type (type)
  (cond ((or (eq type 't)
	     (null type))
	 't)
	((member type '(character base-character standard-char
			extended-character) :test #'eq)
	 'character)
	(t
	 (setq type (car (si:canonicalize-type type)))
	 (cond ((or (eq type 't)
		    (null type))
		't)
	       ((member type '(character base-character standard-char
			       extended-character) :test #'eq)
		'character)
	       (t 't)))))

(defun check-array-initialize-option (ies-p ics-p displaced-to)
  (let ((x 0))
    (and ies-p (incf x))
    (and ics-p (incf x))
    (and displaced-to (incf x))
    (when (> x 1)
      (error ":initial-element, :initial-contents, :displaced-to"))))

(defun make-vector (length &key
			   (element-type t)
			   (initial-element nil ies-p)
			   (initial-contents nil ics-p)
			   fill-pointer
			   adjustable
			   displaced-to
			   (displaced-index-offset 0))
  (setq element-type (upgraded-array-element-type element-type))
  (check-array-initialize-option ies-p ics-p displaced-to)
  (let ((vector (si:*make-vector length element-type initial-element adjustable
				 fill-pointer displaced-to displaced-index-offset)))
    (when ics-p
      (si:*copy-into-seq vector initial-contents))
    vector))

(defun make-array (dimensions &rest rest
			      &key
			      (element-type t)
			      (initial-element nil ies-p)
			      (initial-contents nil ics-p)
			      fill-pointer
			      adjustable
			      displaced-to
			      (displaced-index-offset 0))
  (cond ((integerp dimensions)
	 (apply #'make-vector dimensions rest))
	((= (length dimensions) 1)
	 (apply #'make-vector (car dimensions) rest))
	(t
	 (setq element-type (upgraded-array-element-type element-type))
	 (check-array-initialize-option ies-p ics-p displaced-to)
	 (when fill-pointer
	   (error ":fill-pointer"))
	 (let ((array (si:*make-array dimensions element-type
				      initial-element adjustable
				      displaced-to displaced-index-offset)))
	   (when ics-p
	     (let ((dims (make-list (array-rank array)
				    :initial-element 0))
		   (stack (list initial-contents))
		   (rank (1- (array-rank array))))
	       (dolist (x dims)
		 (push (elt (car stack) 0) stack))
	       (dotimes (i (array-total-size array))
		 (setf (row-major-aref array i) (car stack))
		 (do ((x dims (cdr x))
		      (j rank (1- j)))
		     ((null x))
		   (pop stack)
		   (incf (car x))
		   (when (< (car x) (array-dimension array j))
		     (do ((r (- rank j) (1- r)))
			 ((< r 0))
		       (push (elt (car stack) (nth r dims)) stack))
		     (return))
		   (setf (car x) 0)))))
	   array))))

(defun vector (&rest list)
  (make-vector (length list) :element-type t :initial-contents list))

(defun array-dimensions (array)
  (do ((i (1- (array-rank array)) (1- i))
       (dims '()))
      ((minusp i) dims)
    (push (array-dimension array i) dims)))

(defun array-in-bounds-p (array &rest subscripts)
  (let ((r (array-rank array)))
    (when (/= r (length subscripts))
      (error "subscripts: ~S" subscripts))
    (do ((i 0 (1+ i))
	 (s subscripts (cdr s)))
	((= i r) t)
      (unless (<= 0 (car s) (1- (array-dimension array i)))
	(return nil)))))

(defun adjust-array (old-array
		     dimensions
		     &rest rest
		     &key
		     (element-type nil ets-p)
		     initial-element
		     (initial-contents nil ics-p)
		     (fill-pointer nil fps-p)
		     displaced-to
		     displaced-index-offset)
  (when (/= (length dimensions) (array-rank old-array))
    (error "?"))
  (unless ets-p
    (push (array-element-type old-array) rest)
    (push :element-type rest))
  (when (adjustable-array-p old-array)
    (push t rest)
    (push :adjustable rest))
  (cond (fps-p
	 (unless (array-has-fill-pointer-p old-array)
	   (error "?")))
	(t
	 (when (array-has-fill-pointer-p old-array)
	   (push (fill-pointer old-array) rest)
	   (push :fill-pointer rest))))
  (when (eq old-array displaced-to)
    (error "?"))
  (let ((new-array (apply #'make-array dimensions rest)))
    (or ics-p displaced-to
	(copy-array-partially old-array new-array))
    (cond ((adjustable-array-p old-array)
	   (si:*replace-array old-array new-array)
	   old-array)
	  (t
	   new-array))))

(defun copy-array-partially (src dst)
  (let* ((dims (mapcar #'min (array-dimensions src) (array-dimensions dst)))
	 (r (array-rank src))
	 (s (make-list r :initial-element 0)))
    (setq r (1- r))
    (dotimes (x (apply #'* dims))
      (setf (apply #'aref dst s) (apply #'aref src s))
      (do ((i r (1- i)))
	  ((minusp i))
	(incf (nth i s))
	(when (< (nth i s) (nth i dims))
	  (return))
	(setf (nth i s) 0)))))
