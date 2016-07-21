;;; common.l
;;; commonLisp features for eus
;;;	
;;;	Copyright(c)1988, Toshihiro MATSUI, Electrotechnical Laboratory
;;;	1986-Aug
;;;	1987-Feb
;;;	1988-Jun	defclass, setf

(in-package "LISP")

(list "@(#)$Id: common.l,v 1.1.1.1 2003/11/20 07:46:30 eus Exp $")

(export '(lisp-implementation-type lisp-implementation-version))

(export '(macroexpand prog1 loop unless until
	  pop push pushnew inc dec incf decf))

(export '(defvar defparameter defconstant deflocal
	dotimes dolist
	do-symbols do-external-symbols do-all-symbols
	psetq do do* prog prog*
	case classcase otherwise
	string alias
	caaar caadr cadar cdaar cdadr cddar cdddr
	fourth fifth sixth seventh eighth 
	cadddr cddddr cadddr caaddr cdaddr caddddr
	flatten list-insert list-delete adjoin union intersection
	set-difference set-exclusive-or rotate-list last copy-tree
	copy-list nreconc rassoc acons member assoc subsetp maplist mapcon))

(export '(find find-if find-if-not position position-if position-if-not
	count count-if count-if-not member-if member-if-not 
	pairlis make-list make-sequence fill replace
	transpose-list
	remove remove-if remove-if-not delete delete-if delete-if-not
	substitute substitute-if substitute-if-not 
	nsubstitute nsubstitute-if nsubstitute-if-not
	unique remove-duplicates extream
	send-super-lexpr send-lexpr send-super send-all resend
	send-super* send*
	instance instance* 
	make-instance defclassmethod delete-method
	make-class defstruct defclass readtablep copy-readtable
	set-syntax-from-char
	collect-if collect-instances
))

(export '(keywordp constantp functionp vector-class-p
	compiled-function-p input-stream-p output-stream-p io-stream-p
	special-form-p macro-function))

(export '(zerop plusp minusp oddp evenp /= logandc1 logandc2
	ecase every some reduce merge-list merge expt signum
	defsetf define-setf-method 
	setf multiple-value-bind multiple-value-setq pop push))
(export '(get-internal-run-time  list-length values
	 first second third bye))

(export '(rad2deg deg2rad ))

;; version
(defun lisp-implementation-type () "EusLisp")
(defun lisp-implementation-version ()
	(format nil
	        "EusLisp ~A~A for ~A created on ~A(~A)"
		(car lisp-implementation-version)
		(cdddr lisp-implementation-version)
		*OS-VERSION*
		(cadr lisp-implementation-version)
                (caddr lisp-implementation-version)
                ))
(setq euserror nil)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (load eval)

(defun macroexpand (form)
  (let ((r (macroexpand2 form)))
    (while (and (listp r) (macro-function (car r)))
	(setq r (macroexpand2 r)))
    r))

;(defmacro defun (fname &rest fdef)
;   `(progn
;	(setq (',fname . function) (cons 'lambda ',fdef))
;        (remprop ',fname 'builtin-function-entry)
;	',fname))

(defmacro prog1 (&rest args)
   (let ((first (gensym "PROG1")))
      `(let ((,first ,(car args)))
	  (progn . ,(cdr args)) ,first)))

(defmacro loop (&rest forms) 
   (let ((tag (gensym "LOOP")))
       `(block nil (tagbody ,tag ,@forms (go ,tag)))))
(defmacro unless (pred &rest form)
  `(when (not ,pred) . ,form))
(defmacro until (condition &rest forms)
   `(while (not ,condition) ,@forms))
(defmacro pop (s) `(prog1 (car ,s) (setf ,s (cdr ,s))))
(defmacro push (item place) `(setf ,place (cons ,item ,place)))
(defmacro pushnew (item place &key test test-not key)
   `(progn (if (not (member ,item ,place :test ,test :test-not ,test-not
					 :key ,key))
		(setf ,place (cons ,item ,place)))
	   nil))
(defmacro inc (var &optional h)
   (if h (setq h (list '+ var h)) (setq h (list '1+ var)))
   (list 'setq var h))
(defmacro dec (var &optional h)
   (if h (setq h (list '- var h)) (setq h (list '1- var)))
   (list 'setq var h))
(defmacro incf (var &optional h)
   (if h (setq h (list '+ var h)) (setq h (list '1+ var)))
   (list 'setf var h))
(defmacro decf (var &optional h)
   (if h (setq h (list '- var h)) (setq h (list '1- var)))
   (list 'setf var h))

(defmacro defvar (var &optional (init nil) (doc nil))
   (unless (symbolp var) (error 20))
  `(when (eql (send ',var :vtype) 1) 
	(send ',var :vtype 2)
        (if (not (boundp ',var))
	    (send ',var :global ,init ,doc ))
	',var))

(defmacro deflocal (var &optional (init nil) (doc nil))
   (unless (symbolp var) (error 20))
   `(progn
	(send ',var :special ,init ,doc)
	',var))

(defmacro defparameter (var init &optional (doc nil))
   (unless (symbolp var) (error 20))
   `(send ',var :global ,init ,doc))

(defmacro defconstant (sym val &optional doc)
   (unless (symbolp sym) (error 20))
   `(send ',sym :constant ,val ,doc) )
  

(defmacro dotimes (vars &rest forms)
  (let ((endvar (gensym "DOTIMES")))
     `(let ((,(car vars) 0) (,endvar ,(cadr vars)))
	(declare (integer ,(car vars) ,endvar))
	(while (< ,(car vars) ,endvar)
	       ,@forms
	       (setq ,(car vars) (1+ ,(car vars))))
	,(caddr vars)))) 

(defmacro dolist (vars &rest forms)
   (let ((lists (gensym "DOLIST")) (decl (car forms)))
     (if (and (consp decl) (eq (car decl) 'declare))
	 (setq forms (cdr forms))
	 (setq decl nil))
     `(let ((,(car vars) nil) (,lists ,(cadr vars)))
	,decl
	(while ,lists
	   (setq ,(car vars) (pop ,lists))
	   ,@forms)
	,(caddr vars)))) 

(defmacro do-symbols (vars &rest forms)
   (let* ((symbols (gensym "DOSYM"))
	  (v (car vars))
	  (pkg (if (cadr vars) (cadr vars) '*package*))
	  (pkgv (gensym))
	  (i (gensym))
	  (size (gensym))
	  (svec (gensym))
	  )
   `(let* ((,v nil)
	   (,pkgv (find-package ,pkg))
	   (,i 0)
	   (,svec (,pkgv . intsymvector))
	   (,size (length ,svec)))
	(while (< ,i ,size)
	   (setq ,v (elt ,svec ,i))
	   (inc ,i)
	   (when (symbolp ,v) . ,forms))
	,(caddr vars))))

(defmacro do-external-symbols (vars &rest forms)
   (let* ((symbols (gensym "DOEXTSYM"))
	  (v (car vars))
	  (pkg (if (cadr vars) (cadr vars) '*package*))
	  (pkgv (gensym))
	  (i (gensym))
	  (size (gensym))
	  (svec (gensym))
	  )
   `(let* ((,v nil)
	   (,pkgv (find-package ,pkg))
	   (,i 0)
	   (,svec (,pkgv . symvector))
	   (,size (length ,svec)))
	(while (< ,i ,size)
	   (setq ,v (elt ,svec ,i))
	   (inc ,i)
	   (when (symbolp ,v) . ,forms))
	,(caddr vars))))

(defmacro do-all-symbols (var &rest forms)
   (let ((apackage (gensym "DOALLSYM")))
      `(dolist (,apackage (list-all-packages) ,(cadr var))
	   (do-symbols (,(car var) ,apackage)  . ,forms) )
       ))

(defmacro psetq (&rest varvals)
   (let* (vars vals gvars)
      (while varvals
	(push (pop varvals) vars)
	(push (pop varvals) vals)
	(push (gensym "PSETQ") gvars))
     (setq vars (nreverse vars) vals (nreverse vals) gvars (nreverse gvars))
     `(let* ,(mapcar #'list gvars vals)
	(setq . ,(mapcan #'list vars gvars))
	nil)))

(defmacro do (vars endtest &rest body)
  (let ((decl (car body)) (tag (gensym "DO")))
   (if  (and (consp decl) (eq (car decl) 'declare))
	(setq body (cdr body))
	(setq decl nil))
   `(block nil
     (let
      ,(mapcar
	  #'(lambda (v) (list (car v) (cadr v)))
	  vars)
      ,decl
      (tagbody
	 ,tag
	 (if ,(car endtest) (return (progn . ,(cdr endtest))))
	 ,@body
	 (psetq . ,(mapcan #'(lambda (v) (if (cddr v) (list (car v) (caddr v))))
		       vars))
	 (go ,tag))) )))

(defmacro do* (vars endtest &rest body)
  (let ((decl (car body)) (tag (gensym "DO*")))
   (if  (and (consp decl) (eq (car decl) 'declare))
	(setq body (cdr body))
	(setq decl nil))
   `(block nil
     (let*
      ,(mapcar
	  #'(lambda (v) (list (car v) (cadr v)))
	  vars)
      ,decl
      (tagbody
	 ,tag
	 (if ,(car endtest) (return (progn . ,(cdr endtest))))
	 ,@body
	 (setq . ,(mapcan #'(lambda (v) (if (cddr v) (list (car v) (caddr v))))
		       vars))
	(go ,tag))) )))


(defmacro prog (vars &rest body)
  `(block nil
      (let ,vars 
	 (tagbody ,@body))))
(defmacro prog* (vars &rest body)
  `(block nil
      (let* ,vars 
	 (tagbody ,@body))))

)
;;
;;	case
;;
(eval-when (load eval)
(defun casebody (body) (if (cdr body) (cons 'progn body) (car body)))

(defun casehead (keyvar head)
   (if (atom head)
	(if (memq head '(t otherwise))
	    t
	    (list 'eq keyvar (list 'quote head)))
       (list 'memq keyvar (list 'quote head)) ))

(defun case1 (keyvar clauses)
  (if (atom clauses)
      nil
      (list 'if
	    (casehead keyvar (caar clauses))
            (casebody (cdar clauses))
	    (case1 keyvar (cdr clauses)) nil)))

(defmacro case (key &rest clauses)
  (let ((keyvar (gensym "CASE")) (result nil))
     (list 'let (list (list keyvar key)) (case1 keyvar clauses))
  ))

(defun classcasehead (keyvar head)
  (if (memq head '(t otherwise))
      t
      (if (atom head)
          `(derivedp ,keyvar ,head)
          `(or . ,(mapcar #'(lambda (x) `(derivedp ,keyvar ,x)) head)))))

(defun classcase1 (keyvar clauses)
  (if (atom clauses)
      nil
      (list 'if
	    (classcasehead keyvar (caar clauses))
            (casebody (cdar clauses))
	    (classcase1 keyvar (cdr clauses)) nil)))

(defmacro classcase (key &rest clauses)
   (let ((kv (gensym "CCASE")))
      `(let ((,kv ,key)) ,(classcase1 kv clauses))))
)

;; string

(defun string (x)
  (if (stringp x) x
      (if (symbolp x) (copy-seq (x . pname))
	  (if (numberp x) (format nil "~d" x)
	      (error x)))))

;
; more list functions
;
(eval-when (load eval)
   (defun alias (new old) (setslot new symbol 'function
				   (symbol-function old)))
   (alias 'list-length 'length)
   (alias 'values 'list)
   )

(eval-when (load eval)
(defun caaar (x) (car (caar x)))
(defun caadr (x) (car (cadr x)))
(defun cadar (x) (car (cdar x)))
(defun cdaar (x) (cdr (caar x)))
(defun cdadr (x) (cdr (cadr x)))
(defun cddar (x) (cdr (cdar x)))
(defun cdddr (x) (cdr (cddr x)))
(alias 'first 'car)
(alias 'second 'cadr)
(alias 'third 'caddr)
(defun fourth (x) (cadr (cddr x)))
(defun fifth  (x) (caddr (cddr x)))
(defun sixth  (x) (caddr (cdddr x)))
(defun seventh  (x) (caddr (cddddr x)))
(defun eighth  (x) (cadddr (cddddr x)))
#|
(defun cadddr (x) (car (cdddr x)))
|#
(defun cddddr (x) (cddr (cddr x)))
(defun cadddr (x) (cadr (cddr x)))
(defun caaddr (x) (caar (cddr x)))
(defun cdaddr (x) (cdar (cddr x)))
(defun caddddr (x) (cadr (cdddr x)))
(defun flatten (l &optional accumulator)
  (cond
   ((null l) accumulator)
   ((atom l) (cons l accumulator))
   (t (flatten (car l)
	       (flatten (cdr l) accumulator)))) )

(defun list-insert (item pos list)
   "insert item as the pos'th element in list.
if pos is bigger than the length of list, item is nconc'ed at the tail"
   (cond ((null list) (list item))
	 ((>= pos (length list)) (nconc list (list item)))
	 ((= pos 0) (cons item list))
	 (t (let ((tail (cons item (nthcdr pos list))))
		(rplacd (nthcdr  (1- pos) list) tail)
		list))))

(defun list-delete (lst n)	"(lst n) delete nth element of lst"
   (if (= n 0) 
	(setq lst (cdr lst))
	(rplacd (nthcdr (1- n) lst) (nthcdr (1+ n) lst))  )
   lst)

(defun adjoin (item list &key (test #'eq) (test-not) (key #'identity))
  (if (member item list :test test :test-not test-not :key key)
      list
      (cons item list)))

(defun union (list1 list2 &key (test #'eq) (test-not) (key #'identity))
  (let (result)
    (dolist (item list1)
      (unless (member (funcall key item) result
		      :test test :test-not test-not :key key)
	  (setq result (cons item result))))
    (dolist (item list2)
      (unless (member  (funcall key item) result 
		       :test test :test-not test-not :key key)
	  (setq result (cons item result))))
    (reverse result)))

(defun intersection (list1 list2 &key (test #'eq) (test-not) (key #'identity))
   (let (r)
      (dolist (item list1)
	(if (member (funcall key item) list2
		    :test test :test-not test-not :key key)
	    (setq r (cons item r))))
      r))

(defun set-difference (list1 list2 &key (test #'eq) (test-not)
					(key #'identity))
   (let (result)
     (dolist (l1 list1)
	(unless (member (funcall key l1) list2
			:test test :test-not test-not :key key)
	   (push l1 result)))
     (nreverse result)))

(defun set-exclusive-or (list1 list2 &key (test #'eq) (test-not)
					  (key #'identity))
   (let (result1 result2)
     (dolist (l1 list1)
	(setq l1 (funcall key l1))
	(unless (member l1 list2 :test test :test-not test-not :key key)
	   (push l1 result1)))
     (dolist (l2 list2)
	(setq l2 (funcall key l2))
	(unless (member l2 list1 :test test :test-not test-not :key key)
	   (push l2 result2)))
     (nconc result1 result2)))

(defun rotate-list (l) (append (cdr l) (list (car l))))
(defun last (x)
  (while (consp (cdr x)) (setq x (cdr x)))
  x)
(defun copy-tree (x) (subst t t x))
(defun copy-list (x) (nreverse (reverse x)))
(defun nreconc (x y) (nconc (nreverse x) y))
(defun rassoc (item alist)
  (dolist (a alist)
     (if (equal item (cdr a)) (return-from rassoc a))))
(defun acons (key datum alist) (cons (cons key datum) alist))
(defun member (item list &key key test test-not)
   (supermember item list key test test-not))
(defun assoc (item alist &key key test test-not)
   (superassoc item alist key test test-not))
(defun subsetp (sub super &key key test test-not)
   (every #'(lambda (s) (member s super :key key :test test :test-not test-not))
	  sub))
(defun maplist (func arg &rest more-args &aux result)
   (if more-args
       (let (arglist margs)
	  (while arg
	     (setq arglist nil)
	     (push arg arglist)
	     (setq arg (cdr arg))
	     (setq margs more-args)
	     (while margs
		(push (car margs) arglist)
		(setf (car margs) (cdar margs))
		(setq margs (cdr margs)) )
	     (push (apply func (nreverse arglist)) result) ))
        (while arg
	    (push (funcall func arg) result)
	    (setq arg (cdr arg)))) 
   (nreverse result))

(defun mapcon (func arg &rest more-args &aux result)
   (if more-args
       (let (arglist margs)
	  (while arg
	     (setq arglist nil)
	     (push arg arglist)
	     (setq arg (cdr arg))
	     (setq margs more-args)
	     (while margs
		(push (car margs) arglist)
		(setf (car margs) (cdar margs))
		(setq margs (cdr margs)) )
	     (setq result (nconc (apply func (nreverse arglist)) result) )) )
        (while arg
	    (setq result (nconc (funcall func arg) result))
	    (setq arg (cdr arg)))) 
   (nreverse result))

(defun find (item seq &key (start 0) (end (length seq))
		           (test #'eq) (test-not nil) (key #'identity))
   (system::raw-find item seq test test-not key nil nil start end))
(defun find-if (pred seq &key (start 0) (end (length seq)) (key #'identity))
   (system::raw-find nil seq nil nil key pred nil start end))
(defun find-if-not (pred seq &key (start 0) (end (length seq)) (key #'identity))
   (system::raw-find nil seq nil nil key nil pred start end))

(defun position (item seq &key (start 0) (end (length seq)) (count 1)
		           (test #'eq) (test-not nil) (key #'identity))
   (system::raw-position item seq test test-not key nil nil start end count))
(defun position-if (pred seq &key (start 0) (end (length seq)) (count 1) (key #'identity))
   (system::raw-position nil seq nil nil key pred nil start end count))
(defun position-if-not (pred seq &key (start 0) (end (length seq)) (count 1) (key #'identity))
   (system::raw-position nil seq nil nil key nil pred start end count))

(defun count (item seq &key (start 0) (end (length seq))
		           (test #'eq) (test-not nil) (key #'identity))
   (system::raw-count item seq test test-not key nil nil start end))
(defun count-if (pred seq &key (start 0) (end (length seq)) (key #'identity))
   (system::raw-count nil seq nil nil key pred nil start end))
(defun count-if-not (pred seq &key (start 0) (end (length seq)) (key #'identity))
   (system::raw-count nil seq nil nil key nil pred start end))
(defun member-if (test list &key (key #'identity))
   (while list
      (if (funcall test (funcall key (car list))) 
	  (return-from member-if list)
	  (setq list (cdr list)))))
(defun member-if-not (test list &key (key #'identity))
   (while list
      (if (not (funcall test (funcall key (car list))) )
	  (return-from member-if-not list)
	  (setq list (cdr list)))))
(defun collect-if (func seq &aux r)
   (dolist (s seq)
      (if (funcall func s) (push s r)) )
   (nreverse r) )
(defun collect-instances (klass list)
   (collect-if #'(lambda (i) (derivedp i klass)) (flatten list)))

(defun pairlis (l1 l2 &optional alist)
   (if l1
       (cons (cons (car l1) (car l2)) (pairlis (cdr l1) (cdr l2) alist))
       alist))

(defun transpose-list (dlist)
   (let (r)
      (dotimes (i (length (car dlist)))
	 (push (mapcar #'(lambda (x) (nth i x)) dlist) r))
      (nreverse r)))

(defun make-list (leng &key initial-element)
  (let (r)
   (unless (integerp leng) (error "integer required for length of make-list"))
   (dotimes (i leng r)
	(push initial-element r))))

(defun make-sequence (type size &key initial-element)
  (if (or (memq type '(cons list)) (eq type cons))
      (make-list size :initial-element initial-element)
      (make-array size :element-type type :initial-element initial-element)))

(defun fill (seq item &key (start 0) (end (length seq)))
   (system::raw-fill seq item start end))

(defun replace (dest src &key (start1 0) (end1 (length dest))
			      (start2 0) (end2 (length src)))
  (let ((result dest) (count (min (- end1 start1) (- end2 start2))))
   (cond ((listp dest)
	  (setq dest (nthcdr start1 dest))
	  (cond ((listp src)
		 (setq src (nthcdr start2 src))
		 (dotimes (c count)
		    (setq (dest . car) (pop src))
		    (pop dest)))
		(t
		  (dotimes (c count)
		    (setq (dest . car) (aref src start2))
		    (inc start2) (pop dest)))))
	((listp src)	; list --> vector
	 (setq src (nthcdr start2 src))
	 (dotimes (c count)
	    (aset dest start1 (pop src))
	    (inc start1)))
	(t (system::vector-replace dest src start1 end1 start2 end2)))
    result))

(defun remove (item seq &key (start 0) (end (length seq))
			      (test #'eq) (test-not nil)
			      (count 1000000) (key #'identity))
    (system::universal-remove item seq test test-not key nil nil start end count))
(defun remove-if (pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::universal-remove nil seq nil nil key pred nil start end count))
(defun remove-if-not (pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::universal-remove nil seq nil nil key nil pred start end count))

(defun delete (item seq &key (start 0) (end (length seq))
			      (test #'eq) (test-not nil)
			      (count 1000000) (key #'identity))
    (system::raw-delete item seq test test-not key nil nil start end count))
(defun delete-if (pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::raw-delete nil seq nil nil key pred nil start end count))
(defun delete-if-not (pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::raw-delete nil seq nil nil key nil pred start end count))

(defun substitute (newitem olditem seq &key (start 0) (end (length seq))
			      (test #'eq) (test-not nil)
			      (count 1000000) (key #'identity))
    (system::raw-substitute newitem olditem seq test test-not key nil nil start end count))
(defun substitute-if (newitem pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::raw-substitute newitem nil seq nil nil key pred nil start end count))
(defun substitute-if-not (newitem pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::raw-substitute newitem nil seq nil nil key nil pred start end count))

(defun nsubstitute (newitem olditem seq &key (start 0) (end (length seq))
			      (test #'eq) (test-not nil)
			      (count 1000000) (key #'identity))
    (system::raw-nsubstitute newitem olditem seq test test-not key nil nil start end count))
(defun nsubstitute-if (newitem pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::raw-nsubstitute newitem nil seq nil nil key pred nil start end count))
(defun nsubstitute-if-not (newitem pred seq &key (start 0) (end (length seq))
			      (count 1000000) (key #'identity))
    (system::raw-nsubstitute newitem nil seq nil nil key nil pred start end count))

(defun unique (l)
   (cond
      ((atom (cdr l)) l)
      ((memq (car l) (cdr l)) (unique (cdr l)))
      (t (cons (car l) (unique (cdr l))))))

(defun remove-duplicates (seq &key (test #'eq) (test-not) (key #'identity)
				   (start 0) (end 1000000))
   (system::raw-remove-duplicates seq test test-not key start end))

(defun extream (seq test &optional (key #'identity))
   (if (null seq)
       nil
       (let* ((ext (elt seq 0))  (p (funcall key ext)) x)
	  (if (consp seq)
	      (dolist (v (cdr seq))
		   (when (funcall test (funcall key v) p)
		      (setq ext v
			    p   (funcall key ext))) ) 
	      (dotimes (i (length seq))
		   (when (funcall test
				  (funcall key (setq x (aref seq i)))
				  p)
		      (setq ext x
			    p   (funcall key ext)))) )
          ext)) )
) ;eval-when


;;;
;;; equivalent pairs from WINSTON
;;;

#|
(eval-when (load eval)
(defun coalesce (pairs) (coalesce-aux pairs nil))
(defun coalesce-aux (pairs classes)
  (cond ((null pairs) classes)
	(t (coalesce-aux (cdr pairs)
			 (absorb (car pairs) classes)))))
(defun stick-in (new classes)
  (cond ((member new (car classes)) classes)
	(t (cons (cons new (car classes))
		 (cdr classes)))))
(defun absorb (pair classes)
  (cond ((null classes) (list pair))
	((member (car pair) (car classes))
	 (stick-in (cadr pair) classes))
	((member (cadr pair) (car classes))
	 (stick-in (car pair) classes))
	(t (cons (car classes)
		 (absorb pair (cdr classes))))))
) ;eval-when ; end of more list functions
|#


;;;
;;; LEO functions
;;;
(eval-when (load eval)
(defmacro send-super-lexpr (selector &rest msgs)
  (declare (type metaclass class))
  `(apply #'send-message self (class . super) ,selector . ,msgs)
  )
(defmacro send-super* (&rest msgs)
  (declare (type metaclass class))
  `(apply #'send-message self (class . super) . ,msgs)
  )
(defmacro send-lexpr (target selector &rest msgs)
  `(apply #'send ,target ,selector . ,msgs)
  )
(defmacro send* (&rest msgs)
  `(apply #'send . ,msgs)
  )
(defmacro send-super (selector &rest msgs)
  (declare (type metaclass class))
  `(send-message self (class . super) ,selector ,@msgs))

(defun send-all (receivers &rest mesg)
  (mapcar #'(lambda (r) (apply 'send r mesg)) receivers))

(defun resend (obj mesg)
   (eval (cons 'send (cons obj mesg))))

(defmacro instance (cls &rest message)
  (if message
	(let ((v (gensym "INST")))
	`(let ((,v (instantiate ,cls))) (send ,v ,@message) ,v))
	`(instantiate ,cls)))

(defmacro instance* (cls &rest message)
  (if message
	(let ((v (gensym "INST")))
	`(let ((,v (instantiate ,cls))) (send* ,v ,@message) ,v))
	`(instantiate ,cls)))

(defun make-instance (klass &rest args)
   (let ((inst (instantiate klass)))
      (while args
	 (setslot inst klass (string (pop args)) (pop args)))
      inst))

(defmacro defclassmethod (classname &rest methods)
    `(defmethod ,(metaclass-name  (class (symbol-value classname)))
		. ,methods))
(defun delete-method (classobj methodname)
   (setf (metaclass-methods classobj)
	 (delete methodname (metaclass-methods classobj) :key #'car))
   (system::method-cache t))

;;;
;;;	defclass macro (T.Matsui 1988-Jun)
;;;

(defun make-class (name &key
				(super object)	
				(include object)
				(printer nil)
				(constructor nil)
				(predicate nil)
				(copier nil)
				((:metaclass metaklass) nil)
				(element-type nil)
				(size -1)
				((:slots varlist) nil) 
				(documentation nil))
  (if (symbolp super) (setq super (symbol-value super)))
  (let ((classobj (if (boundp name) (symbol-value name)))
        (variables) (types) (forwards)
        (etype) (index 0) (accessor) (p))
     (cond ((null (classp classobj))
		(cond 
		    (metaklass)
		    ((classp metaklass))
		    (super (setq metaklass (class super)))
		    (t     (setq metaklass (symbol-value 'metaclass))))
		(setq classobj (instantiate metaklass)))
	   (t (setq metaklass (class classobj))))
     (setq variables (nreverse (coerce (super . vars) cons))
	   types (nreverse (coerce (super . types) cons))
	   forwards (nreverse (coerce (super . forwards) cons)))
     (dolist (v varlist)
	(cond ((consp v)
		(if (member (car v) variables)
		    (error "duplicated object variable name"))
		(push (car v) variables)
		(setq p (position :type v))
		(push (if p (elt v (1+ p)) t) types)
		(setq p (position :forward v))
		(push (if p (elt v (1+ p)) nil) forwards))
	      ((symbolp v)
		(if (member v variables)
		    (error "duplicated object variable name"))
		(push v variables)
		(push t types)
		(push nil forwards))
	      (t (error "variable name expected for :slots"))))
     (setq variables (coerce (nreverse variables) vector)
	   types (coerce (nreverse types) vector)
	   forwards (coerce (nreverse forwards) vector))
     (setq etype (cdr (assq element-type 
			'((:BIT . 1) (:CHAR . 2) (:BYTE . 3)
			  (:INTEGER . 4) (:FLOAT . 5) (:FOREIGN . 6)))))
     (if (null etype)
         (setq etype   (if (subclassp metaklass vectorclass)
			   (vectorclass-element-type super)
			   0)))
     (setq (classobj . name) name
	   (classobj . vars) variables
	   (classobj . types) types
	   (classobj . forwards) forwards
	   (classobj . super) super)
     (if (subclassp metaklass vectorclass)
	 (setq  (classobj . element-type) etype
		(classobj . size) size))
     (if (null (classobj . cix))   (enter-class classobj))
;;;???
;;;     (proclaim (list 'special name))
;;      (set name classobj)
;;     (send name :global classobj)
     (putprop name documentation :class-documentation)
;; define slot access functions and setf methods for all variables
     (setq variables (coerce  variables  cons))
     (dolist (v variables)
	(setq accessor (intern (concatenate string
					    (string name) "-" (string v))))
	(setf (symbol-function accessor)
	      `(macro (obj) (list 'slot obj ',name ,index)))
	(incf index))
     classobj ))


(defmacro defstruct (name &rest slots)
   `(progn
	    (send ',name :global
		(make-class ',name :slots ',slots))
	    ',name))


(defmacro defclass (name &key slots
			      (super 'object)
			      (size -1)
			      ((:metaclass metaklass) nil)
			      element-type
			      documentation
			      (doc documentation))
	`(progn
	    (send ',name :global
	        (make-class ',name
		    :super ,super
		    :slots ',slots
		    :metaclass ,metaklass
		    :element-type ,element-type
		    :size ,size
		    :documentation ,doc) )
	    ',name))


;;; 
;;; READTABLES
;;;
(eval-when (load eval)
(defun readtablep (x) (derivedp x readtable))
(defun copy-readtable (&optional (from *readtable*) (to nil))
   (when (null from) (setq from *default-readtable*))
   (when (null to)
      (setq to (instantiate readtable))
      (setf (readtable-syntax to) (instantiate string 256)
	    (readtable-macro to) (instantiate vector 256)
	    (readtable-dispatch-macro to) (instantiate vector 256)))
   (if (or (null (readtablep from)) (null (readtablep to))) 
       (error "readtable expected"))
   (replace (readtable-syntax to) (readtable-syntax from))
   (replace (readtable-macro to) (readtable-macro from))
   (replace (readtable-dispatch-macro to) (readtable-dispatch-macro from))
   (setf (readtable-case to) (readtable-case from))
   to)

(defun set-syntax-from-char
	 (to-char from-char &optional (to-readtable *readtable*)
				      (from-readtable *default-readtable*))
   (let (syn)
      (setq syn (aref (readtable-syntax from-readtable) from-char))
      (aset (readtable-syntax to-readtable) to-char syn)
      (if (or (eq syn 7) (eq syn 8))
	  (aset (readtable-macro to-readtable) to-char
		(aref (readtable-macro from-readtable) from-char)))
      syn))
)


;;
;;	predicates
;;
(eval-when (load eval)
(defun keywordp (sym)
   (declare (type symbol sym))
   (and (symbolp sym) (eq (sym . homepkg) *keyword-package*)))

(defun constantp (obj)
   (declare (type symbol obj))
   (if (symbolp obj)
	(if (or (keywordp obj) (eq (obj . vtype) 0)) t nil)
	(if (listp obj)
	    (if (eq (car obj) 'quote) t nil)
	    (if (atom obj) t nil))))

(defun functionp (obj)
  (cond ((numberp obj) nil)
	((listp obj) 
	    (if (or (memq (car obj) '(lambda lambda-closure))) t nil))
	((derivedp obj compiled-code)
	  (eq (compiled-code-type obj) 0))
	((and (symbolp obj) (fboundp obj))
	 (functionp (symbol-function obj)))
	(t nil)))

(defun vector-class-p (p) (derivedp p vectorclass))
(defun compiled-function-p (x) (derivedp  x compiled-code))
(defun input-stream-p (obj)
  (declare (stream obj))
  (or (and (derivedp obj stream)  (eq (obj . direction) :input))
      (derivedp obj io-stream)))
(defun output-stream-p (obj)
  (declare (stream obj))
  (or (and (derivedp obj stream)  (eq (obj . direction) :output))
      (derivedp obj io-stream)))
(defun io-stream-p (obj) (derivedp obj io-stream))

(defun special-form-p (s)
  (and (symbolp s)
       (fboundp s)
       (setq s (symbol-function s))
       (compiled-function-p s)
       (eq (s . type) 2)))

(defun macro-function (s)
  (and (symbolp s)
       (fboundp s)
       (setq s (symbol-function s))
       (if (and (compiled-function-p s)
	        (eq (s . type) 1))
	   s
	   (if (and (listp s) (eq (car s) 'macro)) s nil)
       )))

(defun zerop (n) (= n 0))
(defun plusp (n) (> n 0))
(defun minusp (n) (< n 0))
(defun oddp (n) (logbitp 0 n))
(defun evenp (n) (not (logbitp 0 n)))
(defun /= (n1 n2) (not (= n1 n2)))
(defun logandc1 (x y) (logand (lognot x) y))
(defun logandc2 (x y) (logand x (lognot y)))
(defmacro ecase (&rest x) (cons 'case x))

(defun every (pred arg &rest more-args)
  (cond ((and (null more-args) (listp arg))
	 (while arg (unless (funcall pred (pop arg)) (return-from every nil))))
	(t
	 (setq arg (cons arg more-args))
	 (dotimes (i (length (car arg)))
	     (unless (apply pred (mapcar #'(lambda (x) (elt x i)) arg))
		     (return-from every nil)))))
   t)

(defun some (pred arg &rest more-args &aux result)
   (setq arg (cons arg more-args))
   (dotimes (i (length (car arg)))
        (if (setq result (apply pred (mapcar #'(lambda (x) (elt x i)) arg)))
	    (return-from some result)))
   nil)
)

(eval-when (load eval)
(defun reduce (func seq &key (start 0) (end (length seq))
			     from-end initial-value)
   (let ((length (- end start)))
      (when from-end (setq seq (reverse seq)))
      (cond
	 ((and (= length 1) (null initial-value)) (elt seq start))
	 ((= length 0)
	  (if initial-value initial-value (funcall func)))
	 (t
	   (unless initial-value
		(setq initial-value
		      (funcall func (elt seq start) (elt seq (inc start))))
		(dec length 2) (inc start))
	   (dotimes (i length)
	     (setq initial-value
		   (funcall func initial-value (elt seq (+ start i)))))
	   initial-value))))

(defun merge-list (list1 list2 pred key &aux result p1 e1 e2 pp1 pp2)
   (while (and list2  (not (funcall pred (funcall key (car list1))
				         (funcall key (car list2)))))
      (push (pop list2) result))
   (setq result (nreverse result))
   (setq p1 list1)
   (while (and list2 (cdr p1))
      (setq e2 (funcall key (car list2)))
      (while (and p1 (funcall pred (funcall key (cadr p1)) e2)) (pop p1))
      (when p1
	 (setf pp1 (cdr p1)
	       pp2 (cdr list2)
	       (cdr p1) list2
	       (cdr list2) pp1
	       p1 list2
	       list2 pp2)) )
   (nconc result list1 list2))

(defun merge (result-class seq1 seq2 pred &key (key #'identity))
   (if (and (eq result-class cons) (listp seq1) (listp seq2))
       (merge-list seq1 seq2 pred key)
       (let* ((l1 (length seq1)) (l2 (length seq2)) (l (+ l1 l2))
	      (result (make-sequence result-class l))
	      (i1 0) (i2 0) (j 0) (e1) (e2) (e))
          (while (< j l)
	    (cond ((>= i1 l1) (setq e (elt seq2 i2)) (inc i2))
	          ((>= i2 l2) (setq e (elt seq1 i1)) (inc i1))
	          (t (setq e1 (elt seq1 i1)
		           e2 (elt seq2 i2))
		     (if (funcall pred (funcall key e1) (funcall key e2))
		         (progn (inc i1) (setq e e1))
		         (progn (inc i2) (setq e e2)))))
	    (setf (elt result j) e)
	    (inc j))
          result)))
)
		   

;;
;; arithmetics aux
;;

(defun expt (a x)
   (cond ((and (integerp x) (>= x 0))
	  (if (zerop x)
	      1
	      (let ((b a) (y 1) (ix (1- x)))
		(declare (integer y ix))
		(while (> ix 0)
		    (cond ((> ix y)
			    (setq b (* b b)
			    	  ix (- ix y)
			    	  y (ash y 1)))
			  (t (setq b (* b a) ix (1- ix)))))
	        b)))
	 (t (exp (* x (log a))))))
(defun signum (x) (if (zerop x) x (/ x (abs x))))
(defun rad2deg (rad) (/ (* 360.0 rad) 2pi))
(defun deg2rad (deg) (/ (* 2pi deg) 360.0))
)

;;;; (C) Copyright Taiichi Yuasa and Masami Hagiya, 1984. All rights reserved.
;;;;
;;;;                                setf routines
;;;;	Modified by T.Matsui to be run on euslisp
;;;;	1988-Jun-27

;;; DEFSETF macro.
(defmacro defsetf (access-fn &rest rest)
  (cond ((and (car rest) (or (symbolp (car rest)) (functionp (car rest))))
         `(progn (putprop ',access-fn ',(car rest) 'setf-update-fn)
                 (remprop ',access-fn 'setf-lambda)
                 (remprop ',access-fn 'setf-method)
                 (putprop ',access-fn
                             ,(when (not (endp (cdr rest)))
                                    (unless (stringp (cadr rest))
                                            (error "A doc-string expected."))
                                    (unless (endp (cddr rest))
                                            (error "Extra arguments."))
                                    (cadr rest))
                             'setf-documentation)
                 ',access-fn))
	(t
	 (unless (= (list-length (cadr rest)) 1)
		 (error "(store-variable) expected."))
         `(progn (putprop ',access-fn ',rest 'setf-lambda)
                 (remprop ',access-fn 'setf-update-fn)
                 (remprop ',access-fn 'setf-method)
;                 (putprop ',access-fn
;                             ,(find-documentation (cddr rest))
;                             'setf-documentation)
                 ',access-fn))))


;;; DEFINE-SETF-METHOD macro.
(defmacro define-setf-method (access-fn &rest rest)
  `(progn (putprop ',access-fn #'(lambda ,@rest) 'setf-method)
          (remprop ',access-fn 'setf-lambda)
          (remprop ',access-fn 'setf-update-fn)
;          (putprop ',access-fn
;                      ,(find-documentation (cdr rest))
;                      'setf-documentation)
          ',access-fn))

;;; The expansion function for SETF.
(defun setf-expand-1 (place newvalue)
 (let (g)
  (setq place (macroexpand place))
  (cond ((and (consp place) (eq (car place) 'the))
          (setf-expand-1 (caddr place) `(the ,(cadr place) ,newvalue)))
        ((symbolp place)  `(setq ,place ,newvalue))
        ((and (symbolp (car place)) (setq g (get (car place) 'setf-update-fn)))
         `(,g ,@(cdr place) ,newvalue))
        ((and (symbolp (car place))
             (setq g (get (car place) 'structure-access))
             (get (car place) 'setf-lambda)
             (not (eq (car g) 'list))
             (not (eq (car g) 'vector)))
          `(system::structure-set ,(cadr place) ',(car g) ,(cdr g) ,newvalue))
	((macro-function (car place))
	 (setf-expand-1 (macroexpand place) newvalue))
	((setq g (get (car place) 'setf-lambda))
	   (apply (append '(lambda) (list (append (cadr g) (car g))) (cddr g))
		  newvalue (cdr place)))
;	((get (car place) 'setf-method)
;	 (apply (get (car form) 'setf-method) (cdr place)))
	(t (error "SETF?")))))

(defun setf-expand (l)
  (cond ((endp l) nil)
        ((endp (cdr l)) (error "~S is an illegal SETF form." l))
        (t
         (cons (setf-expand-1 (car l) (cadr l))
               (setf-expand (cddr l))))))


;;; SETF macro.
(defmacro setf (&rest rest)
  (cond ((endp rest) nil)
        ((endp (cdr rest)) (error "~S is an illegal SETF form." rest))
        ((endp (cddr rest)) (setf-expand-1 (car rest) (cadr rest)))
        (t (cons 'progn (setf-expand rest)))))

;(defmacro incf (form &optional (d 1))
;  `(setf ,form (+ ,form ,d)))
;(defmacro decf (form &optional (d 1))
;  `(setf ,form (- ,form ,d)))

;;;
;;;  MULTI-VALUE simulation macros
;;;
(defmacro multiple-value-bind (vlist init &rest forms)
   (let* ((inits (gensym "MULT")) (vilist) (count 0))
      (dolist (v vlist)
	 (push (list v `(elt ,inits ,count)) vilist)
         (inc count))
      `(let* ((,inits ,init) . ,vilist) . ,forms)))

(defmacro multiple-value-setq (vlist form)
   (let ((i 0) (tempvar (gensym "MULT")) setq-forms)
     (dolist (v vlist)
	(push (list
		(second
		   (assoc i
			'((0 first) (1 second) (2 third)
			(3 fourth)(4 fifth) (5 sixth) (6 seventh))))
		tempvar)
	      setq-forms)
	(push v setq-forms)
	(incf i))
    `(let ((,tempvar ,form))
	(setq . ,setq-forms))))

(alias 'values  'list)

#|
(defun quick-sort (sequence start end predicate key &aux (j 0) (k 0) exch)
  (declare (fixnum start end j k))
  (when (<= end (the fixnum (1+ start)))
        (return-from quick-sort sequence))
  (setq j start)
  (setq k (1- end))
  (do ((d (aref sequence start)))
      ((> j k))
    (do ()
	((or (> j k)
	     (funcall predicate
		      (funcall key (aref sequence k))
		      (funcall key d))))
      (decf k))
    (when (< k start)
	  (quick-sort sequence (1+ start) end predicate key)
	  (return-from quick-sort sequence))
    (do ()
	((or (> j k)
	     (not (funcall predicate
			   (funcall key (aref sequence j))
			   (funcall key d)))))
      (incf j))
    (when (> j k) (return))
    (setf exch (aref sequence k)
	  (aref sequence k) (aref sequence j)
	  (aref sequence j) exch)
    (incf j)
    (decf k))
  (quick-sort sequence start j predicate key)
  (quick-sort sequence j end predicate key)
  sequence)

(defun qsort (seq test &optional (key #'identity) &aux (vec nil) (s nil))
  (cond ((null seq) nil)
	((listp seq)
	 (setq vec (coerce seq vector))
	 (quick-sort vec 0 (length seq) test key)
	 (setq s seq)
	 (dotimes (i (length vec))
	    (rplaca s (aref vec i))
	    (setq s (cdr s)))
	 seq)
	((vectorp seq)
	 (quick-sort seq 0 (length seq) test key)
	 seq) ))
|#

#|
(eval-when (load eval)
   (defmacro pop (s) `(prog1 (car ,s) (setf ,s (cdr ,s))))
   (defmacro push (item place) `(setf ,place (cons ,item ,place)))
 )
|#


