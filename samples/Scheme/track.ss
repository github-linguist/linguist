;;;; Main track tooling

;;; Config

;; config/config.ss => config.json
(define (make-config)
  (let ((config.json "config.json"))
    (when (file-exists? config.json)
      (delete-file config.json))
    (with-output-to-file config.json
      (lambda ()
        (json-write (processed-config))))))

;;; Problem Specifications

;; List the files in the given problem's
;; problem-specifications/exercises/problem directory
(define (get-problem-specification problem)
  (let* ((problem-dir (format "../problem-specifications/exercises/~a" problem))
         (spec (directory-list problem-dir)))
    (map (lambda (file)
           (format "~a/~a" problem-dir file))
         spec)))

;; reads the test specification for a given problem
(define (get-test-specification problem)
  (lookup problem (load-specifications)))

;; parse the canonical-data.json file to scheme
(define (get-implemented-test-specification problem)
  (let ((test-suite-file (find (lambda (spec)
                                 (string=? "json" (path-extension spec)))
                               (get-problem-specification problem))))
    (and test-suite-file
         (cons problem
               (with-input-from-file test-suite-file json-read)))))

;; name of the file containing all the persisted specifications
(define specification-file
  "input/specifications.ss")

;; read all the problems with canonical-data.json files and save as
;; a scheme datum
(define (persist-specifications)
  (when (file-exists? specification-file)
    (delete-file specification-file))
  (with-output-to-file specification-file
    (lambda ()
      (pretty-print
       (filter (lambda (x) x)
               (map get-implemented-test-specification
                    (get-problem-list)))))))

;; load saved specifications
(define (load-specifications)
  (with-input-from-file specification-file read))

;; list all the problems in the problem-specifications directory
(define (get-problem-list)
  (map string->symbol (directory-list "../problem-specifications/exercises")))

;;; Test suite

;; read the input/test.ss file as s-expressions
(define *test-definitions*
  (with-input-from-file "input/skeleton-test.ss" read-all))

;;; Problem Implementations

(define (load-problem problem)
  (load (format "input/exercises/~a/test.ss" problem)))

;; table to hold problem implementations
(define *problem-table*
  (make-hash-table))

;; log a problem and its implementation to the problem table. The
;; implementation is specified as an association list with tests, and
;; file paths to the problem skeleton and the problem example
;; solution.
(define (put-problem! problem implementation)
  (for-each (lambda (aspect)
              (unless (assoc aspect implementation)
                (error 'put-test! "problem does not implement" problem aspect)))
            ;; test is an sexpression. skeleton and solution are file paths
            '(test skeleton solution))
  (hashtable-set! *problem-table* problem implementation))

;; look up the problem in the problem table.
(define (get-problem problem)
  (let ((implementation (hashtable-ref *problem-table* problem #f)))
    (or implementation
        (begin
          (load-problem problem)
          (let ((implementation (hashtable-ref *problem-table* problem #f)))
            (unless implementation
              (error 'get-problem "no implementation" problem))
            implementation)))))

;;; Stubbing, Building, and Testing problems

;; Read the problem-specifications directory and generate a stub
;; implementation.
(define (stub-exercism problem)
  (format #t "setting up ~a~%" problem)
  (let* ((dir (format "input/exercises/~a" problem))
         (implementation (format "~a/test.ss" dir))
         ;; todo, add "properties" found in spec to stub skeleton and solution
         (skeleton (format "~a/~a.scm" dir problem))
         (solution (format "~a/example.scm" dir))
         ;; see input/exercises/anagram/anagram.ss for more information
         (stub-implementation
          `(,@'((define (parse-test test)
                  `(test-success (lookup 'description test)
                                 equal?
                                 problem
                                 (lookup 'input test)
                                 (lookup 'expected test)))
                (define (spec->tests spec)
                  (map parse-test (lookup 'cases spec))))
            (let ((spec (get-test-specification ',problem)))
              (put-problem! ',problem
                            `((test . ,(spec->tests spec))
                              (stubs ,problem)
                              (version . (lookup 'version spec))
                              (skeleton . ,,(path-last skeleton))
                              (solution . ,,(path-last solution))
                              (markdown . (splice-exercism ,,problem)))))))
         (stub-solution `((import (rnrs))
                          (define (,problem)
                            'implement-me!))))
    (when (file-exists? implementation)
      (error 'setup-exercism "implementation already exists" problem))
    (system (format "mkdir -p ~a" dir))
    (format #t "~~ writing stub implementation~%")
    (write-expression-to-file stub-implementation implementation)
    (format #t "~~ writing stub solution~%")
    (write-expression-to-file stub-solution skeleton)
    (format #t "~~ writing stub skeleton~%")
    (write-expression-to-file stub-solution solution)))

;; output the problem as specified in input/exercises/problem/* to
;; _build/exercises/problem/*. This is a temporary location to first
;; test the problem before actually writing to exercises/problem/*.
(define (build-exercism problem)
  (let ((implementation (get-problem problem)))
    (let* ((dir (format "_build/exercises/~a" problem))
           (src (format "input/exercises/~a" problem))
           (test.scm (format "~a/test.scm" dir))
           (skeleton.scm (format "~a/~a" src (lookup 'skeleton implementation)))
           (solution.scm (format "~a/~a" src (lookup 'solution implementation))))
      (format #t "writing _build/exercises/~a~%" problem)
      (system
       (format "mkdir -p ~a && cp ~a ~a && cp ~a ~a && cp ~a ~a/Makefile"
               dir skeleton.scm dir solution.scm dir "input/skeleton-makefile" dir))
      (markdown-exercism problem)
      (version-exercism problem)
      (write-expression-to-file
       (apply make-test-file
              (lookup 'test implementation)
              problem
              (lookup 'stubs implementation))
       test.scm))))

;; splice the skeleton test file with the problem's test cases
(define (make-test-file tests problem . stub-defs)
  `((import (except (rnrs) current-output-port))
    ,@*test-definitions*
    ,@(map (lambda (stub-def)
             `(define ,stub-def))
           stub-defs)
    (define test-cases
      (list
       ,@(map (lambda (test)
                `(lambda ()
                   ,test))
              tests)))
    (define (test . query)
      (apply run-test-suite test-cases query))
    (let ((args (command-line)))
      (cond ((null? (cdr args))
	     (load ,(format "~a.scm" problem))
	     (test 'input 'output))
	    ((string=? (cadr args) "--docker")
	     (load ,(format "~a.scm" problem))
	     (run-docker test-cases))
	    (else
	     (load (cadr args))
	     (test 'input 'output))))))

;; output the markdown for the problem
(define (markdown-exercism problem)
  (let* ((markdown (lookup 'markdown (get-problem problem)))
         (target (format "_build/exercises/~a/.meta/hints.md" problem))
         (meta-dir (path-parent target)))
    (unless (file-exists? meta-dir)
      (mkdir (path-parent target)))
    (when (file-exists? target)
      (delete-file target))
    (with-output-to-file target
      (lambda ()
        (put-md markdown)))))

;; if version field is specified, include .meta/version in exercise
;; directory.
(define (version-exercism problem)
  (cond ((assoc 'version (get-problem problem)) =>
         (lambda (version)
           (let* ((target (format "_build/exercises/~a/.meta/version" problem))
                  (meta-dir (path-parent target)))
             (unless (file-exists? meta-dir)
               (mkdir (path-parent target)))
             (when (file-exists? target)
               (delete-file target))
             (with-output-to-file target
               (lambda ()
                 (display (cdr version)))))))))

;; test the problem output in _build/exercises/problem/* by using the
;; skeleton makefile
(define (verify-exercism problem)
  (let ((dir (format "_build/exercises/~a" problem)))
    (check-config-for problem)
    (let ((x (system (format "cd ~a && make check-all solution=example.scm" dir))))
      (unless (zero? x)
        (error 'verify-exercism "example solution incorrect" problem)))
    'done))

;; called if the tests succeed. write the problem to exercises/problem/
(define (include-exercism problem)
  (format #t "including exercises/~a~%" problem)
  (system (format "rm -rf exercises/~a && cp -r _build/exercises/~a exercises/~a"
                  problem problem problem))
  'done)

;; build all implementations in the problem table
(define (build-implementations)
  (for-each build-exercism implementations))

;; test all problems as implemented
(define (verify-implementations)
  (for-each verify-exercism implementations))

;; build/test/write problem
(define (make-exercism problem)
  (build-exercism problem)
  (verify-exercism problem)
  (include-exercism problem))

;; file came from https://github.com/exercism/scheme/blob/main/code/track.ss
;; license is as follows
;; MIT License

;; Copyright (c) 2021 Exercism

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.