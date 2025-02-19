(define-module (statprof)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 format)
  #:export (statprof-active?
            statprof-start
            statprof-stop
            statprof
            gcprof))


(define (sample-stack-procs state stack)
  (set-sample-count! state (+ (sample-count state) 1))

  (let lp ((frame (stack-ref stack 0))
           (len (stack-length stack))
           (buffer (buffer state))
           (pos (buffer-pos state)))
    (define (write-sample sample)
      (vector-set! buffer pos sample))
    (define (continue pos)
      (lp (frame-previous frame) (1- len) buffer pos))
    (define (write-sample-and-continue sample)
      (write-sample sample)
      (continue (1+ pos)))
    (cond
     ((= pos (vector-length buffer))
      (lp frame len (expand-buffer buffer) pos))
     ((or (zero? len) (not frame))
      (write-sample #f)
      (set-buffer! state buffer)
      (set-buffer-pos! state (1+ pos)))
     (else
      (write-sample-and-continue (frame-instruction-pointer frame))))))

(define* (statprof-display-anomalies #:optional (state
                                                 (existing-profiler-state)))
