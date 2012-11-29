;; Scheme basics
(load "cert/syntax.scm")
(load "cert/functions.scm")
(load "cert/data_structures.scm")
(load "cert/macros.scm")

(load "cert/timer.scm")
(load "cert/closure.scm")
(load "cert/loop.scm")

(define (full-tests)
  (terpri-err)
  (write-string-err "--- CONTROL TESTS: BEGIN ---")
  (terpri-err)
  (if (control-tests)
      (begin
	(write-string-err "--- CONTROL TESTS: PASS ---")
	(terpri-err)
	(write-string-err "--- TIME TRIALS: BEGIN ---")
	(terpri-err)
	(time-tests)
	(write-string-err "--- TIME TRIALS: FINISH ---")
	(terpri-err)
	#t)
      (begin
	(write-string-err "--- CONTROL TESTS: FAIL ---")
	(terpri-err)
	(write-string-err "--- TIME TRIALS: ABORT ---")
	(terpri-err)
	#f)))

(define (time-tests)
  (write-string-err "--- CLOSURE TIME TRIAL: BEGIN ---")
  (terpri-err)
  (time-trial 1000
	      (closure-tests))
  (write-string-err "--- CLOSURE TIME TRIAL: FINISH ---")
  (terpri-err)
  (write-string-err "--- LOOP TIME TRIAL: BEGIN ---")
  (terpri-err)
  (time-trial 1000
	      (loop-tests))
  (write-string-err "--- LOOP TIME TRIAL: FINISH ---")
  (terpri-err))

(define (control-tests)
  (and
   (syntax-tests)
   (function-tests)
   (data-structure-tests)
   (macro-tests)))

(define (extract sym struct)
  (define (doit cont strc)
    (if (eq? sym (car strc))
	(cons strc cont)
	(if (remove-if-not cons? strc)
	    (let ((conses (remove-if-not cons? strc)))
	      (doit (lambda () (doit cont (cdr conses)))
		    (car conses)))
	    (cont))))
  (doit (lambda () #f) struct))

;; (extract 'foo '((narf (foo zoop)) (quad nil) (foo 42)))

(define-macro (with-gensyms syms . body)
  `(let (,@(map (lambda (n) (list n '(gensym)))))
     ,@body))
