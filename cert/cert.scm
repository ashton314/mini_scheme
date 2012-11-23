;; Scheme basics
(load "cert/special_forms.scm")
(load "cert/functions.scm")
(load "cert/macros.scm")

(load "cert/timer.scm")
(load "cert/closure.scm")
(load "cert/loop.scm")

(define (full-tests)
  (write-string-err "--- CONTROL TESTS: BEGIN ---")
  (terpri-err)
  (if (control-tests)
      (begin
	(write-string-err "--- CONTROL TESTS: PASS ---")
	(write-string-err "--- TIME TRIALS: BEGIN ---")
	(time-tests)
	(write-string-err "--- TIME TRIALS: FINISH ---")
	#t)
      (begin
	(write-string-err "--- CONTROL TESTS: FAIL ---")
	(write-string-err "--- TIME TRIALS: ABORT ---")
	#f)))

(define (time-tests)
  (write-string-err "--- CLOSURE TIME TRIAL: BEGIN ---")
  (time-trial 1000
	      (closure-tests))
  (write-string-err "--- CLOSURE TIME TRIAL: FINISH ---")
  (terpri-err)
  (write-string-err "--- LOOP TIME TRIAL: BEGIN ---")
  (time-trial 1000
	      (loop-tests))
  (write-string-err "--- LOOP TIME TRIAL: FINISH ---"))

(define (control-tests)
  (and
   (syntax-tests)
   (function-tests)
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

(defmacro (with-gensyms syms . body)
  `(let (,@(map (lambda (n) (list n '(gensym)))))
     ,@body))
