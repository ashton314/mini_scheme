(load "cert/timer.scm")
(load "cert/closure.scm")
(load "cert/loop.scm")

(define (time-tests)
  (time-trial 1000
	      (closure-tests))
  (time-trial 1000
	      (loop-tests))
  (time-trial 1000
	      (control-tests)))

(define (control-tests)
  (and
   (syntax-tests)
   (function-tests)
   (macro-tests)))

(define (function-tests)
  (and
   (recursion-tests)))

(define (recursion-tests)
  (define (! n)
    (if (= n 1)
	1
	(* n (! (- n 1)))))
  (= (! 5) 120))

(define (syntax-tests)
  (and 
   (if (= 1 1)
       #t
       (/ 0 0))
   (or #f #t (/ 0 0))))

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
