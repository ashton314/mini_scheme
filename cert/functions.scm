(load "cert/test_macro.scm")

(define (function-tests)
  (test
   ((single-recursion) "SINGLE RECURSION TESTS")
   ((double-recursion) "DOUBLE RECURSION TESTS")))

(define (single-recursion)
  (define (fact n)
    (if (= n 1)
	1
	(* n (fact (- n 1)))))
  (= (fact 5) 120))

(define (double-recursion)
  (define (fib n)
    (if (or (= n 1)
	    (= n 0))
	1
	(+ (fib (- n 1)) (fib (- n 2)))))
  (= (fib 10) 89))