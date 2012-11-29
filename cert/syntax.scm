(define (syntax-tests)
  (test
   ((if-tests) "IF TESTS")
   ((define-tests) "DEFINE TESTS")
   ((begin-tests) "BEGIN TESTS")
   ((lambda-tests) "LAMBDA TESTS")))

(define (if-tests)
  (and
   ;; Conditional evaluation
   (if (= 1 0) (/ 1 0) #t)
   (if (= 1 1) #t (/ 1 0))))

(define (define-tests)
  (and
   (let ((env1 1))
     (define (bang n)
       (if (= n 1)
	   1
	   (* n (bang (- n 1)))))
     (= (bang 5) 120))
   (let ((env2 2))
     (define (bang n)
       (+ n 1))
     (and (not (= (bang 5) 120))
	  (= (bang 5) 6)))))

(define (begin-tests)
  #t)

(define (lambda-tests)
  #t)
