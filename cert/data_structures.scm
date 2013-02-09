(define (data-structure-tests)
  (test
   ((eq?-tests)  "EQ? TESTS")
   ((cons-tests) "CONS TESTS")
   ((destructive-tests) "DESTRUCTIVE TESTS")))

(define (eq?-tests)
  (and
   (eq? 'foo 'foo)
   (let ((var1 'forty-two)
   	 (var2 'forty-two)
   	 (var3 'forty-three))
     (and
      (eq? 'forty-two var1)
      (begin
   	(set! var2 var1)
   	(eq? var1 var2))
      (not (eq? var1 var3))
      (not (eq? var3 'forty-two))))
   (let ((var1 '(1 2 3)))
     (let ((var2 var1))
       (and
   	(eq? var1 var2)
   	(not (eq? var1 '(1 2 3))))))))
      

(define (cons-tests)
  (let ((one (cons 'a 'b))
	(two (cons 'a (cons 'b nil))))
    (and
     (eq? (car one) 'a)
     (eq? (cdr one) 'b)
     (eq? (car one) (car two))
     (eq? (car (cdr two)) (cdr one)))))

(define (destructive-tests)
  (let ((cons-cell '(1 2 3)))
    (and
     (= (cadr cons-cell) 2)
     (begin
       (set-car! (cdr cons-cell) 4)
       (= (cadr cons-cell) 4))
     (not (= (cadr cons-cell) 2))
     (= (cadr cons-cell) 4)
     (= (caddr cons-cell) 3)
     (begin
       (set-cdr! cons-cell '(5 6))
       (and (= (cadr cons-cell) 5)
	    (= (caddr cons-cell) 6)))
     (= (cadr cons-cell) 5)
     (= (caddr cons-cell) 6))))
