(define (even? n)
  (= (mod n 2) 0))

(define (odd? n)
  (= (mod n 2) 1))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (length lst)
  (if (null lst)
      0
      (+ (length (cdr lst)) 1)))

(define (foreach func lst)
  (if (null lst)
      nil
      (cons (func (car lst)) (foreach func (cdr lst)))))

(define (reduce func lst)
  (if (= (length lst) 2)
      (func (car lst) (car (cdr lst)))
      (reduce func (cons (func (car lst) (car (cdr lst)))
			 (cdr (cdr lst))))))

;; (defmacro let (forms . body)
;;   (cons (list 'lambda (map car forms) body)
;;      (map cadr forms)))

