(define (even? n)
  (= (mod n 2) 0))

(define (odd? n)
  (= (mod n 2) 1))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (cadr lst)
  (car (cdr lst)))

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

(define (find-if func lst)
  (if (null lst)
      #f
      (if (func (car lst))
	  (car lst)
	  (find-if func (cdr lst)))))

(define (map func . lsts)
  (if (find-if null lsts)
      nil
      (cons (apply func (foreach car lsts))
	    (apply map (cons func (foreach cdr lsts))))))

(defmacro (incf thing)
  (list 'set! thing (list '+ thing 1)))

(define foo 42)
(define zoop 13)

(defmacro (let forms . body)
  (cons (list 'lambda (map car forms) (cons 'begin body))
	(map cadr forms)))

