(define (even? n)
  (= (mod n 2) 0))

(define (odd? n)
  (= (mod n 2) 1))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;; (defmacro let (forms . body)
;;   (cons (list 'lambda (map car forms) body)
;;      (map cadr forms)))
