;;; Loop test functions

(define (loopy-factorial n)
  (let ((acc 1))
    (dotimes (i (- n 1))
	     (set! acc (* acc (+ i 1))))
    acc))

(define (loop-tests)
  (let ((10-loop-bang (loopy-factorial 10))
	(10-recr-bang (factorial 10)))
    (= 10-loop-bang 10-recr-bang)))