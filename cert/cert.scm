;; Closure testing
(define (make-distinct-lexpad-closure start-val)
  (lambda (inc) (set! start-val (+ start-val inc)) start-val))

(define (make-shared-lexpad-closures start-val)
  (list (lambda (inc) (set! start-val (+ start-val inc)) start-val)
	(lambda (inc) (set! start-val (- start-val inc)) start-val)))

(define (closure-distinct-lexpad-tests)
  (write-string "--- DISTINCT CLOSURE LEXPADS: BEGIN ---")
  (terpri)
  (let ((closure1 (make-distinct-lexpad-closure 1))
	(closure2 (make-distinct-lexpad-closure 1)))
    (or
     (and (is (closure1 1) 2 "closure1 returns 2")
	  (is (closure2 10) 11 "closure2 returns 11")
	  (is (closure1 1) 3 "closure1 returns 3")
	  (is (closure2 10) 21 "closure2 returns 21")
	  (begin
	    (write-string "--- DISTINCT CLOSURE LEXPADS: PASS ---")
	    (terpri)
	    #t))
     (begin
       (write-string "--- DISTINCT CLOSURE LEXPADS: FAIL ---")
       (terpri)
       #f))))

(define (closure-shared-lexpad-tests)
  (write-string "--- SHARED CLOSURE LEXPADS: BEGIN ---")
  (terpri)
  (let ((closures (make-shared-lexpad-closures 1)))
    (let ((incr (car closures))
	  (decr (car (cdr closures))))
      (or
       (and (is (incr 1) 2 "incr returns 2")
	    (is (incr 2) 4 "incr returns 4")
	    (is (decr 1) 3 "decr returns 3")
	    (is (decr 4) -1 "decr returns -1")
	    (begin
	      (write-string "--- SHARED CLOSURE LEXPADS: PASS ---")
	      (terpri)
	      #t))
       (begin
	 (write-string "--- DISTINCT CLOSURE LEXPADS: FAIL ---")
	 (terpri)
	 #f)))))

(define (closure-tests)
  (write-string "--- CLOSURE TESTS: BEGIN ---")
  (terpri)
  (let ((distinct (closure-distinct-lexpad-tests))
	(shared   (closure-shared-lexpad-tests)))
    (if (and distinct shared)
	(begin
	  (write-string "--- CLOSURE TESTS: PASS ---")
	  (terpri)
	  #t)
	(begin
	  (write-string "--- CLOSURE TESTS: FAIL ---")
	  (terpri)
	  #f))))

(define (is thing1 thing2 test-string)
  (if (or (eq? thing1 thing2)
	  (= thing1 thing2))
      (begin
	(write-string test-string)
	(write-string " - ")
	(write 'ok)
	(terpri)
	#t)
      (begin
	(write-string test-string)
	(write-string " - ")
	(write 'error)
	(write (list 'is thing1 thing2))
	(terpri)
	#f)))