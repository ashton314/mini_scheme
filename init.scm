;;; General (bootstrap) functions

(define (atom x)
  (not (list? x)))

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

(define (reverse lst)
  (define (rev lst acc)
    (if (null lst)
	acc
	(rev (cdr lst) (cons (car lst) acc))))
  (rev lst nil))

;;; Macros

(defmacro (cond . forms)
  (if (null forms)
      #f
      (list 'if (car (car forms))
	   (cons 'begin (cdr (car forms)))
	   (cons 'cond (cdr forms)))))

(defmacro (incf thing)
  (list 'set! thing (list '+ thing 1)))

(defmacro (let forms . body)
  (cons (list 'lambda (map car forms) (cons 'begin body))
	(map cadr forms)))

(defmacro (when test . body)
  (list 'if test (cons 'begin body)))

(defmacro (unless test . body)
  (list 'if (list 'not test) (cons 'begin body)))

(defmacro (and . rest)
  (define (expander lst)
    (if (null lst)
	#t
	(list 'if (car lst) (expander (cdr lst)))))
  (expander rest))

;;; Non-bootstrapping functions

(define (remove-if func lst)
  (let ((acc nil))
    (map (lambda (n) (if (not (func n)) (set! acc (cons n acc)))) lst)
    (reverse acc)))

(defmacro (do forms condition . body)	; I needed that remove-if function
  (list 'let (map (lambda (form) (list (car form) (cadr form))) forms)
	(list 'while (list 'not (car condition))
	      (cons 'begin (remove-if not
				      (map (lambda (form) (if (cadr (cdr form))
							      (list 'set! (car form) (cadr (cdr form)))))
					   forms)))
	      (cons 'begin body))
	(if (cadr condition)
	    (cadr condition)
	    #f)))

(define (cddr lst)
  (cdr (cdr lst)))

(define (caddr lst)
  (car (cddr lst)))

(define (cdar lst)
  (cdr (car lst)))

;;; Backquote macros

(define *bq-simplify* #f)

(defmacro (backquote x)
  (bq-completely-process x))

(define (bq-completely-process x)
  (let ((raw-result (bq-process x)))
    (bq-remove-tokens (if *bq-simplify*
			  (bq-simplify raw-result)
			  raw-result))))

(define (bq-process x)
  (write-err "To process: " x)
  (terpri)
  (cond ((atom x)
	 (list 'bq-quote x))
	((eq? (car x) 'backquote)
	 (bq-process (bq-completely-process (cadr x))))
	((eq? (car x) 'comma) (cadr x))
	((eq? (car x) 'comma-splice)
	 (write-err ",@ after `")
	 (terpri))
	(#t (do ((p x (cdr p))
		 (q '() (cons (bracket (car p)) q)))
		((atom p)
		 (cons 'bq-append
		       (cons (reverse q) (list (list 'bq-quote p)))))
	      (cond ((eq? (car p) 'comma)
		     (unless (null (cddr p)) (write-err "Malformed ,"))
		     (cons 'bq-append
			   (cons (reverse q) (list (cadr p)))))
		    ((eq? (car p) 'comma-splice)
		     (write-err "Malformed ,@")))))))

(define (bracket x)
  (cond ((atom x)
	 (list 'bq-list (bq-process x)))
	((eq? (car x) 'comma)
	 (list 'bq-list (cadr x)))
	((eq? (car x) 'comma-splice)
	 (cadr x))
	(#t (list 'bq-list (bq-process x)))))

(define (bq-remove-tokens x)
  (cond ((eq? x 'bq-list) 'list)
	((eq? x 'bq-append) 'append)
	((eq? x 'bq-nconc) 'nconc)
	((eq? x 'bq-list) 'list)
	((eq? x 'bq-quote) 'quote)
	((atom x) x)
	((eq? (car x) ('bq-clobberable))
	 (bq-remove-tokens (cadr x)))
	((and (eq? (car x) 'bq-list)
	      (list? (cddr x))
	      (null (cdddr x)))
	 (cons 'cons (maptree bq-remove-tokens (cdr x))))
	(#t (maptree bq-remove-tokens x))))

(define (maptree fn x)
  (if (atom x)
      (fn x)
      (let ((a (fn (car x)))
	    (d (maptree fn (cdr x))))
	(if (and (eq? a (car x)) (eq? d (cdr x)))
	    x
	    (cons a d)))))