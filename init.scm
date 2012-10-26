;;; General (bootstrap) functions

(define (atom x)
  (not (list? x)))

(define (odd? n)
  (= (mod n 2) 1))

(define (even? n)
  (not (odd? n)))

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

(defmacro (dolist form . body)
  (list 'begin
	(list 'map (list 'lambda (list (car form)) (cons 'begin body))
	      (cadr form))
	#f))

(defmacro (push what where)
  (list 'set! where (list 'cons what where)))

;;; Non-bootstrapping functions

(define (write-ln . strings)
  (apply write-string strings)
  (terpri))

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

(define (cdddr lst)
  (cdr (cddr lst)))

(define (cadddr lst)
  (car (cdddr lst)))

(define (cdar lst)
  (cdr (car lst)))

(define (append . lsts)
  (let ((acc nil))
    (map (lambda (n)
	   (map (lambda (m) (push m acc))
		n))
	 lsts)
    (reverse acc)))

;;; Backquote macros

(defmacro (backquote obj)
  (bq-process obj))

(define foo '(1 2 3))

;; `(foo `(foo ,foo ,,foo) ,@foo)

;; (backquote
;;  (foo
;;   (backquote (foo (comma foo) (comma (comma foo))))
;;   (comma-splice foo)))

;; (FOO `(FOO ,FOO ,(1 2 3)) 1 2 3)

;; (append
;;   (list (quote foo))
;;   (list (backquote
;; 	 (list foo (comma (comma foo))))) ; Ignoring `(... `(... ,foo ...))
;;   foo)

;; `(foo ,foo ,@foo)

;; (append
;;   (list (quote foo))
;;   (list foo)
;;   foo)

(defmacro (backquote x)
  (bq-process x 1))

(define (bq-process x depth)
  (cons 'append
  	(let ((thing (map (lambda (n) (bq-loop n depth)) x)))
	  thing)))

(define (bq-loop x depth)
  (define (count-comma lst acc)
    (if (and (list? lst)
	     (not (null lst))
	     (eq? (car lst) 'comma))
	(count-comma (cadr lst) (+ acc 1))
	acc))
  (define (nthcadr thing times)
    (if (= times 1)
	(cadr thing)
	(nthcadr (cadr thing) (- times 1))))
  (cond ((atom x)
	 (list 'list (list 'quote x)))
	((eq? (car x) 'comma)
	 (if (= (- (count-comma x 1) 1) depth)
	     (list 'list (nthcadr x depth))
	     (list 'list (list 'quote x))))
	((eq? (car x) 'comma-splice)
	 (cadr x))
	((eq? (car x) 'backquote)
	 (list 'list
	       (list 'append (apply (lambda (n) (bq-process n (+ depth 1)))
				       (cdr x)))))
	(#t (let ((thing (map (lambda (n)
				(bq-loop n depth)) x)))
	      (list 'list (cons 'append thing))))))

;;; Bootstrap++

(defmacro (or . rest)
  (define (expander lst)
    (if (null lst)
	#f
	(let ((sym (gensym)))
	  `(let ((,sym ,(car lst))) (if ,sym ,sym ,(expander (cdr lst)))))))
  (expander rest))

(defmacro (dotimes args . body)
  `(do ((,(car args) 0 (+ ,(car args) 1)))
       ((= ,(cadr args) ,(car args)) nil)
     ,@body))

;; (define *bq-simplify* #f)

;; (defmacro (backquote x)
;;   (bq-completely-process x))

;; (define (bq-completely-process x)
;;   (let ((raw-result (bq-process x)))
;;     (bq-remove-tokens (if *bq-simplify*
;; 			  (bq-simplify raw-result)
;; 			  raw-result))))

;; (define (bq-process x)
;;   (cond ((atom x)
;; 	 (list 'bq-quote x))
;; 	((eq? (car x) 'backquote)
;; 	 (bq-process (bq-completely-process (cadr x))))
;; 	((eq? (car x) 'comma) (cadr x))
;; 	((eq? (car x) 'comma-splice)
;; 	 (write-err ",@ after `")
;; 	 (terpri))
;; 	(#t (do ((p x (cdr p))
;; 		 (q nil (cons (bracket (car p)) q)))
;; 		((atom p)
;; 		 (cons 'bq-append
;; 		       (cons (reverse q) (list (list 'bq-quote p)))))
;; 	      (cond ((eq? (car p) 'comma)
;; 		     (unless (null (cddr p)) (write-err "Malformed ,"))
;; 		     (cons 'bq-append
;; 			   (cons (reverse q) (list (cadr p)))))
;; 		    ((eq? (car p) 'comma-splice)
;; 		     (write-err "Malformed ,@")))))))

;; (define (bracket x)
;;   (cond ((atom x)
;; 	 (list 'bq-list (bq-process x)))
;; 	((eq? (car x) 'comma)
;; 	 (list 'bq-list (cadr x)))
;; 	((eq? (car x) 'comma-splice)
;; 	 (cadr x))
;; 	(#t (list 'bq-list (bq-process x)))))

;; (define (bq-remove-tokens x)
;;   (cond ((eq? x 'bq-list) 'list)
;; 	((eq? x 'bq-append) 'append)
;; 	((eq? x 'bq-nconc) 'nconc)
;; 	((eq? x 'bq-list) 'list)
;; 	((eq? x 'bq-quote) 'quote)
;; 	((atom x) x)
;; 	((eq? (car x) 'bq-clobberable)
;; 	 (bq-remove-tokens (cadr x)))
;; 	((and (eq? (car x) 'bq-list)
;; 	      (list? (cddr x))
;; 	      (null (cdr (cddr x))))
;; 	 (cons 'cons (maptree bq-remove-tokens (cdr x))))
;; 	(#t (maptree bq-remove-tokens x))))

;; (define (maptree fn x)
;;   (if (atom x)
;;       (fn x)
;;       (let ((a (fn (car x)))
;; 	    (d (maptree fn (cdr x))))
;; 	(if (and (eq? a (car x)) (eq? d (cdr x)))
;; 	    x
;; 	    (cons a d)))))
