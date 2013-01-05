(define (cps sexpr) (cpc sexpr nil 'cont))

(define (cpc sexpr env cont)
  (if (atom sexpr)
      (cpc-atom sexpr env cont)
      (or
       (case (car sexpr)
	 ('quote (if cont `(,cont ,sexpr) sexpr))
	 ('lambda (cpc-lambda sexpr env cont))
	 ('if (cpc-if sexpr env cont))
	 ('catch (cpc-catch sexpr env cont))
	 ('labels (cpc-labels sexpr env cont)))
       (cond ((and (atom (car sexpr))
		   (macro? (car sexpr)))
	      (cpc (macroexpand sexpr) env cont)) ; Macro expand
	     (t (cpc-form sexpr env cont))))))

(define (cpc-atom sexpr env cont)
  ((lambda (at)
     (if (or
	  (eq? cont #f)
	  (null? cont)) at `(,cont ,at)))
   (cond ((number? sexpr) sexpr)
	 ((member sexpr env) sexpr)
	 (#t (implode (cons '@ (explode sexpr)))))))

(define (cpc-lambda sexpr env cont)
  ((lambda (cn)
     ((lambda (lx) (if cont `(,cont ,lx) lx))
      `(lambda (,@(cadr sexpr) ,cn)
	 ,(cpc (caddr sexpr)
	       (append (cadr sexpr) (cons cn env))
	       cn))))
   (gensym)))

(define (cpc-if sexpr env cont)
  ((lambda (kn)
     `((lambda (,kn)
	 ,(cpc (cadr sexpr)
	       env
	       ((lambda (pn)
		  `(lambda (,pn)
		     (if ,pn
			 ,(cpc (caddr sexpr)
			       env
			       kn)
			 ,(cpc (cadddr sexpr)
			       env
			       kn))))
		(gensym))))
       ,cont))
   (gensym)))

(define (cpc-catch sexpr env cont)
  ((lambda (en)
     `((lambda (,en)
	 ((lambda (,(cadr sexpr))
	    ,(cpc (caddr sexpr)
		  (cons (cadr sexpr) env)
		  en))
	  (lambda (v c) (,en v))))
       ,cont))
   (gensym)))

(define (cpc-labels sexpr env cont)
  (do ((x (cadr sexpr) (cdr x))
       (y env (cons (caar x) y)))
      ((null? x)
       (do ((w (cadr sexpr) (cdr w))
	    (z nil (cons (list (caar w)
			       (cpc (cadar w) y nil))
			 z)))
	   ((null? w)
	    `(labels ,(reverse z)
		     ,(cpc (caddr sexpr) y cont)))))))

(define (cpc-form sexpr env cont)
  (terpri-err)
  (labels ((loop1
	    (lambda (x y z)
	      (if (null? x)
		  (do ((f (reverse (cons cont y))
			  (if (null? (car z))
			      f
			      (cpc (car z)
				   env
				   `(lambda (,(car y)) ,f))))
		       (y y (cdr y))
		       (z z (cdr z)))
		      ((null? z) f))
		  (cond ((or (null? (car x))
			     (atom (car x)))
			 (loop1 (cdr x)
				(cons (cpc (car x) env nil) y)
				(cons nil z)))
			((eq? (caar x) 'quote)
			 (loop1 (cdr x)
				(cons (car x) y)
				(cons nil z)))
			((eq? (caar x) 'lambda)
			 (loop1 (cdr x)
				(cons (cpc (car x) env nil) y)
				(cons nil z)))
			(#t (loop1 (cdr x)
				   (cons (gensym) y)
				   (cons (car x) z))))))))
	  (loop1 sexpr nil nil)))

(define-macro (mcp func)		; Make Contiuation Passing
  `(define ,(implode (cons '@ (explode func)))
     (lambda (arg1 &rest args)
       (set! args (cons arg1 args))
       (let ((cont (car (reverse args))))
	 (cont (apply ,func (reverse (cdr (reverse args)))))))))

(define-macro (mcps . funcs)
  `(begin ,@(map (lambda (func) `(mcp ,func)) funcs)))

(mcps > < = + - * / mod eq? quit car cdr last rplaca rplacd cons list
      not number? macro? list? apply null? int read clear time load write
      write-string write-string-err error write-err sleep terpri terpri-err
      fle macroexpand implode explode gensym env_symbols verbose dumper trace)