
;; continuation-passing functions

(define-macro (mcp func)		; Make Contiuation Passing
  `(define ,(implode (cons '@ (explode func)))
     (lambda (arg1 . args)
       (set! args (cons arg1 args))
       (let ((cont (car (reverse args))))
	 (cont (apply ,func (reverse (cdr (reverse args)))))))))

(define-macro (mcps . funcs)
  `(begin ,@(map (lambda (func) `(mcp ,func)) funcs)))

(mcps > < = + - * / mod) ; eq? quit car cdr last rplaca rplacd cons list)
      ;; not number? macro? list? apply null? int read clear time load write
      ;; write-string write-string-err error write-err sleep terpri terpri-err
      ;; fle macroexpand implode explode gensym env_symbols verbose trace)
