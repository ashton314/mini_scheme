(define-macro (test . forms)
  `(and 
    ,@(map (lambda (form)
	     `(begin
		(write-string-err ,(cadr form))
		(if ,(car form)
		    (begin
		      (write-string-err " - pass")
		      (terpri-err)
		      #t)
		    (begin
		      (write-string-err " - fail")
		      (terpri-err)
		      #f)))) forms)))

;; (test
;;  ((= 1 1) "Equality")
;;  ((not (= 2 3)) "Inequality"))
