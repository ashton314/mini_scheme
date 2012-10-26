(defmacro (timer times . body)
  ;; Returns: analyzation and single execution time,
  ;;          total time,
  ;;          net time
  ;;          average time per iteration
  (let ((start-time (gensym))
	(first-time (gensym))
	(last-time (gensym))
	(total-time (gensym))
	(itr (gensym)))
    `(let ((,start-time (time))
	   (,first-time (begin ,@body (time)))
	   (,last-time 0))
       (dotimes (,itr ,times)
		,@body)
       (set! ,last-time (time))
       (let ((,total-time (- ,last-time ,start-time)))
	 (list (- ,first-time ,start-time)
	       ,total-time
	       (- ,total-time (- ,first-time ,start-time))
	       (/ (- ,total-time (- ,first-time ,start-time))
		  ,times))))))

(defmacro (time-trial itr-times . body)
  (let ((times (gensym)))
    `(let ((,times (timer ,itr-times ,@body)))
       (write-ln "ANALYZATION AND ONE EXECUTION: " (car ,times))
       (write-ln "TOTAL TIME ELAPSED:            " (cadr ,times))
       (write-ln "NET TIME:                      " (caddr ,times))
       (write-ln "AVERAGE ITERATION TIME:        " (cadddr ,times)))))