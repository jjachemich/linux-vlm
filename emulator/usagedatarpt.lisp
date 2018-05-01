;;; -*- Package: CL-USER; Base: 10 -*-

(declare (special  *iusedata* *itotalused*))

(defun report-on-usage-data (&optional (path "usagereport.text"))
  (with-open-file (report path :direction :output)
    (let ((sorteddata (setq  *iusedata* (cl:sort *iusedata* #'> :key #'cadr))))
      (dolist (item sorteddata)
	(destructuring-bind (name usage) item
	  (format report "~a ~d/~d = ~a%~%" 
		  name usage *itotalused* (* 100.0 (/ (* 1.0 usage) *itotalused*))))))))
