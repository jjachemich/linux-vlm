(defun fibonacci (x)
  (if (<= x 2)
      1 
      (+ (fibonacci (- x 2))(fibonacci (1- x)))))
