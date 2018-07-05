(defun factorial (x)
    (cond 
        ((or (not (integerp x)) (< x 0)) 
            nil)
        ((eq x 0) 
            1)
        (t 
            (* x (factorial (- x 1))))))