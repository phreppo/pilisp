(set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))
(set 'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x))))))
(set 'iso (lambda (x) (cond ((eq x 1) 1 ) ((eq x 2) 2) (t 666))))
(set 'plo (lambda (n) (+ 1 n)))