(set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))
(set 'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x))))))
(set 'iso (lambda (x) (cond ((eq x 1) 1 ) ((eq x 2) 2) (t 666))))
(set 'plo (lambda (n) (+ 1 n)))
; (set 'defun (macro (name param body) ???))
(set 'mysetq (macro (name val) (set name val)))
(set 'mydefun 
    (macro (name param body) 
        (list 'set (list 'quote name) (list 'lambda param body))) )

((macro (name val) (set name val)) ciao 1) ; funzia