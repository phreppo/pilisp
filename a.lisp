(set 'n 1)               (set 'n2 2)                        NIL 44 n2 44 2 n
; to zero function
(set 'toz 
    (lambda (x) 
        ( cond 
            ( (eq x 0)
                0 )
            ( T 
                (toz (- x 1))
            )
        )
    )
)

(set 
    'ff
    (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))
)