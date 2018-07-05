(set 'a "A")
(set 'b 777)
(set 'ff (lambda (x) 
    (cond 
        ((atom x) x ) 
        (t (ff (car x))))
))

(set 'we
    (lambda ()
    (cond 
        (NIL NIL)
        ((atom a) 
            (1)
        )
    ) )
)

(set 'test1 (lambda (num) 
    ( cond
        ( (eq num 0) 
            T)
        ( T 
            (test1 (- num 1)) )
    ) )
)

;(timer '(test 18000))
;(timer '(test 17000))