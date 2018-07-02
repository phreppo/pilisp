(set 'n 1)               
(set 'n2 2)                        
NIL 
44 
n2 
44 
2 
n
; to zero function
(set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))
(set 'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x))))))

((lambda (x) (lambda (y) y)) 1 )
(((lambda (x) (lambda (y) 2))  ) ) 
(((lambda (x) (lambda (y) y)) 1 ) 2)
(((lambda (x) (lambda (y) y)) 1 2 ) 3)
((((lambda (x) (lambda (y) (lambda (z) z))) 1 ) 2) 3)
( (lambda (x) (+ 1 x ) ) 1 )
(set 'ivar (lambda (x) (set 'var x)))
(set 'f (lambda (x) (lambda (y) (+ 1 2))))
((f))
(set 'l '(nil (1 2) ((1)(2)) nil "ciao"))
(or nil nil nil '(1 2 3) 4)
(set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))

(dotimes (n 1) (toz n))
(dotimes (n 2) (toz n))
(dotimes (n 3) (toz n))
(dotimes (n 1) 666)
(dotimes (n 2) 666)
(dotimes (n 3) 666)

;(time '(dotimes (n 10) (toz n)))
;(cond ( (not nil) 1 ) (t 2) (t 1))
;(cond (nil nil) ((not nil) 1 ) (t 2) (t 1))

;; (d '(cos x) 'x)
;; (* (cos x) 1)
