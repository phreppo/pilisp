; compiler cases
; (#lap "!B" (a b c) cdr)

(load "./compiler/compiler.lisp")

(terpri )
(terpri )
(write "-- Compiling --")
(terpri )
(terpri )

(write 
    (cc '1))
(terpri)

(write 
    (cc 'x))
(terpri)

(write 
    (cc ''(a b c)))
(terpri)


(write 
    (cc '(car '(a b c))))
(terpri)

(write 
    (cc '(cdr '(a b c))))
(terpri)

(write 
    ( get_interpretable_code '((:LOADCONST . 1) (:CBS1 . CAR))))
(terpri)

(write 
    ( cc '(atom 33)))
(terpri )

(write 
    ( cc '(list 1 2 3 4 5 6 7 8 9 10 11)))
(terpri )

(terpri )
(terpri )
(write "-- Compiled --")
(terpri )
(terpri )

; ((#lambdalap (1 . 0) ("11C" cons) 100)
; ((#lambda (2 . 0) (#lap "12C" cons) 100 200)
; ((#lambdalap(4 . 0)("1230DAE" list)) 1 2 3 4)
; 
; (setq ll '(#lambdalap (2.0) ("12C" cons)))
; (#lap "!!c" 100 100 ll) ; questo non va ma dovrebbe andare
; questo non va bene
; quando compilo il lap sia per lambdalap ho bisogno di una symbol table