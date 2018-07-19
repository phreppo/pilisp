; compiler cases
; (#lap "!B" (a b c) cdr)

(load "compiler.lisp")

(terpri )
(terpri )
(write "-- Compiling --")
(terpri )
(terpri )

(write 
    (plc '1))
(terpri)

(write 
    (plc 'x))
(terpri)

(write 
    (plc ''(a b c)))
(terpri)


(write 
    (plc '(car '(a b c))))
(terpri)

(write 
    (plc '(cdr '(a b c))))
(terpri)

(write 
    ( get_interpretable_code '((:LOADCONST . 1) (:CBS . CAR))))
(terpri)

(write 
    ( plc '(atom 33)))
(terpri )

(write 
    ( plc '(list 1 2 3 4 5 6 7 8 9 10 11)))
(terpri )

(terpri )
(terpri )
(write "-- Compiled --")
(terpri )
(terpri )

; ((#lambdalap (1 . 0) ("11C" cons) 100)
; ((#lambda (2 . 0) (#lap "12C" cons) 100 200)
; ((#lambdalap(4 . 0)("1230DAE" list)) 1 2 3 4)
; ((#lambdalap (1.0) ("1!C" 1 +)) 2)

; (setq ll '(#lambdalap (2.0) ("12C" cons)))
; (#lap "!!c" 100 100 ll) ; questo non va ma dovrebbe andare
; questo non va bene
; quando compilo il lap sia per lambdalap ho bisogno di una symbol table