; these examples come from LISP 1.5 book, page 4
(a b c)
((a b) c)
(a b (c d))
(a)
((a))
(a (b .c))
; the next are the dot version of the first group
(a . (b . (c . NIL)))
((a.(b.NIL)).(c.NIL))
(a.(b.((c.(d.NIL)).NIL)))
(a.NIL)
((a.NIL).NIL)
(a.((b.c).NIL))
