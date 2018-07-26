(defun string-include (string1 string2)
    (cond
        ((eq (length string1) 0) nil)
        ((> (length string1) (length string2)) nil) 
        ;; ((eq string1 (subseq string2 0 (length string1))) string1) 
        ;; (t ( string-include string1 (subseq string2 1)))
    ))

;; ( string-include "ciao" "ciaociao")

(defun aa (x y)
    (cond 
        (x x)
        (y y) 
    ) 
)

((LAMBDA (X Y) (COND (Y Y) (X X))) 3 4)
((LAMBDA (X Y) (COND (X X) (Y Y))) 3 4)
((LAMBDA (X Y) (COND (X Y) (Y Y))) 3 4)
((LAMBDA (X Y) (COND (X Y) (Y Y))) 3 4)