(load "./examples/functions.lisp")
(load "./compiler/compiler.lisp")
(set 'n 789)               
(set 'n2 2)                        
NIL 
44 
n2 
44 
2 
n

(dotimes (n 5) (toz 5))
(ff '(a))
(ff '((((a) b ) c ) d))
(ff '((a) b ))
; ((label ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) '((a)))
; ((label ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) 'a)
;
((lambda (x) (cond ((eq x 1) 1 ) ((eq x 2) 2) (t 666))) '(1))
((lambda (x) x) '(1))


((lambda (x) (lambda (y) y)) 1 )
(((lambda (x) (lambda (y) 2))  ) ) 
(((lambda (x) (lambda (y) y)) 1 ) 2)
(((lambda (x) (lambda (y) y)) 1 2 ) 3)
((((lambda (x) (lambda (y) (lambda (z) z))) 1 ) 2) 3)
( (lambda (x) (+ 1 x ) ) 1 )
(set 'ivar (lambda (x) (set 'var x)))
(set 'l '(nil (1 2) ((1)(2)) nil "ciao"))
(or nil nil nil '(1 2 3) 4)
; this leaks memory but not alone!
; (set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))
(defun id (x) x)
(let ((x 1)(y 2)) (+ x y))
;
(dotimes (n 1) (toz n))
(dotimes (n 2) (toz 3))
(dotimes (n 3) (toz 4))
(dotimes (n 3) (+ 2 2))
(dotimes (n 1) 666)
(dotimes (n 2) 666)
(dotimes (n 3) 666)
;
(let ((l '(1 2 3))(num 0)) (nth num l))
;
(setq maze1 '(
    (1) 
    (0 3)
    (3 -1)
    (1 2)
))

(setq maze2 '(
    (3 1)       ;0
    (0 2 4)     ;1
    (1 5)       ;2
    (0 4)       ;3
    (1 3)       ;4
    (2 8)       ;5
    (-1 7)      ;6
    (6 8)       ;7
    (5 7)       ;8
))

(setq maze3 '(
    (4)         ; 0
    (2)         ; 1
    (1 3)       ; 2
    (2 7)       ; 3
    (8 5 0)     ; 4
    (4 6)       ; 5
    (5 7)       ; 6
    (3 11 6)    ; 7
    (4)         ; 8
    (10 13)     ; 9
    (9 14)      ; 10
    (7 15)      ; 11
    (13 -1)     ; 12
    (9 12)      ; 13
    (10 15)     ; 14
    (11 14)     ; 15
))

(defun sm1 (maze actualCell exploredCells doors)
    (cond 
        ( (not doors)
            nil ) 
        ( t 
            (cond 
                ( (not (solveMazeRec maze (car doors) exploredCells))
                    (sm1 maze actualCell exploredCells (cdr doors)) ) 
                ( t 
                    (solveMazeRec maze (car doors) exploredCells)
                ) ) ) ) )

(defun solveMazeRec 
    (maze actualCell exploredCells)
        (cond 
            ((= actualCell -1)
                exploredCells
            ) 
            ((member actualCell exploredCells)     
                nil)
            (t
                (sm1 maze actualCell (cons actualCell exploredCells) (nth actualCell maze))
            ) ) )


(defun solveMaze 
    (maze)
    (solveMazeRec maze 0 '()) )


(solveMaze maze1)

(check "zombie")

(LASM 3 "@A@B@C@C@B@A!!$I" 1 2 LIST)
(defun mylist (x y z) (list z y x x y z  1 2))

(write "Invoking trough name")
(time (dotimes ( n 10000000) (mylist 1 2 3)))

(write "invoking through lambda")
(time (dotimes (n 10000000) ( (lambda (x y z) (list z y x x x y z 1 2 )) 1 2 3 )))

(write "invoking through compiled code")
(compile mylist)
(time (dotimes ( n 10000000) (mylist 1 2 3)))

(write "invoking direct compiled code")
(time (dotimes (n 10000000) ( (LASM 3 "@A@B@C@C@B@A!!$I" 1 2 LIST) 1 2 3 )))

(load c)
(md)
;; (compile defmacro)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile 1+)
(compile id)
;; 