(load "./examples/functions.lisp")
(set 'n 1)               
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
((label ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) '((a)))
((label ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) 'a)

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
(set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))
(defun id (x) x)
(let ((x 1)(y 2)) (+ x y))

(dotimes (n 1) (toz n))
(dotimes (n 2) (toz n))
(dotimes (n 3) (toz n))
(dotimes (n 3) (+ 1 n))
(dotimes (n 1) 666)
(dotimes (n 2) 666)
(dotimes (n 3) 666)

(let ((l '(1 2 3))(num 0)) (nth num l))

(setq m '(
    (1) 
    (0 3)
    (3 -1)
    (1 2) ))


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
                exploredCells) 
            ((member actualCell exploredCells)     
                nil)
            (t
                (let
                    ((newExplored (cons actualCell exploredCells))
                    (doors (nth actualCell maze)))
                    
                            (cond 
        ( (not doors)
            nil ) 
        ( t 
            (cond 
                ( (not (solveMazeRec maze (car doors) newExplored))
                    (sm1 maze actualCell newExplored (cdr doors)) ) 
                ( t 
                    (solveMazeRec maze (car doors) newExplored)
                ) ) ) ) ) ) 
                ))

(defun sm
    (maze)
    (solveMazeRec maze 0 '()) )

; (solveMaze maze1)