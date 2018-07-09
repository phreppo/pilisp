(setq maze '(
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

(defun ff (x) (cond ((atom x) x ) (t (ff (car x)))) )

(defun toz (n) (cond ((eq n 0) 0) (t (toz (- n 1))) ) )


(write "Resolving a maze 20 times")
(time (dotimes (n 20 ) (solvemaze maze)))

(write "Doing ff 1000000 times")
(time (dotimes (n 1000000) (ff '(((((((((((a)))))))))))) ))

(write "Doing list 1000000 times ")
(time (dotimes (n 1000000) (list  n n n n n)))

(write "Doing additions")
(time (dotimes (n 1000000) (+  n n )))

(write "Doing to zero")
(time (dotimes (n 15000) (toz n)))
