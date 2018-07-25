(setq maze1 '(
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

(defun else () t)

(defun sm1 (maze actualCell exploredCells doors)
    (cond 
        ((not doors)
            nil ) 
        (t 
            (cond 
                ((not (solveMazeRec maze (car doors) exploredCells))
                    (sm1 maze actualCell exploredCells (cdr doors))) 
                (( else) 
                    (solveMazeRec maze (car doors) exploredCells))))))

(defun solveMazeRec (maze actualCell exploredCells)
    (cond 
        ((eq actualCell -1)
            exploredCells) 
        ((member actualCell exploredCells)     
            nil)
        (( else)
            (sm1 maze actualCell (cons actualCell exploredCells) (nth actualCell maze)))))


(defun solveMaze (maze)
    (solveMazeRec maze 0 '()))
