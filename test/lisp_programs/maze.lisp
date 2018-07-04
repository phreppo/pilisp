(setq maze1 '(
    (1) 
    (0 3)
    (3 -1)
    (1 2)
))

(defun sm1 (maze actualCell exploredCells doors)
    (cond 
        (
            (not doors)
            ; no exit
            nil
        ) (
            t ; at least one door
            (cond 
                ( (not (solveMazeRec maze (car doors) exploredCells))
                    (sm1 maze actualCell exploredCells (cdr doors))
                ) 
                ( t 
                    (solveMazeRec maze (car doors) exploredCells)
                ) 
            )
        )
    )
)


(defun solveMazeRec 
    (maze actualCell exploredCells)
        (cond 
            ((= actualCell -1)
                ; finished
                exploredCells
            ) 
            ((member actualCell exploredCells)
                ; already explored this path        
                nil)
            (t
                ; not finished
                (sm1 maze actualCell (cons actualCell exploredCells) (nth actualCell maze))
            )
        ) 
    )


(defun solveMaze 
    (maze)
    (solveMazeRec maze 0 '()))


; (solveMaze maze1)