(set 'maze1 '(
    (1) 
    (0 3)
    (3 -1)
    (1 2)
))

(set 'sm1 
    (lambda (maze actualCell exploredCells doors)
    (cond 
        (
            (not doors)
            ; no exit
            NIL
        ) (
            T ; at least one door
            (cond 
                ( (not (solveMazeRec maze (car doors) exploredCells))
                    ; => the result is not a valid path 
                    (sm1 maze actualCell exploredCells (cdr doors))
                ) 
                ( t ; => the result is a valid path
                    (solveMazeRec maze (car doors) exploredCells)
                ) 
            )
        )
    )
    )
)

(set 'solveMazeRec 
    (lambda (maze actualCell exploredCells)
        (cond 
            ((= actualCell -1)
                ; finished
                exploredCells
            ) 
            ((member actualCell exploredCells)
                ; already explored this path        
                NIL)
            (t
                ; not finished
                (sm1 maze actualCell (cons actualCell exploredCells) (nth actualCell maze))
            )
        ) 
    )
)

(set 'solveMaze 
    (lambda (maze)
    (solveMazeRec 0 '() maze) )
)

(solveMaze maze1)