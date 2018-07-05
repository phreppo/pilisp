(setq maze1 (quote ((1) (0 3) (3 -1) (1 2))))

(defun sm1 (maze actualCell exploredCells doors)
  (cond
    ((not doors) nil)
    (t
      (cond
        ( (not (solveMazeRec maze (car doors) exploredCells))
          (sm1 maze actualCell exploredCells (cdr doors)))
        (t (solveMazeRec maze (car doors) exploredCells))))))

(defun solveMazeRec (maze actualCell exploredCells)
  (cond
    ((= actualCell -1) exploredCells)
    ((member actualCell exploredCells) nil)
    (t
      (let ((newExplored (cons actualCell exploredCells)) (doors (nth actualCell maze)))
        (cond
          ((not doors) nil)
          (t
            (cond
              ( (not (solveMazeRec maze (car doors) newExplored))
                (sm1 maze actualCell newExplored (cdr doors)))
              (t (solveMazeRec maze (car doors) newExplored)))))))))

(defun solveMaze (maze) (solveMazeRec maze 0 (quote ())))

;((label solveMazeRec
;    (lambda (maze actualCell exploredCells)
;      (cond
;        ((= actualCell -1) exploredCells)
;        ((member actualCell exploredCells) nil)
;        (t
;          (let ((newExplored (cons actualCell exploredCells)) (doors (nth actualCell maze)))
;            (cond
;              ((not doors) nil)
;              (t
;                (cond
;                  ( (not (solveMazeRec maze (car doors) newExplored))
;                    (sm1 maze actualCell newExplored (cdr doors)))
;                  (t (solveMazeRec maze (car doors) newExplored))))))))))
;  (quote ((1) (0 3) (3 -1) (1 2))) 0 (quote ()))
;
; solves!
