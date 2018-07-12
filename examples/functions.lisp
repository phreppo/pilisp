;(set 'mysetq (macro (name val) (set name val)))
;(set 'mydefun (macro (name param body) (list 'set (list 'quote name) (list 'lambda param body))))
;(set 'defun (macro (name param body) (list 'set (list 'quote name) (list 'lambda param body))))

(set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1))))))
(defun toz (n) (cond ((eq n 0) 0) (t (toz (- n 1))) ) )

(set 'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x))))))
(defun ff (x) (cond ((atom x) x ) (t (ff (car x)))) )

(set 'iso (lambda (x) (cond ((eq x 1) 1 ) ((eq x 2) 2) (t 666))))
(set 'plo (lambda (n) (+ 1 n)))

(setq maze '( (4) (2) (1 3) (2 7) (8 5 0) (4 6) (5 7) (3 11 6) (4) (10 13) (9 14) (7 15) (13 -1) (9 12) (10 15) (11 14) ))

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

(defun first_arg (func) (car (cdr func)) )
(defun second_arg (func) (car (cdr (cdr func))) )
(defun fun_name (func) (car func) )

(defun mult (first second) (cons '* (cons first (cons second NIL))) )
(defun expo (first second) (cons 'expt (cons first (cons second NIL))) )
(defun sum  (first second) (cons '+ (cons first (cons second NIL))) )
(defun diff (first second) (cons '- (cons first (cons second NIL))) )
(defun frac (first second) (cons '/ (cons first (cons second NIL))) )

(defun d (func) ( cond
    ((integerp func) 0) 
    ((symbolp func) 1)
    ((eq (fun_name func) 'ln) 
      (mult 
        (frac 1 (first_arg func)) 
        (d (first_arg func))))
    ((eq (fun_name func) 'exp)
      (mult 
        func 
        (d (first_arg func))))
    ((eq (car func) 'expt)
      (mult 
        (cons 'expt (cons (first_arg func) (- (second_arg func ) 1) ) ) 
        (second_arg func)))
    ((eq (fun_name func) 'sin)
      (mult
        (cons 'cos (cons (first_arg func) NIL))
        (d (first_arg func))))
    ((eq (fun_name func) 'cos)
      (mult
        (mult (cons 'sin (cons (first_arg func) NIL)) -1)
        (d (first_arg func))))
    ((eq (fun_name func) 'tan)
      (mult 
        (frac 
          1 
          (expo (cons 'cos (cons (first_arg func) NIL)) 2) ) 
        (d (first_arg func))))
    ((eq (fun_name func) '+)
      (sum
        (d (first_arg func))
        (d (second_arg func))))
    ((eq (fun_name func) '*)
      (sum
        (mult (d (first_arg func)) (second_arg func))
        (mult (first_arg func) (d (second_arg func)))))
    ((eq (fun_name func) '/)
      (frac
        (diff 
          (mult (d (first_arg func)) (second_arg func)) 
          (mult (first_arg func) (d (second_arg func))))
        (expo (second_arg func) 2)))
    (t 
      NIL)))

(defun string-include (string1 string2)
  (cond
   ((eq (length string1) 0) nil)
   ((> (length string1) (length string2)) nil) 
   ((eq string1 (subseq string2 0 (length string1))) string1) 
   (t (string-include string1 (subseq string2 1)))))
  
(defun check
  (word)
  (cond
    ((string-include "cie" word) NIL)
    ((and (string-include "ei" word)
         (not (string-include "cei" word))) NIL)
    (t t)))

(defun ff (x) (cond ((atom x) x ) (t (ff (car x)))) )

(defun toz (n) (cond ((eq n 0) 0) (t (toz (- n 1))) ) )

(defun mymult (x y) (* x y))

(defun second (list) (car (cdr list)))