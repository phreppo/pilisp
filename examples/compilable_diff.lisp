;; (time (dotimes (n 100000) (d '(/ (+ (expt x 2) 1) (cos x)))))

;; needs the compiler to be loaded

(defun first_arg (func) (car (cdr func)) )
(compile first_arg)
(defun second_arg (func) (car (cdr (cdr func))) )
(compile second_arg)
(defun fun_name (func) (car func) )
(compile fun_name)

(defun mult (first second) (cons '* (cons first (cons second NIL))) )
(compile mult)
(defun expo (first second) (cons 'expt (cons first (cons second NIL))) )
(compile expo)
(defun sum  (first second) (cons '+ (cons first (cons second NIL))) )
(compile sum)
(defun diff (first second) (cons '- (cons first (cons second NIL))) )
(compile diff)
(defun frac (first second) (cons '/ (cons first (cons second NIL))) )
(compile frac)

(defun is_ln (func) (eq (car func) 'ln))
(compile is_ln)
(defun is_exp (func) (eq (car func) 'exp))
(compile is_exp)
(defun is_expt (func) (eq (car func) 'expt))
(compile is_expt)
(defun is_sin (func) (eq (car func) 'sin))
(compile is_sin)
(defun is_cos (func) (eq (car func) 'cos))
(compile is_cos)
(defun is_tan (func) (eq (car func) 'tan))
(compile is_tan)
(defun is_sum (func) (eq (car func) '+))
(compile is_sum)
(defun is_mult (func) (eq (car func) '*))
(compile is_mult)
(defun is_div (func) (eq (car func) '/))
(compile is_div)

(defun d (func) ( cond
    ((integerp func) 0) 
    ((symbolp func) 1)
    (( is_ln func) 
      ( mult 
        ( frac 1 ( first_arg func)) 
        ( d ( first_arg func))))
    (( is_exp func)
      ( mult 
        func 
        ( d ( first_arg func))))
    (( is_expt func)
      ( mult 
        (cons 'expt (cons ( first_arg func) (- ( second_arg func ) 1))) 
        ( second_arg func)))
    (( is_sin func)
      ( mult
        (cons 'cos (cons ( first_arg func) NIL))
        ( d ( first_arg func))))
    (( is_cos func)
      ( mult
        ( mult (cons 'sin (cons ( first_arg func) NIL)) -1)
        ( d ( first_arg func))))
    (( is_tan func)
      ( mult 
        ( frac 
          1 
          ( expo (cons 'cos (cons ( first_arg func) NIL)) 2) ) 
        ( d ( first_arg func))))
    (( is_sum func)
      ( sum
        ( d ( first_arg func))
        ( d ( second_arg func))))
    (( is_mult func)
      ( sum
        ( mult ( d ( first_arg func)) ( second_arg func))
        ( mult ( first_arg func) ( d ( second_arg func)))))
    (( is_div func)
      ( frac
        ( diff 
          ( mult ( d ( first_arg func)) ( second_arg func)) 
          ( mult ( first_arg func) ( d ( second_arg func))))
        ( expo ( second_arg func) 2)))
    (t NIL)))