;; (time (dotimes (n 100000) (d '(/ (+ (expt x 2) 1) (cos x)))))

;; needs the compiler to be loaded

(defun first_arg (func) (car (cdr func)) )
(defun second_arg (func) (car (cdr (cdr func))) )
(defun fun_name (func) (car func) )

(defun mult (first second) (cons '* (cons first (cons second NIL))) )
(defun expo (first second) (cons 'expt (cons first (cons second NIL))) )
(defun sum  (first second) (cons '+ (cons first (cons second NIL))) )
(defun diff (first second) (cons '- (cons first (cons second NIL))) )
(defun frac (first second) (cons '/ (cons first (cons second NIL))) )

(defun is_ln (func) (eq (car func) 'ln))
(defun is_exp (func) (eq (car func) 'exp))
(defun is_expt (func) (eq (car func) 'expt))
(defun is_sin (func) (eq (car func) 'sin))
(defun is_cos (func) (eq (car func) 'cos))
(defun is_tan (func) (eq (car func) 'tan))
(defun is_sum (func) (eq (car func) '+))
(defun is_mult (func) (eq (car func) '*))
(defun is_div (func) (eq (car func) '/))

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

t

(compile first_arg)
(compile second_arg)
(compile fun_name)
(compile mult)
(compile expo)
(compile sum)
(compile diff)
(compile frac)
(compile is_ln)
(compile is_exp)
(compile is_expt)
(compile is_sin)
(compile is_cos)
(compile is_tan)
(compile is_sum)
(compile is_mult)
(compile is_div)

t