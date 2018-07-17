;; ==================== Instructions Generator ====================

; Instructions:
;      :loadconst
;      :loadsymbol
;      :argsnum
;      :cbs[0-3] -> call builtin stack with 0-3 params
;      :cbsn -> call builtin stack with n > 3 params

; why not directly compile? because (cc '1) would give back 
; (:loadconst 1) instead of ((:loadconst 1)) 
(defun first_compile (expr)
    (cond 
        ((atom expr) 
            (list ( compile_atom expr)))
        
        (( is_quoted_expression expr)
            (list ( compile_quote expr)))
            
        (( else)
            ( _compile expr))))

(defun _compile (expr)
    (cond
        ; atom -> just load it
        ((atom expr)
            ( compile_atom expr))
        
        ; quote -> just load it
        (( is_quoted_expression expr)
            ( compile_quote expr))

        ; atom function
        ((atom (car expr))
            ( compile_atom_function expr)) 

        ;; (()) 
    ))

(defun is_quoted_expression (expr)
    (and (atom (car expr)) (eq 'quote (car expr))))

;; ==================== Atom or Quote Compiling ====================

(defun compile_atom (x)
    (cond
        ((symbolp x)    
            (cons :loadsymbol x)) 
        (( else)              
            (cons :loadconst x))))

(defun compile_quote (quote_expression)
    (cons :loadconst ( cons_cell quote_expression)))

(defun cons_cell (quote_expression)
     (car (cdr quote_expression)))

;; ==================== Atom function Compiling ====================

(defun compile_atom_function (expr)
    ( compile_atom_function_name_args (car expr) (cdr expr)))

(defun compile_atom_function_name_args (fun args)
    (cond 
        (( is_builtin_stack fun) 
            ( compile_builtin_stack fun args))
        ((not ( is_builtin_stack fun)) 
            nil)))

(defun is_builtin_stack (fun)
    (cond 
        ((eq fun 'car) t)
        ((eq fun 'cdr) t)
        ((eq fun 'cdr) t)
        ((eq fun 'cons) t)
        ((eq fun 'atom) t)
        ((eq fun 'eq) t)
        ((eq fun 'list) t)
        (t nil)))

(defun compile_builtin_stack (fun args_list)
    ( compile_args_and_append_builtin_stack fun args_list ( args_number args_list)))

; why keep passing fun? -> no list surgery, but when found the botton
; naturally append the function apply
(defun compile_args_and_append_builtin_stack (fun args_list initial_args_number)
    (cond 
        ((null args_list) 
            ( create_builtin_stack_trailer fun initial_args_number))
        (( else)  
            (cons 
            ( compile_first_arg args_list)
            ( compile_remaining_list_and_append_builtin_stack fun args_list initial_args_number)))))

(defun compile_first_arg (args_list)
    ( _compile (car args_list)))

(defun compile_remaining_list_and_append_builtin_stack (fun args_list initial_args_number)
    ( compile_args_and_append_builtin_stack fun (cdr args_list) initial_args_number))

(defun create_builtin_stack_trailer (fun initial_args_number)
    (cond 
    
    (( is_builtin_args_number initial_args_number)
        ( create_builtin_stack_trailer_builtin_args fun initial_args_number))

    (( is_not_builtin_args_number initial_args_number)
        ( create_builtin_stack_trailer_not_builtin_args fun initial_args_number))))

(defun create_builtin_stack_trailer_builtin_args (fun args_number)
    (list (cons 
        ( get_builtin_stack_instruction args_number)
        fun)))

; -> (:cbs0 . [FUN_NAME]) (:argsnum . [ARGS_NUMBER])
(defun create_builtin_stack_trailer_not_builtin_args (fun args_number)
    (list 
        (cons 
            ( get_builtin_stack_instruction args_number)
            fun)
        ( get_params_trailer args_number)))

(defun get_params_trailer (args_number)
    (cons :argsnum args_number))

(defun get_builtin_stack_instruction (initial_args_number)
    (cond 
        ((eq initial_args_number 0) :cbs0)
        ((eq initial_args_number 1) :cbs1)
        ((eq initial_args_number 2) :cbs2)
        ((eq initial_args_number 3) :cbs3)
        (( else) :cbsn))
)

(defun is_builtin_args_number (args_number)
    (<= args_number 3))

(defun is_not_builtin_args_number (args_number)
    (> args_number 3))


;; ==================== Utility ====================

(defun args_number (args_list) 
    (length args_list))

(defun else () t)