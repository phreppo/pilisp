;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                               ;;
;; 88""Yb 88 88     88 .dP"Y8 88""Yb      dP""b8  dP"Yb  8b    d8 88""Yb 88 88     888888 88""Yb ;;
;; 88__dP 88 88     88 `Ybo." 88__dP     dP   `" dP   Yb 88b  d88 88__dP 88 88     88__   88__dP ;;
;; 88"""  88 88  .o 88 o.`Y8b 88"""      Yb      Yb   dP 88YbdP88 88"""  88 88  .o 88""   88"Yb  ;;
;; 88     88 88ood8 88 8bodP' 88          YboodP  YbodP  88 YY 88 88     88 88ood8 888888 88  Yb ;;
;;                                                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; (plc '[EXPRESSION]) -> (asm [ASM_STRING] {ARGS_LIST})
(defun plc (not_evaluated_expression)
    ( get_interpretable_code 
        ( _compile not_evaluated_expression)))

;; ****************************************************************
;; *=================== Instructions Generator ===================*
;; ****************************************************************


; Instructions:
;      :loadconst
;      :loadsymbol
;      :argsnum
;      :cbs[0-3] -> call builtin stack with 0-3 params
;      :cbsn -> call builtin stack with n > 3 params

(setq builtin_stack_lambdas 
    '( car cdr cons atom eq list))

; why not directly compile? because (plc '1) would give back 
; (:loadconst 1) instead of ((:loadconst 1)) 
(defun _compile (expr)
    (cond 
        ((atom expr) 
            (list ( compile_atom expr)))
        
        (( is_quoted_expression expr)
            (list ( compile_quote expr)))
        
        ((atom (car expr))
            ( compile_atom_function expr)) 

        (( else )
            "_ERROR:UNRECOGNIZED_FUNCTION_")))

;; ==================== Atom or Quote Compiling ====================

(defun compile_atom (x)
    (cond
        (( null x)
            (cons :loadconst x))
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
        (( is_lambda fun)
            ( compile_lambda fun args))
        (( else) 
            nil)))

(defun is_builtin_stack (fun)
    (member fun builtin_stack_lambdas))

(defun is_lambda (fun)
    (eq fun 'lambda))

;; Builtin stack compiling

(defun compile_builtin_stack (fun args_list)
    ( compile_args_and_append_builtin_stack fun args_list ( count_args args_list)))

; why keep passing fun? -> no list surgery, but when found the botton
; naturally append the function apply
(defun compile_args_and_append_builtin_stack (fun args_list initial_args_number)
    (cond 
        ((null args_list) 
            ( create_builtin_stack_trailer fun initial_args_number))
        (( else)  
            (append 
            ( compile_first_arg args_list)
            ( compile_remaining_list_and_append_builtin_stack fun args_list initial_args_number)))))

(defun compile_first_arg (args_list)
    ( _compile (car args_list)))

(defun compile_remaining_list_and_append_builtin_stack (fun args_list initial_args_number)
    ( compile_args_and_append_builtin_stack fun ( next args_list) initial_args_number))

(defun create_builtin_stack_trailer (fun initial_args_number)
    (list 
        (cons :cbs fun)
        ( get_params_trailer initial_args_number)))

(defun get_params_trailer (args_number)
    (cons :argsnum args_number))

;; TODO: Compile Lambda
(defun compile_lambda (fun args)
    (let (
        (lambda_args  ( extract_lambda_args args))
        (lambda_body  ( extract_lambda_body args))
        (symbol_table ( build_symbol_table ( extract_lambda_args args))))
    (list ; maybe cons here
        ( build_lambda_args_number_instruction lambda_args)
        ( build_lambda_body_instruction_list lambda_body symbol_table)))
)

(defun build_symbol_table (lambda_args)
    ( build_symbol_table_with_position lambda_args 0))

; we need the position to set that number in the pair (x . 0)
(defun build_symbol_table_with_position (lambda_args actual_position)
    (cond 
        ((null lambda_args) NIL)
        (( else) ( build_one_symbol_and_the_rest_of_the_list lambda_args actual_position))))

(defun build_one_symbol_and_the_rest_of_the_list (lambda_args actual_position)
    (cons 
        ( build_one_symbol lambda_args actual_position)
        ( build_symbol_table_with_position ( next lambda_args) (1+ actual_position))))

(defun build_one_symbol (lambda_args actual_position)
    (cons (car lambda_args) actual_position))

(defun build_lambda_args_number_instruction (lambda_args)
    (cons :lambdanargs ( count_args lambda_args)))

; TODO HERE
(defun build_lambda_body_instruction_list (lambda_body symbol_table) 
    (cons :HEYHEY :WOWO )
)

(defun compile_with_symbol_table (lambda_body symbol_table)
    
)


; @param lambda cons -> ((x y z) (+ x y z))
(defun extract_lambda_args (lambda_cons)
    (car lambda_cons))

(defun extract_lambda_body (lambda_cons)
    (car (cdr lambda_cons)))



;; *****************************************************************
;; *=================== Machine Code Generation ===================*
;; *****************************************************************

; ((:[INSTRUCTION] . [PARAM]) {(:[INSTRUCTION] . [PARAM])} ) 
;           -> (ASM "{MACHINE_CODE_OPERATIONS}" {PARAMETERS})
(defun get_interpretable_code (compiled_expression)
    (cons 'asm 
        (cons 
        ( extract_machine_code_string compiled_expression ) 
        ( extract_args compiled_expression))))

(defun extract_instruction_code (compiled_expression)
    (car (car compiled_expression)))

(defun extract_arg (compiled_expression)
    (cdr (car compiled_expression)))

;; ==================== Args append ====================

(defun extract_args (compiled_expression)
    (cond
        ((null compiled_expression) nil)
        (( else) ( build_one_arg_and_extract_next compiled_expression))))

(defun build_one_arg_and_extract_next (compiled_expression)
    (cond
        (( must_ignore_arg compiled_expression)
            ( extract_args (cdr compiled_expression)))
        (( else)
            (cons 
            ( extract_arg compiled_expression) 
            ( extract_args (cdr compiled_expression))))))

; case: (list 1 2 3 4) -> compilation ->  
;   ((:LOADCONST . 1) (:LOADCONST . 2) (:LOADCONST . 3) 
;   (:LOADCONST . 4) (:CBS0 . LIST) (:ARGSNUM . 4))
; -> must not append the last 4 to the list
(defun must_ignore_arg (compiled_expression)
    (eq :argsnum ( extract_instruction_code compiled_expression)))

;; ==================== Machine code string generation ====================

(defun extract_machine_code_string (compiled_expression)
    (cond 
        ((null compiled_expression) "")
        (( else) ( build_remaining_machine_code_string_char compiled_expression))))

(defun build_remaining_machine_code_string_char (compiled_expression)
    (concatenate 'string 
        ( get_instruction_code compiled_expression) 
        ( extract_machine_code_string ( next compiled_expression))))

(defun get_instruction_code (compiled_expression)
    ( translate_instruction_code 
        ( extract_instruction_code compiled_expression)
        ( extract_arg compiled_expression)))

; :[keyword] -> "[MACHINE_CODE]"
(defun translate_instruction_code (code arg)
    (cond 
        ((eq code :loadconst) "!")
        ((eq code :loadsymbol) "?")
        ((eq code :cbs) "$")
        ((eq code :argsnum) ( get_instruction_code_for_n_args arg))
        (( else) "__ERROR:UNKNOWN_INSTRUCTION_CODE__")))

(defun get_instruction_code_for_n_args (args_number)
    (cond
        ((eq args_number 0) "A")
        ((eq args_number 1) "B")
        ((eq args_number 2) "C")
        ((eq args_number 3) "D")
        ((eq args_number 4) "E")
        ((eq args_number 5) "F")
        ((eq args_number 6) "G")
        ((eq args_number 7) "H")
        ((eq args_number 8) "I")
        ((eq args_number 9) "J")
        ((eq args_number 10) "K")
        ((eq args_number 11) "L")
        ((eq args_number 12) "M")
        ((eq args_number 13) "n")
        ((eq args_number 14) "O")
        ((eq args_number 15) "P")
        ((eq args_number 16) "Q")
        ((eq args_number 17) "R")
        ((eq args_number 18) "S")
        ((eq args_number 19) "T")
        ((eq args_number 20) "U")
        ((eq args_number 21) "V")
        ((eq args_number 22) "W")
        ((eq args_number 23) "X")
        ((eq args_number 24) "Y")
        ((eq args_number 25) "Z")
        (( else) "__ERROR:TOO_MANY_ARGS__")))



;; *************************************************
;; *=================== Utility ===================*
;; *************************************************


(defun is_quoted_expression (expr)
    (and (atom (car expr)) (eq 'quote (car expr))))

(defun count_args (args_list) 
    (length args_list))

(defun else () t)
(defun next (l) (cdr l))