; (cc '[EXPRESSION]) -> (lap [LAP_STRING] {ARGS_LIST})
(defun cc (not_evaluated_expression)
    ( get_interpretable_code 
        ( first_compile not_evaluated_expression)))

;; ==================== Instructions Generator ====================

; Instructions:
;      :loadconst
;      :loadsymbol
;      :argsnum
;      :cbs[0-3] -> call builtin stack with 0-3 params
;      :cbsn -> call builtin stack with n > 3 params

(setq builtin_stack_lambdas 
    '( car cdr cons atom eq list))

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
        ((atom expr)
            ( compile_atom expr))
        
        (( is_quoted_expression expr)
            ( compile_quote expr))

        ((atom (car expr))
            ( compile_atom_function expr)) 

        (( else )
            "_ERROR:UNRECOGNIZED_FUNCTION_") 
    ))

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
    (member fun builtin_stack_lambdas))

(defun compile_builtin_stack (fun args_list)
    ( compile_args_and_append_builtin_stack fun args_list ( count_args args_list)))

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
    ( compile_args_and_append_builtin_stack fun ( next args_list) initial_args_number))

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

(defun is_quoted_expression (expr)
    (and (atom (car expr)) (eq 'quote (car expr))))

(defun count_args (args_list) 
    (length args_list))

(defun else () t)
(defun next (l) (cdr l))


;; ==================== Machine Code Generation ====================

; ((:[INSTRUCTION] . [PARAM]) {(:[INSTRUCTION] . [PARAM])} ) 
;           -> (LAP "{MACHINE_CODE_OPERATIONS}" {PARAMETERS})
(defun get_interpretable_code (compiled_expression)
    (cons 'lap 
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
        ((eq code :loadsymbol) "7")
        ((eq code :cbs0) "A")
        ((eq code :cbs1) "B")
        ((eq code :cbs2) "C")
        ((eq code :cbs3) "D")
        ((eq code :cbsn) "A")
        ((eq code :argsnum) ( get_instruction_code_for_n_args arg))
        (( else) "__ERROR:UNKNOWN_INSTRUCTION_CODE__")))

(defun get_instruction_code_for_n_args (args_number)
    (cond
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
        ((eq args_number 15) "Q")
        ((eq args_number 15) "R")
        ((eq args_number 15) "S")
        ((eq args_number 15) "T")
        ((eq args_number 15) "U")
        ((eq args_number 15) "V")
        ((eq args_number 15) "W")
        ((eq args_number 15) "X")
        ((eq args_number 15) "Y")
        ((eq args_number 15) "Z")
        (( else) "__ERROR:TOO_MANY_ARGS__")))