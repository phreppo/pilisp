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
