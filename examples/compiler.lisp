(load "instructions_generator.lisp")
(load "machine_code_generator.lisp")

; (cc '[EXPRESSION]) -> (lap [LAP_STRING] {ARGS_LIST})
(defun cc (not_evaluated_expression)
    ( get_interpretable_code 
        ( first_compile not_evaluated_expression)))