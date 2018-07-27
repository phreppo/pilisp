################################################################################
#                                 TESTS                                        
################################################################################

test_path = meson.source_root()+'/test/'
expressions_path = test_path+'expressions/'
programs_path = test_path+'lisp_programs/'
bad_expressions_path = expressions_path+'badexpressions/'

# Parser test for accepted sexpressions
parser_accepted_strings_test = executable(
    'parser_accepted_strings_test', 'parser_accepted_strings_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for parser for accepted sexpressions
test('[PAR] Void file test', parser_accepted_strings_test, args : [expressions_path+'void.lisp'])
test('[PAR] Atoms test', parser_accepted_strings_test, args : [expressions_path+'atoms.lisp'])
test('[PAR] Single atom test', parser_accepted_strings_test, args : [expressions_path+'atom.lisp'])
test('[PAR] Dot sexpressions test', parser_accepted_strings_test, args : [expressions_path+'dotexpressions.lisp'])
test('[PAR] List notation accepted strings 1', parser_accepted_strings_test, args : [expressions_path+'listnotation1.lisp'])
test('[PAR] List notation accepted strings 2', parser_accepted_strings_test, args : [expressions_path+'listnotation2.lisp'])

# Parser test for rejected sexpressions
parser_rejected_strings_test = executable(
    'parser_rejected_strings_test', 'parser_rejected_strings_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for parser for rejected sexpressions
test('[REJ] Bad sexpression with closed par test', parser_rejected_strings_test, args : [bad_expressions_path+'closedpar.lisp'])
test('[REJ] Incomplte sexpression test', parser_rejected_strings_test, args : [bad_expressions_path+'unfinished.lisp'])
test('[REJ] Complicate bad sexpression test', parser_rejected_strings_test, args : [bad_expressions_path+'complicate.lisp'])



# Print test for accepted sexpressions
print_test = executable(
    'print_test', 'print_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for print sexpressions
test('[PRINT] Void file print test', print_test, args : [expressions_path+'void.lisp'])
test('[PRINT] Atoms print test', print_test, args : [expressions_path+'atoms.lisp'])
test('[PRINT] Single atom print test', print_test, args : [expressions_path+'atom.lisp'])
test('[PRINT] Dot sexpressions print test', print_test, args : [expressions_path+'dotexpressions.lisp'])
test('[PRINT] Print list notation accepted strings 1', print_test, args : [expressions_path+'listnotation1.lisp'])
test('[PRINT] Print list notation accepted strings 2', print_test, args : [expressions_path+'listnotation2.lisp'])

# Print test for tokens
print_lexer_test = executable(
    'print_lexer_test', 'print_lexer_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for print tokens
test('[PRINT] Tokens print test', print_lexer_test, args : [expressions_path+'tokens.lisp'])

# Bad prints test
bad_prints_test = executable(
    'bad_prints_test', 'bad_prints_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for bad prints
test('[PRINT] Recognize bad prints test', bad_prints_test, args : [expressions_path+'tokens.lisp'])

# Sexpr copy test
sexpr_copy_test = executable(
    'sexpr_copy_test', 'sexpr_copy_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for bad prints
test('[COPY] Sexpression copy test', sexpr_copy_test, args : [])

# Lisp program test
lisp_program_test = executable(
    'lisp_program_test', 'lisp_program_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases lisp programs prints
test('[LISP] +', lisp_program_test, args : [
    '(+ 1 1 1)',
    '3 ',
    'L+'
])

test('[LISP] - 1', lisp_program_test, args : [
    '(- 1 1)',
    '0 ',
    'L-1'
])

test('[LISP] -2', lisp_program_test, args : [
    '(- 1)',
    '-1',
    'L-2'
])

test('[LISP] *', lisp_program_test, args : [
    '(* -1 -1)',
    '1 ',
    'L*'
])
test('[LISP] /', lisp_program_test, args : [
    '(/ 8 2 (+ 1 1))',
    '2 ' ,
    'LFR'
])
test('[LISP] or 1', lisp_program_test, args : [
    '(set \'n 1)(or n "ciao" n NIL)',
    '1 ',
    'LOR1'
])
test('[LISP] or 2', lisp_program_test, args : [
    '(or NIL NIL NIL NIL NIL)',
    'NIL',
    'LOR2'
])
test('[LISP] and 1', lisp_program_test, args : [
    '(and 1 NIL)',
    'NIL ' ,
    'LAND1'
])
test('[LISP] and 2', lisp_program_test, args : [
    '(or "hey" 7 "hey")',
    '"hey"',
    'LAND2'
])
test('[LISP] not 1', lisp_program_test, args : [
    '(not T)',
    'NIL',
    'LNOT1'
])
test('[LISP] not 2', lisp_program_test, args : [
    '(not NIL)',
    'T ',
    'LNOT2'
])
test('[LISP] car 1', lisp_program_test, args : [
    '(car \'(a b))',
    'A ',
    'LCAR1'
])
test('[LISP] car 2', lisp_program_test, args : [
    '(car \'((a a) b))',
    '(A A) ',
    'LCAR2'
])
test('[LISP] >', lisp_program_test, args : [
    '(set \'n 1)(> n (- 0 100))',
    'T ',
    'L>'
])
test('[LISP] >=', lisp_program_test, args : [
    '(set \'n 1)(>= n n)',
    'T ',
    'L>='
])
test('[LISP] <', lisp_program_test, args : [
    '(set \'n 1)(< (* 100 100) n)',
    'NIL ',
    'L<'
])
test('[LISP] <=', lisp_program_test, args : [
    '(set \'n 1)(<= n n)',
    'T ',
    'L<='
])
test('[LISP] lambda: id', lisp_program_test, args : [
    '(set \'str "hi")(set \'id (lambda (x) x)) (id str)',
    '"hi" ',
    'LLID'
])
test('[LISP] lambda: ff', lisp_program_test, args : [
    '(set \'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) (set \'str "hi") (ff \'(((((str)7)8))))',
    'str ',
    'LLFF'
])
test('[LISP] nth 1', lisp_program_test, args : [
    '(nth 1 \'(a b c) )',
    'B ',
    'LNTH1'
])
test('[LISP] nth 2', lisp_program_test, args : [
    '(nth 100 \'(a (1 2) c) )',
    'NIL ',
    'LNTH2'
])
test('[LISP] member 1', lisp_program_test, args : [
    '(set \'n 1)(member n \'(4 1 2 3) )',
    '(1 2 3) ',
    'LMEM1'
])
test('[LISP] member 2', lisp_program_test, args : [
    '(set \'n 1)(member n \'(n n2 NIL) )',
    'NIL',
    'LMEM2'
])
test('[LISP] list', lisp_program_test, args : [
    '(set \'n 1)(list n 1 1)',
    '(1 1 1)',
    'LLIST'
])
test('[LISP] length 1', lisp_program_test, args : [
    '(length \'(NIL NIL NIL) )',
    '3',
    'LLEN1'
])
test('[LISP] length 2', lisp_program_test, args : [
    '(length NIL )',
    '0',
    'LLEN2'
])
test('[LISP] load test', lisp_program_test, args : [
    '(load "'+programs_path+'loadtest.lisp")(+ n 1)',
    '2' ,
    'LLOAD'
])
test('[LISP] env printing test', lisp_program_test, args : [
    '(env)',
    'T',
    'LENV'
])

test('[LISP] set-sum test', lisp_program_test, args : [
    '(+ 1 2) (+ 1 2) (+ 1 2) (set \'n 3) n (+ 1 n)',
    '4',
    'LSUMSET'
])

test('[LISP] set-lambda test 1', lisp_program_test, args : [
    '(set \'id (lambda (x) x)) (set \'n 1) (id 1)',
    '1',
    'LLSET'
])

test('[LISP] set test', lisp_program_test, args : [
    '(set \'n1 1) (set \'n2 2) (set \'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) (ff \'(((n1 . n2))))',
    'n1',
    'LSET'
])

test('[LISP] dotimes test', lisp_program_test, args : [
    '(set \'n1 1) (set \'n2 2) (set \'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) (dotimes (n 100) (ff \'(n1 n2)))',
    # (set 'n1 1) 
    # (set 'n2 2) 
    # (set 'ff (lambda (x) (cond ((atom x) x ) 
    # (t (ff (car x)))))) (dotimes (n 100) (ff '(n1 n2)))'
    # (set 'n1 1) (set 'n2 2) (set 'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) (dotimes (n 100) (ff '(n1 n2)))
    'NIL',
    'LDOTIMES'
])

test('[LISP] to zero dotimes test', lisp_program_test, args : [
    '(set \'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1)))))) (time (dotimes (n 10) (toz n)))',
    'NIL',
    '30'
])

test('[LISP] cond test', lisp_program_test, args : [
    '(cond (nil nil) ((not nil) 1 ) (t 2) (t 1))',
    '1',
    'LCOND'
])

test('[LISP] stress test 1', lisp_program_test, args : [
    '(set \'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1)))))) (set \'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) (set \'iso (lambda (x) (cond ((eq x 1) 1 ) ((eq x 2) 2) (t 666)))) (set \'plo (lambda (n) (+ 1 n))) (dotimes (n 5) (toz 5)) (ff \'(a)) (ff \'((((a) b ) c ) d)) (ff \'((a) b )) ((lambda (x) (cond ((eq x 1) 1 ) ((eq x 2) 2) (t 666))) \'(1)) ((lambda (x) x) \'(1)) ((lambda (x) (lambda (y) y)) 1 ) (((lambda (x) (lambda (y) 2))  ) ) (((lambda (x) (lambda (y) y)) 1 ) 2) (((lambda (x) (lambda (y) y)) 1 2 ) 3) ((((lambda (x) (lambda (y) (lambda (z) z))) 1 ) 2) 3) ( (lambda (x) (+ 1 x ) ) 1 ) (set \'ivar (lambda (x) (set \'var x))) (set \'l \'(nil (1 2) ((1)(2)) nil "ciao")) (or nil nil nil \'(1 2 3) 4) (set \'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( T (toz (- x 1)))))) (dotimes (n 1) (toz n)) (dotimes (n 2) (toz n)) (dotimes (n 3) (toz n)) (dotimes (n 3) (+ 1 n)) (dotimes (n 1) 666) (dotimes (n 2) 666) (dotimes (n 3) 666) NIL',
    'NIL',
    'LSTRESS'
])

test('[LISP] label test', lisp_program_test, args : [
    '((label ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) \'((a)))',
    'A',
    'LLABEL'
])

test('[LISP] builtin setq test', lisp_program_test, args : [
    '(setq fun (lambda (x) x)) fun',
    '(lambda (x) x)',
    'LBSETQ'
])

test('[LISP] builtin defun test', lisp_program_test, args : [
    '(defun ff (x) (cond ((atom x) x ) (t (ff (car x)))) ) (ff \'(((a. c) .b)))',
    'a',
    'LBDEFUN'
])

test('[LISP] write function test', lisp_program_test, args : [
    '(setq num 1) (write (+ 1 num))',
    '2',
    'LWRITE'
])

test('[LISP] bye test', lisp_program_test, args : [
    '(BYE)',
    'BYE',
    'LBYE'
])

test('[LISP] map test 1', lisp_program_test, args : [
    '(map 1+ \'(99 100 101))',
    '(100 101 102)',
    'LMAP1'
])

test('[LISP] map test 2', lisp_program_test, args : [
    '(map 1+ NIL)',
    'NIL',
    'LMAP2'
])

test('[LISP] map test 3', lisp_program_test, args : [
    '(map not \'(1 2 NIL NIL "hi"))',
    '(NIL NIL T T NIL)',
    'LMAP3'
])

test('[LISP] subseq test 1', lisp_program_test, args : [
    '(subseq "ciao" 1 2)',
    '"i"',
    'LSUBSEQ1'
])

test('[LISP] subseq test 2', lisp_program_test, args : [
    '(subseq "stringa composta" 0 8)',
    '"stringa "',
    'LSUBSEQ2'
])

test('[LISP] reverse test 1', lisp_program_test, args : [
    '(not (reverse NIL))',
    'T',
    'LREVERSE1'
])

test('[LISP] reverse test 2', lisp_program_test, args : [
    '(setq l \'(1 2 3 (4 5))) (reverse l)',
    '((4 5) 3 2 1)',
    'LREVERSE2'
])

test('[LISP] keyword test 1', lisp_program_test, args : [
    ':imakeyword',
    ':imakeyword',
    'LKEYW1'
])

test('[LISP] keyword test 2', lisp_program_test, args : [
    '(atom :imakeyword)',
    'T',
    'LKEYW2'
])

test('[LISP] keyword test 3', lisp_program_test, args : [
    '(eq :imakeyword :imakeyword)',
    'T',
    'LKEYW3'
])

test('[LISP] keyword test 4', lisp_program_test, args : [
    '(eq :imakeyword :13)',
    'NIL',
    'LKEYW4'
])

test('[LISP] concatenate test 1', lisp_program_test, args : [
    '(concatenate \'string "Hi " "my friend")',
    '"Hi my friend"',
    'LCONCAT1'
])

test('[LISP] concatenate test 2', lisp_program_test, args : [
    '(setq str1 "one") (setq str2 "")(concatenate \'string str1 str2)',
    '"one"',
    'LCONCAT2'
])

test('[LISP] append test 1', lisp_program_test, args : [
    '(APPEND \'( 1 2 3 ) \'(4 5 6))',
    '(1 2 3 4 5 6)',
    'LAPPEND1'
])

test('[LISP] append test 2', lisp_program_test, args : [
    '(APPEND \'() \'(4 5 6))',
    '(4 5 6)',
    'LAPPEND2'
])

test('[LISP] append test 3', lisp_program_test, args : [
    '(APPEND \'() \'1)',
    '1',
    'LAPPEND3'
])

test('[LISP] append test 3', lisp_program_test, args : [
    '(APPEND \'(1 2 3 ) \'4)',
    '(1 2 3 . 4)',
    'LAPPEND4'
])

# ==================== MACROS ====================

test('[MACRO] simple macro test', lisp_program_test, args : [
    '((macro (name val) (set name val)) hi 1) hi',
    '1',
    'MMACRO'
])
 
test('[MACRO] mysetq test', lisp_program_test, args : [
    '(set \'mysetq (macro (name val) (set name val))) (mysetq ciao 1) ciao',
    '1',
    'MMYSETQ'
])

test('[MACRO] mydefun test 1', lisp_program_test, args : [
    '(set \'mydefun (macro (name param body) (list \'set (list \'quote name) (list \'lambda param body)))) (mydefun id (x) x) (id "hi")',
    '"hi"',
    'MMYDEFUN1'
])

test('[MACRO] mydefun test 2', lisp_program_test, args : [
    '(set \'mydefun (macro (name param body) (list \'set (list \'quote name) (list \'lambda param body)))) (mydefun toz (n) (cond ((eq n 0) 0) (t (toz (- n 1)))) ) (toz 100)',
    '0',
    'MMYDEFUN2'
])

test('[MACRO] mydefun test 3', lisp_program_test, args : [
    '(set \'mydefun (macro (name param body) (list \'set (list \'quote name) (list \'lambda param body)))) (mydefun ff (x) (cond ((atom x) x ) (t (ff (car x)))) ) (ff \'((((a)))))',
    'a',
    'MMYDEFUN3'
])

test('[MACRO] mydefun test 4', lisp_program_test, args : [
    '(set \'mydefun (macro (name param body) (list \'set (list \'quote name) (list \'lambda param body)))) (mydefun triple (x) (* x 3) ) (triple 333)',
    '999',
    'MMYDEFUN4'
])

# (set 'toz (lambda (x) ( cond ( (eq x 0) 0 ) ( t (toz (- x 1))))))
# ((label ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) '((a))

# Lisp program loaded from file test
lisp_program_load_test = executable(
    'lisp_program_load_test', 'lisp_program_load_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases lisp programs prints
test('[LOAD] first load test', lisp_program_load_test, args : [
    programs_path+'loadtest.lisp',
    '1',
    '1',
    'LOAD'
])

test('[LOAD] set-style maze test', lisp_program_load_test, args : [
    programs_path+'maze_old.lisp',
    '(solveMaze maze1)',
    '(2 3 1 0)',
    'SETMAZE'
])

test('[LOAD] builtin defun and setq maze test', lisp_program_load_test, args : [
    programs_path+'maze.lisp',
    '(solveMaze maze1)',
    '(2 3 1 0)',
    'MAZEBUILTIN'
])

test('[LOAD] diff calculator: d(33x)', lisp_program_load_test, args : [
    programs_path+'diff.lisp',
    '(d \'(* x 33))',
    '(+ (* 1 33) (* X 0))',
    'DIFF1'
])

test('[LOAD] diff calculator: d(x^2)', lisp_program_load_test, args : [
    programs_path+'diff.lisp',
    '(d \'(expt x 2))',
    '(* (EXPT X . 1) 2)',
    'DIFF2'
])

test('[LOAD] diff calculator: d((x^2+1)/cos(x))', lisp_program_load_test, args : [
    programs_path+'diff.lisp',
    '(d \'(/ (+ (expt x 2) 1) (cos x)))',
    '(/ (- (* (+ (* (EXPT X . 1) 2) 0) (COS X)) (* (+ (EXPT X 2) 1) (* (* (SIN X) -1) 1))) (EXPT (COS X) 2))',
    'DIFF3'
])

test('[LOAD] max in list', lisp_program_load_test, args : [
    programs_path+'max.lisp',
    '(list_max (list 1 2 3 4 5 6 7 8 9 1024))',
    '1024',
    'MAX'
])

test('[LOAD] factorial 10', lisp_program_load_test, args : [
    programs_path+'factorial.lisp',
    '(factorial 10)',
    '3628800',
    'FACT1'
])

test('[LOAD] factorial -1: NIL', lisp_program_load_test, args : [
    programs_path+'factorial.lisp',
    '(not (factorial -1))',
    't',
    'FACT2'
])

test('[LOAD] let maze', lisp_program_load_test, args :  [
    programs_path+'maze_let.lisp',
    # why this not? beacuse we can't write in a file # an empty cell: it won't write anything
    '(solveMaze maze1)',
    '(2 3 1 0)',
    'LETMAZE'
])

test('[LOAD] string include 1', lisp_program_load_test, args :  [
    programs_path+'ibeforee.lisp',
    '(not (string-include "ciao" "a"))',
    'T',
    'STRINGINCLUDE1'
])

test('[LOAD] string include 2', lisp_program_load_test, args :  [
    programs_path+'ibeforee.lisp',
    '(string-include "STRING" "ANOTHER CAPS LOCK STRING")',
    '"STRING"',
    'STRINGINCLUDE2'
])

test('[LOAD] I before E except after C', lisp_program_load_test, args :  [
    programs_path+'ibeforee.lisp',
    # challange took from: https://www.reddit.com/r/dailyprogrammer/comments/8q96da/20180611_challenge_363_easy_i_before_e_except/
    '(map check \'("a" "zombie" "transceiver" "veil" "icier"))',
    '(t t t NIL NIL)',
    'IBEFEEXCC'
])

test('[LOAD] map and maze with list', lisp_program_load_test, args :  [
    programs_path+'maze.lisp',
    '(map solvemaze \'( \'((1) (0 3) (3 -1) (1 2)) \'((3 1) (0 2 4) (1 5) (0 4) (1 3) (2 8) (-1 7) (6 8) (5 7)) ))',
#'(map solvemaze '( ((1) (0 3) (3 -1) (1 2)) ((3 1) (0 2 4) (1 5) (0 4) (1 3) (2 8) (-1 7) (6 8) (5 7)) ))',
#    '(map solvemaze '( ((1) (0 3) (3 -1) (1 2)) )',
    '((2 3 1 0) (6 7 8 5 2 1 4 3 0))',
    'MAPMAZELIST'
])

test('[LOAD] map and maze with variables', lisp_program_load_test, args :  [
    programs_path+'maze.lisp',
    '(map solvemaze \'(maze1 maze2))',
    '((2 3 1 0) (6 7 8 5 2 1 4 3 0))',
    'MAPMAZEVAR'
])

test('[COMP] number const', lisp_program_test, args : [
    '(plc \'1)',
    '(ASM "!" 1)',
    'CCONSTNUM'
])

test('[COMP] string const', lisp_program_test, args : [
    ' (plc \'"hi")',
    '(ASM "!" "hi")',
    'CCONSTSTR'
])

test('[COMP] symbol', lisp_program_test, args : [
    '(plc \'x)',
    '(ASM "?" x)',
    'CSYM'
])


test('[COMP] 0 par BS', lisp_program_test, args : [
    '(plc \'(car))',

    '(ASM "$A" CAR)',
    'CBSPAR0'
])

test('[COMP] 1 par BS', lisp_program_test, args : [
    '(plc \'(car \'(a)))',

    '(ASM "!$B" (a) CAR)',
    'CBSPAR1'
])

test('[COMP] 2 par BS', lisp_program_test, args : [
    '(plc \'(cons 1 x))',

    '(ASM "!?$C" 1 X CONS)',
    'CBSPAR2'
])

test('[COMP] 3 par BS', lisp_program_test, args : [
    '(plc \'(list 1 2 3))',

    '(ASM "!!!$D" 1 2 3 LIST)',
    'CBSPAR3'
])

test('[COMP] 4 par BS', lisp_program_test, args : [
    '(plc \'(list 1 2 3 4))',

    '(ASM "!!!!$E" 1 2 3 4 LIST)',
    'CBSPAR4'
])

test('[COMP] N par BS', lisp_program_test, args : [
    '(plc \'(list 1 2 3 4 5 6 7))',

    '(ASM "!!!!!!!$H" 1 2 3 4 5 6 7 LIST)',
    'CBSPARN5'
])

test('[COMP] nested car and cdr stack 1', lisp_program_test, args : [
    '(plc \'(car (cdr \'(a))))',

    '(ASM "!$B$B" (a) CDR CAR)',
    'CNESTEDBS1'
])

test('[COMP] nested list and car stack 2', lisp_program_test, args : [
    '(plc \'(list 1 2 3 (car \'(a))))',

    '(ASM "!!!!$B$E" 1 2 3 (a) CAR LIST)',
    'CNESTEDBS2'
])

test('[COMP] nested list, car and cdr stack 2', lisp_program_test, args : [
    '(plc \'(list 1 (car \'(2)) 3 (car (cdr \'(a b)))))',

    '(ASM "!!$B!!$B$B$E" 1 (2) CAR 3 (A B) CDR CAR LIST)',
    'CNESTEDBS3'
])

test('[COMP] (lambda (x) x)', lisp_program_test, args : [
    '(plc \'(lambda (x) x))',

    '(LASM 1 "@A")',
    'CLASM1'
])

test('[COMP] (lambda (x y) (+ x y))', lisp_program_test, args : [
    '(plc \'(lambda (x y) (+ x y)))',

    '(LASM 2 "@A@B$C" +)',
    'CLASM2'
])


test('[COMP] (lambda (x y) 11)', lisp_program_test, args : [
    '(plc \'(lambda (x y) 11))',

    '(LASM 2 "!" 11)',
    'CLASM3'
])

test('[COMP] (lambda () (list 1 2))', lisp_program_test, args : [
    '(plc \'(lambda () (list 1 2)))',

    '(LASM 0 "!!$C" 1 2 LIST)',
    'CLASM4'
])

test('[COMP] (lambda (x y z) (list (car \'(A)) 2 x y x ext))', lisp_program_test, args : [
    '(plc \'(lambda (x y z) (list (car \'(A)) 2 x y x ext)))',

    '(LASM 3 "!$B!@A@B@A?$G" (A) CAR 2 EXT LIST)',
    'CLASM5'
])

# ==================== ASM ====================
test('[ASM] load const 1', lisp_program_test, args : [
    '(ASM "!" 1)',
    '1',
    'ASMLCONST1'
])

test('[ASM] load const 2', lisp_program_test, args : [
    '(ASM "!" "hi")',
    '"hi"',
    'ASMLCONST2'
])

test('[ASM] builtin stack call car', lisp_program_test, args : [
    '(ASM "!$B" (A B) CAR)',
    'A',
    'ASMLCBS1'
])

test('[ASM] builtin stack call cdr', lisp_program_test, args : [
    '(ASM "!$B" (A B) CDR)',
    '(B)',
    'ASMLCBS2'
])

test('[ASM] (car (cdr \'(a b)))', lisp_program_test, args : [
    '(ASM "!$B$B" (A B) CDR CAR)',
    'B',
    'ASMLCBS3'
])

test('[ASM] (list 1 2 3 4)', lisp_program_test, args : [
    '(ASM "!!!!$E" 1 2 3 4 LIST)',
    '(1 2 3 4)',
    'ASMLCBS4'
])

test('[ASM] (list 1 2 3 (car \'(4))', lisp_program_test, args : [
    '(ASM "!!!!$B$E" 1 2 3 (4) CAR LIST)',
    '(1 2 3 4)',
    'ASMLCBS4'
])

test('[ASM] (list 1 (car \'(2)) 3 (car (cdr \'(A B)))', lisp_program_test, args : [
    '(ASM "!!$B!!$B$B$E" 1 (2) CAR 3 (A B) CDR CAR LIST)',
    '(1 2 3 B)',
    'ASMLCBS5'
])

test('[ASM] var recognition 1', lisp_program_test, args : [
    '(SETQ A "hi") (ASM "?" A)',
    '"hi"',
    'ASMASSOC1'
])

test('[ASM] var recognition 2', lisp_program_test, args : [
    '(SETQ MYLIST \'(A B C)) (ASM "?$B$B" MYLIST CDR CAR)',
    'B',
    'ASMASSOC2'
])


test('[LASM] ((lambda (x) 1))', lisp_program_test, args : [
    '((LASM 0 "!" 1))',
    '1',
    'LASM1'
])

test('[LASM] ((lambda (x) x) 1)', lisp_program_test, args : [
    '((LASM 1 "@A") 1)',
    '1',
    'LASM2'
])

test('[LASM] ((lambda (x) (car x)) \'(a))', lisp_program_test, args : [
    '((LASM 1 "@A$B" CAR) \'(A))',
    'A',
    'LASM3'
])

test('[LASM] ((lambda (x y z) (list x y z 1 2)) 1 2 3)', lisp_program_test, args : [
    '((LASM 3 "@A@B@C!!$F" 1 2 LIST) 1 2 3)',
    '(1 2 3 1 2)',
    'LASM4'
])

test('[COMPILED] compile my eq', lisp_program_test, args : [
    '(defun myeq (x y) (eq x y))  '+
    '(compile myeq)' +
    '(myeq nil 11)',

    'NIL',
    'COMPILED1'
])

test('[COMPILED] compile my cons', lisp_program_test, args : [
    '(defun mycons (x y) (cons x y))  '+
    '(compile mycons)' +
    '(mycons 1 \'(a b c))',

    '(1 . (a b c))',
    'COMPILED2'
])
 
test('[COMPILED] compile my atom 1', lisp_program_test, args : [
    '(defun myatom (x) (atom x))  '+
    '(compile myatom)' +
    '(myatom 1)',

    'T',
    'COMPILED3'
])

test('[COMPILED] compile my atom 2', lisp_program_test, args : [
    '(defun myatom (x) (atom x))  '+
    '(compile myatom)' +
    '(myatom \'(a b c))',

    'NIL',
    'COMPILED4'
])

test('[COMPILED] compile my +', lisp_program_test, args : [
    '(defun mysum (x y) (+ x y))  '+
    '(compile mysum)' +
    '(mysum 1 -3)',

    '-2',
    'COMPILED5'
])

test('[COMPILED] compile my car', lisp_program_test, args : [
    '(defun mycar (x) (car x))  '+
    '(compile mycar)' +
    '(mycar \'(a))',

    'a',
    'COMPILED6'
])

test('[COMPILED] compile my list', lisp_program_test, args : [
    '(defun mylist (x y z) (list x y z))  '+
    '(compile mylist)' +
    '(mylist \'(a) 1 2)',

    '((a) 1 2)',
    'COMPILED7'
])

test('[COMPILED] compiled diff: d(33x)', lisp_program_load_test, args : [
    programs_path+'diff.lisp',
    '(load "'+programs_path+'compilable_diff.lisp")'+
    '(d \'(* x 33))',
    '(+ (* 1 33) (* X 0))',
    'COMPILEDDIFF1'
])


test('[COMPILED] diff calculator: d(x^2)', lisp_program_load_test, args : [
    programs_path+'diff.lisp',
    '(load "'+programs_path+'compilable_diff.lisp")'+
    '(d \'(expt x 2))',
    '(* (EXPT X . 1) 2)',
    'COMPILEDDIFF2'
])

test('[COMPILED] diff calculator: d((x^2+1)/cos(x))', lisp_program_load_test, args : [
    programs_path+'diff.lisp',
    '(load "'+programs_path+'compilable_diff.lisp")'+
    '(d \'(/ (+ (expt x 2) 1) (cos x)))',
    '(/ (- (* (+ (* (EXPT X . 1) 2) 0) (COS X)) (* (+ (EXPT X 2) 1) (* (* (SIN X) -1) 1))) (EXPT (COS X) 2))',
    'COMPILEDDIFF3'
])