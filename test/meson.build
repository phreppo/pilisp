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
test('[REJ] Bad list notation test', parser_rejected_strings_test, args : [bad_expressions_path+'badlistnotation.lisp'])

# Sexpression print tester for self-referencing structures
recursive_structure_print_test = executable(
    'recursive_structure_print_test', 'recursive_structure_print_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for parser for rejected sexpressions
test('[PRINT] Print self-referencing structures test', recursive_structure_print_test)

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
    '1'
])
test('[LISP] -', lisp_program_test, args : [
    '(- 1 1)',
    '0 ',
    '2'
])
test('[LISP] *', lisp_program_test, args : [
    '(* -1 -1)',
    '1 ',
    '3'
])
test('[LISP] /', lisp_program_test, args : [
    '(/ 8 2 (+ 1 1))',
    '2 ' ,
    '3'
])
test('[LISP] or 1', lisp_program_test, args : [
    '(set \'n 1)(or n "ciao" n NIL)',
    '1 ',
    '4'
])
test('[LISP] or 2', lisp_program_test, args : [
    '(or NIL NIL NIL NIL NIL)',
    'NIL',
    '5'
])
test('[LISP] and 1', lisp_program_test, args : [
    '(and 1 NIL)',
    'NIL ' ,
    '6'
])
test('[LISP] and 2', lisp_program_test, args : [
    '(or "hey" 7 "hey")',
    '"hey"',
    '7'
])
test('[LISP] not 1', lisp_program_test, args : [
    '(not T)',
    'NIL',
    '8'
])
test('[LISP] not 2', lisp_program_test, args : [
    '(not NIL)',
    'T ',
    '9'
])
test('[LISP] car 1', lisp_program_test, args : [
    '(car \'(a b))',
    'A ',
    '10'
])
test('[LISP] car 2', lisp_program_test, args : [
    '(car \'((a a) b))',
    '(A A) ',
    '11'
])
test('[LISP] >', lisp_program_test, args : [
    '(set \'n 1)(> n (- 0 100))',
    'T ',
    '12'
])
test('[LISP] >=', lisp_program_test, args : [
    '(set \'n 1)(>= n n)',
    'T ',
    '13'
])
test('[LISP] <', lisp_program_test, args : [
    '(set \'n 1)(< (* 100 100) n)',
    'NIL ',
    '14'
])
test('[LISP] <=', lisp_program_test, args : [
    '(set \'n 1)(<= n n)',
    'T ',
    '15'
])
test('[LISP] lambda: id', lisp_program_test, args : [
    '(set \'str "hi")(set \'id (lambda (x) x)) (id str)',
    '"hi" ',
    '16'
])
test('[LISP] lambda: ff', lisp_program_test, args : [
    '(set \'ff (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) (set \'str "hi") (ff \'(((((str)7)8))))',
    'str ',
    '17'
])
test('[LISP] nth 1', lisp_program_test, args : [
    '(nth 1 \'(a b c) )',
    'B ',
    '18'
])
test('[LISP] nth 2', lisp_program_test, args : [
    '(nth 100 \'(a (1 2) c) )',
    'NIL ',
    '19'
])
test('[LISP] member 1', lisp_program_test, args : [
    '(set \'n 1)(member n \'(4 1 2 3) )',
    '(1 2 3) ',
    '20'
])
test('[LISP] member 2', lisp_program_test, args : [
    '(set \'n 1)(member n \'(n n2 NIL) )',
    'NIL',
    '21'
])
test('[LISP] length 1', lisp_program_test, args : [
    '(length \'(NIL NIL NIL) )',
    '3',
    '22'
])
test('[LISP] length 2', lisp_program_test, args : [
    '(length NIL )',
    '0',
    '23'
])
test('[LISP] load test', lisp_program_test, args : [
    '(load "'+programs_path+'loadtest.lisp")(+ n 1)',
    '2' ,
    '24'
])
test('[LISP] env printing test', lisp_program_test, args : [
    '(env)',
    'T',
    '25'
])