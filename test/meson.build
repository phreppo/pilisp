################################################################################
#                                 TESTS                                        
################################################################################

test_path = meson.source_root()+'/test/'
expressions_path = test_path+'expressions/'
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
