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
test('Void file test', parser_accepted_strings_test, args : [expressions_path+'void.l'])
test('Atoms test', parser_accepted_strings_test, args : [expressions_path+'atoms.l'])
test('Single atom test', parser_accepted_strings_test, args : [expressions_path+'atom.l'])
test('Dot sexpressions test', parser_accepted_strings_test, args : [expressions_path+'dotexpressions.l'])

# Parser test for rejected sexpressions
parser_rejected_strings_test = executable(
    'parser_rejected_strings_test', 'parser_rejected_strings_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Cases for parser for rejected sexpressions
test('Bad sexpression with closed par test', parser_rejected_strings_test, args : [bad_expressions_path+'badsexpr1.l'])
test('Bad sexpression with dot test', parser_rejected_strings_test, args : [bad_expressions_path+'badsexpr2.l'])
test('Incomplte sexpression test', parser_rejected_strings_test, args : [bad_expressions_path+'badsexpr3.l'])
test('Complicate bad sexpression test', parser_rejected_strings_test, args : [bad_expressions_path+'badsexpr4.l'])
