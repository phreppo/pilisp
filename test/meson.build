################################################################################
#                                 TESTS                                        
################################################################################

# Parser test
parser_test = executable(
    'parser_test', 'parser_test.c',
    include_directories : inc,
    link_with : pilisplib
)
# Parser tests
test('Void file test', parser_test, args : [meson.source_root()+'/test/expressions/void.l'])
test('Atoms test', parser_test, args : [meson.source_root()+'/test/expressions/atoms.l'])
test('Single atom test', parser_test, args : [meson.source_root()+'/test/expressions/atom.l'])
test('Dot sexpressions test', parser_test, args : [meson.source_root()+'/test/expressions/dotexpressions.l'])
