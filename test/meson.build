testSqrt = executable(
    'testSqrt', 'testRadiceQuadrata.c',
    include_directories : inc,
    link_with : pilisplib
)
test('sqrt(25)', testSqrt, args : ['25', '5'])
test('sqrt(9)', testSqrt, args : ['9', '3'])
test('sqrt(4)', testSqrt, args : ['4', '2'])