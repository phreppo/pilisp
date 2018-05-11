################################################################################
#                                 TESTS                                        
################################################################################

# Test 1
testSqrt = executable(
    'testSqrt', 'testRadiceQuadrata.c',
    include_directories : inc,
    link_with : pilisplib,
    # TODO: learn how to use gcovr. Install package gcovr and run 'gcovr -r .', but for now it has 0%
    # link_args : coverage_arguments
)
# Test 1 cases
test('sqrt(25)', testSqrt, args : ['25', '5'])
test('sqrt(9)', testSqrt, args : ['9', '3'])
test('sqrt(4)', testSqrt, args : ['4', '2'])