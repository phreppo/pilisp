################################################################################
#                               CONFIGURATION
################################################################################

# Project setup
project(
  'pilisp', 'c',
  version : '1.0.0',
)

# Compiler
cc = meson.get_compiler('c')
# arguments for the coverage
# coverage_arguments = ['-O3']
# add_global_arguments(coverage_arguments, language : 'c')

# Dependencies
thread_dep = dependency('threads')                # pthreads
mathlib = cc.find_library('m', required: false)   # math.h
dependencies =[
    thread_dep,
    mathlib
]

# Includes
inc = include_directories('include')

# Subdirs
subdir('include')
subdir('src')
subdir('test')