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
arguments = ['-O3','-faggressive-loop-optimizations']
# arguments = []
add_global_arguments(arguments, language : 'c')

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