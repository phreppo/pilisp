project('pilisp', 'c',
  version : '1.0.0',
)

cc = meson.get_compiler('c')

# glib_dep = dependency('glib-2.0')

thread_dep = dependency('threads')

mathlib = cc.find_library('m', required: false)

dependencies =[
    thread_dep,
    mathlib
]

inc = include_directories('include')

subdir('include')
subdir('src')
subdir('test')

executable('pilisp', 'main.c', link_with : pilisplib)