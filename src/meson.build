################################################################################
#                                 SOURCES                                        
################################################################################

# List of sources
sources = [
    'main.c',
    'pibuiltin.c',
    'picell.c',
    'pichecks.c',
    'picore.c',
    'pierror.c',
    'pifile.c',
    'piinit.c',
    'pilisp.c',
    'piparser.c',
    'piprint.c',
    'piremove.c',
    'pistack.c',
    'pitestutils.c',
    'piutils.c',
]

# Creation of static library for testing
pilisplib = shared_library(
    'pilisplib',
    sources,
    include_directories : inc,
    dependencies : dependencies,
    install : false
)

# Main executable
executable(
    '../pilisp', 
    sources: sources,
    include_directories : inc,
    dependencies : dependencies,
    c_args: ['-O3','-finline-small-functions'],
    install: true
)