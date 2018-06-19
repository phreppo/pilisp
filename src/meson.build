################################################################################
#                                 SOURCES                                        
################################################################################

# List of sources
sources = [
    'main.c',
    'pibuiltin.c',
    'picell.c',
    'picore.c',
    'pierror.c',
    'pilisp.c',
    'piparser.c',
    'piprint.c',
    'pitestutils.c',

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
    install: true
)