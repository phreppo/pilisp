################################################################################
#                                 SOURCES                                        
################################################################################

# List of sources
sources = [
    'pilisp.c',
    'main.c'
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