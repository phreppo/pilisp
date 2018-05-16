# pilisp #


[![Build Status](https://travis-ci.com/parof/pilisp.svg?token=tdfVkJVdJvEzUpskJRQE&branch=master)](https://travis-ci.com/parof/pilisp) [![codecov](https://codecov.io/gh/parof/pilisp/branch/master/graph/badge.svg)](https://codecov.io/gh/parof/pilisp) [![Generic badge](https://img.shields.io/badge/docs-ghpages-brightgreen.svg)](https://parof.github.io/pilisp/html/index.html)

* [Documentation](#documentation)
* [Installation](#installation)
* [Generate Documentation](#generatedocumentation)

## Documentation ##
Full code documentation can be found here [https://parof.github.io/pilisp/html/index.html](https://parof.github.io/pilisp/html/index.html). It is automatically generated using [Doxygen](http://www.stack.nl/~dimitri/doxygen/).

## Generate Documentation ##

Install `doxygen` and `moxygen`, then run 
```
doxygen 
```

```
moxygen --groups --output=./docs/api-%s.md docs/xml
```

## Installation ##

### Prerequisites ###

* [Meson](http://mesonbuild.com/)
* [Python](https://www.python.org/) (version 3.5 or newer)
* [Ninja](https://ninja-build.org/) (version 1.5 or newer)

### Installing with Meson ###

These commands should run on any OS. To build the `ninja.build` file run

```
meson build
```

To build the executable in the `build` directory run
```
ninja -C build
```

To install `pilisp` run with root permissions
```
ninja install -C build
```

## TODO ##

* Learn how to output moxygen in multiple files. (something related to "groups")
* Learn and use breathe to convert doxygen to Sphinx documentation
* Learn to use the stack "doxygen breathe shpinx readthedocs"
