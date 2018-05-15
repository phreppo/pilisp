# pilisp #


[![Build Status](https://travis-ci.com/parof/pilisp.svg?token=tdfVkJVdJvEzUpskJRQE&branch=master)](https://travis-ci.com/parof/pilisp) [![Documentation Status](https://readthedocs.org/projects/pilisp/badge/?version=latest)](http://pilisp.readthedocs.io/en/latest/?badge=latest) [![codecov](https://codecov.io/gh/parof/pilisp/branch/master/graph/badge.svg)](https://codecov.io/gh/parof/pilisp)

* [Installation](#installation)
* [Generate Documentation](#generatedocumentation)

## Generate Documentation ##

Install `doxygen` and `moxygen`, then run 
```
doxygen 
```

```
moxygen --anchors --modules --output docs/markdown/api.md docs/xml
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
