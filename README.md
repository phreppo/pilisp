# pilisp #

[![Build Status](https://travis-ci.com/parof/pilisp.svg?token=tdfVkJVdJvEzUpskJRQE&branch=master)](https://travis-ci.com/parof/pilisp) [![codecov](https://codecov.io/gh/parof/pilisp/branch/master/graph/badge.svg)](https://codecov.io/gh/parof/pilisp) [![Github Pages docs](https://img.shields.io/badge/docs-ghpages-blue.svg)](https://parof.github.io/pilisp/)

* [Introduction](#introduction)
* [Documentation](#documentation)
* [Installation](#installation)

## Introduction ##
Pilisp aims to be a small LISP interpreter for the 1.5 version of the language described [here](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf).

## Documentation ##

Full code documentation can be found on [github pages](https://parof.github.io/pilisp/). It is automatically generated using [Doxygen](http://www.stack.nl/~dimitri/doxygen/), with [Bootstrap](https://getbootstrap.com/) CSS (using [this](https://github.com/Velron/doxygen-bootstrapped) guide). The code documentation is generated every push with [Travis CI](https://travis-ci.org/), so it should be always up to date.

## Installation ##

### Prerequisites ###

* [Meson](http://mesonbuild.com/)   (version 0.44 or newer)
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

To run tests use

```
ninja test -C build
```