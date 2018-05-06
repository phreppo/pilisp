# pilisp
[![Build Status](https://travis-ci.com/parof/pilisp.png?branch=master)](https://travis-ci.com/parof/pilisp)

* [Installation](Installation)

## Installation

### Prerequisites

* [Meson](http://mesonbuild.com/) 
* [Python](https://www.python.org/) (version 3.5 or newer)
* [Ninja](https://ninja-build.org/) (version 1.5 or newer)

### Installing with Meson
This commands should run on any OS. To build the `ninja.build` file run 
```
meson build && cd build
```
To build the executable run 
```
ninja
```
To install `pilisp` run 
```
ninja install
```