# pilisp #

[![Build Status](https://travis-ci.com/parof/pilisp.svg?token=tdfVkJVdJvEzUpskJRQE&branch=master)](https://travis-ci.com/parof/pilisp) [![Documentation Status](https://readthedocs.org/projects/pilisp/badge/?version=latest)](http://pilisp.readthedocs.io/en/latest/?badge=latest)

* [Installation](##Installation)

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