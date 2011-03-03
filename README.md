# README: cl-opencv #


## Introduction ##

These are OpenCV bindings for SBCL. They do not promise to be
complete, though eventually I hope to have fairly complete coverage of
OpenCV functionality. This package depends on the [cffi
package](http://common-lisp.net/project/cffi/) and libffi. I use
[Quicklisp](http://www.quicklisp.org/) to set up my Lisp environment.


## Compatibility ##

This package is known to work in the following configurations.

 - Mac OS 10.6, sbcl 1.0.45 (MacPorts), OpenCV 2.2 (MacPorts)
 - Ubuntu 10.10 (64-bit), sbcl 1.0.40, OpenCV 2.1

If you have gotten cl-opencv to run with some other combination of OS
and software versions, please let me know.


## Installation ##

### Mac OS X ###

 1. Install OpenCV from MacPorts: `port install opencv`
 2. Install libffi from MacPorts: `port install libffi`
 3. In your lisp environment, make sure that cffi is available. In
    Quicklisp you would just do `(ql:quickload "cffi")`.

### Ubuntu ###

 1. Install OpenCV from the repos: `sudo apt-get install libhighgui-dev`.
 2. Install libffi from the repos: `sudo apt-get install libffi`
 3. In your lisp environment, make sure that cffi is available. In
    Quicklisp you would just do `(ql:quickload "cffi")`.


## Troubleshooting ##

### Ubuntu ###

If you CFFI has problems loading the library, be sure:

 - your lisp and your OpenCV library are both 32 bit or both 64 bit
 - if you installed OpenCV to a non-standard location, you might try
 adding that path to cffi:*foreign-library-directories*

Different camera support different resolutions and framerates.  The
tests assume a camera capable of about 30fps at 640x480.  Adjust the
values at the top of test.lisp to match your camera.  A program like
VLC can help you determine those.
