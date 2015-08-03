# cl-raytracer

A project to port my undergrad raytracer project from C++ to common lisp.

## Install

I'm using [SBCL](https://en.wikipedia.org/wiki/Steel_Bank_Common_Lisp) to build this.

Other than that, I've also installed [quicklisp](https://www.quicklisp.org/beta/)

Then you should symlink the `cl-raytracer` directory into `~/quicklisp/local-projects/`
or whever you set up quicklisp.

## Run

```
$ sbcl
* (require 'asdf)
* (asdf:load-system 'raytracer)
* (raytracer:tracer)
```
