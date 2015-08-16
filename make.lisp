#!/usr/bin/env sbcl --script

;; setup quicklisp
(let ((quicklisp-init
       (merge-pathnames "quicklisp/setup.lisp"
                        (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; build system
(asdf:operate 'asdf:program-op 'raytracer)
