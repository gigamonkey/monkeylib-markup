;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;


(in-package :cl-user)

(defpackage :com.gigamonkeys.markup3
  (:use :common-lisp 
        :com.gigamonkeys.macro-utilities
        :com.gigamonkeys.utilities
        :com.gigamonkeys.pathnames)
  (:export :parse-file))

(defpackage :com.gigamonkeys.markup3.html
  (:use :common-lisp
        :com.gigamonkeys.markup3
        :com.gigamonkeys.utilities
        :com.gigamonkeys.foo))

(defpackage :com.gigamonkeys.markup3.xml
  (:use :common-lisp
        :com.gigamonkeys.markup3
        :com.gigamonkeys.utilities
        :com.gigamonkeys.foo
        :com.gigamonkeys.foo.xml))