;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.markup
  :name "com.gigamonkeys.markup"
  :description "Library for parsing Markup-formatted text."
  :components
  ((:file "packages")
   (:file "markup" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages")))
  :depends-on
  (:cl-ppcre
   :com.gigamonkeys.utilities
   :com.gigamonkeys.pathnames))
