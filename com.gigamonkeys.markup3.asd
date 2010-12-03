;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.markup
  :name "com.gigamonkeys.markup"
  :components
  ((:file "packages")
   (:file "markup" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages"))
   (:file "html" :depends-on ("packages"))
   (:file "xml" :depends-on ("packages")))
  :depends-on
  (:cl-ppcre
   :com.gigamonkeys.macro-utilities
   :com.gigamonkeys.utilities
   :com.gigamonkeys.foo))
