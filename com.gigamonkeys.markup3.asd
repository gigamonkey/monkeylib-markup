;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.markup3
  :name "com.gigamonkeys.markup3"
  :components
  ((:file "packages")
   (:file "markup3" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages"))
   (:file "html" :depends-on ("packages"))
   (:file "xml" :depends-on ("packages")))
  :depends-on
  (:com.gigamonkeys.macro-utilities
   :com.gigamonkeys.utilities
   :com.gigamonkeys.foo))
