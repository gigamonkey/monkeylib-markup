;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(defsystem com.gigamonkeys.markup3
  :name "com.gigamonkeys.markup3"
  :components
  ((:file "packages")
   (:file "markup3" :depends-on ("packages"))
   (:file "tests" :depends-on ("packages")))q
  :depends-on (:com.gigamonkeys.macro-utilities))
