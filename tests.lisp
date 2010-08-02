;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package com.gigamonkeys.markup3)

(defparameter *expected-failures* ())
(defparameter *to-skip* '())

(defun test-number (txt)
  (parse-integer (subseq (pathname-name txt) 5)))

(defun tests ()
  (loop with passed = 0
       with failed = 0
       with skipped = 0
     for file in (directory "./tests/test_*.txt")
     for n = (test-number file)
     do
       (cond
         ((member n *to-skip*)
          (format t "~&Skipping test ~d." n)
          (incf skipped))
         ((test-file file)
          (incf passed))
         (t (incf failed)))
     finally (format t "~&~d passed; ~d failed; ~d skipped." passed failed skipped)))


(defun test (n &rest parser-args)
  (apply #'test-file 
         (make-pathname
          :directory '(:relative "tests")
          :name (format nil "test_~2,'0d" n)
          :type "txt")
         parser-args))

(defun ok (n)
  (ok-file 
   (make-pathname
    :directory '(:relative "tests")
    :name (format nil "test_~2,'0d" n)
    :type "txt")))

(defun test-file (txt &rest parser-args)
  (let* ((sexp (make-pathname :type "sexp" :defaults txt))
         (parsed (apply #'parse-file txt parser-args))
         (expected (get-expected sexp))
         (ok (equal parsed expected))
         (n (test-number txt)))
    (if ok
        (format t "~&[~d] okay." n)
        (format t "~&[~d] FAIL:~2&Got:~2&~s~2&Expected:~2&~s" n parsed expected))
    ok))

(defun ok-file (txt)
  (with-output-to-file (out (make-pathname :type "sexp" :defaults txt))
    (with-standard-io-syntax
      (print (parse-file txt) out))))

(defun get-expected (file)
  (with-open-file (in file :if-does-not-exist :create)
    (read in nil nil)))

(defun show-string (string)
  (let ((output ()))
    (let ((translator (make-basic-translator-chain (lambda (char) (push char output)))))
      (loop for c across string do (funcall translator c))
      (funcall translator :eof)
      (nreverse output))))

(defun show-file (file)
  (with-open-file (in file)
    (let ((output ()))
      (let ((translator (make-basic-translator-chain (lambda (char) (push char output)))))
        (loop for c = (read-char in nil nil)
           while c do (funcall translator c))
        (funcall translator :eof)
        (nreverse output)))))


