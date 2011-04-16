;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package com.gigamonkeys.markup)

(defparameter *to-skip* '())

(defun test-number (txt)
  (parse-integer (first (cl-ppcre:all-matches-as-strings "[0-9]+" (pathname-name txt)))))


(defun test-files (dir)
  (let ((files ()))
    (walk-directory dir (lambda (x) 
                          (when (cl-ppcre:scan ".*[0-9].*\\.txt" (file-namestring x))
                            (push x files))))
    (sort files #'< :key #'test-number)))


(defun tests (&optional (dir "tests/"))
  (loop with passed = 0
     with failed = 0
     with skipped = 0
     for file in (test-files dir)
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
         (text (file-text txt))
         (parsed (apply #'parse-file txt parser-args))
         (string-parsed (apply #'parse-text text parser-args))
         (expected (get-expected sexp))
         (ok (equal parsed expected))
         (file-vs-text-ok (equal parsed string-parsed))
         (n (test-number txt)))
    (if ok
        (format t "~&[~d] Parsed to expected sexp." n)
        (format t "~&[~d] FAIL:~2&Got:~2&~s~2&Expected:~2&~s" n parsed expected))
    (if file-vs-text-ok
        (format t "~&[~d] parse-file vs parse-text okay." n)
        (format t "~&[~d] FAIL:~2&parse-file:~2&~s~2&parse-text:~2&~s" n parsed string-parsed))
    (and ok file-vs-text-ok)))

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


(defun renumber (&optional (spacing 1))
  (flet ((file-to-list (p)
           (let ((filename (pathname-name p)))
             (multiple-value-bind (num pos) (parse-integer filename :junk-allowed t)
               (if num
                   (list num p (subseq filename (1+ pos)))
                   (list most-positive-fixnum p filename))))))

    (let ((tests (sort (mapcar #'file-to-list (remove-if-not #'txt-p (list-directory "."))) #'< :key #'car)))
      
      (loop with digits = (max (ceiling (log (1+ (length tests)) 10)) 2)
         for i from 1 
         for (num original name) in tests
         collect (rename-file original (format nil "~v,'0d_~a.txt" digits (* i spacing) name))))))
    

(defun txt-p (p) (string= (pathname-type p) "txt"))


(defun newtest (n &rest parser-args)
  (apply #'test-file 
         (make-pathname
          :directory '(:relative "tests")
          :name (format nil "test_~2,'0d" n)
          :type "txt")
         parser-args))


#+#:circular-dependency-on-markup-xml
(defun xmlify ()
    (let ((tests (remove-if-not #'txt-p (list-directory "."))))
      (loop for test in tests
           do (com.gigamonkeys.markup.xml::render test))))