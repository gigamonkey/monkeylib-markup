;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package com.gigamonkeys.markup3)

(declaim (optimize (debug 3)))

(defparameter *spaces-per-tab* 8)

(defclass parser ()
  ((bindings :initform () :accessor bindings)
   (elements :initform () :accessor elements)
   (subdocument-tags :initform '(:note :comment) :accessor subdocument-tags)))

(defclass element ()
  ((tag :initarg :tag :accessor tag)
   (current-child-cons :initform nil :accessor current-child-cons)
   (children :initform nil :accessor children)))

(defclass indentation ()
  ((spaces :initarg :spaces :accessor spaces)))

(defun make-indentation (spaces)
  (make-instance 'indentation :spaces spaces))

(defun indentation-p (x) (typep x 'indentation))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream)
    (format stream "tag: ~a" (tag object))))

(defmethod print-object ((object indentation) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~a" (spaces object))))

(defgeneric to-sexp (thing))

(defmethod to-sexp ((string string)) string)

(defmethod to-sexp ((element element))
  `(,(tag element) ,@(loop with last = (current-child-cons element)
                        for cons on (children element)
                        for child = (car cons)
                        collect (to-sexp 
                                 (if (and (eql cons last) (stringp child))
                                     (string-right-trim " " child)
                                     child)))))


(defun make-element (parent tag &rest format-args)
  (let ((e (make-instance 'element :tag (intern (string-upcase (apply #'format nil tag format-args)) :keyword))))
    (when parent (append-child parent e))
    e))

(defun append-child (element child)
  (let ((cons (current-child-cons element))
        (new-cons (cons child nil)))
    (when cons (setf (cdr cons) new-cons))
    (unless (children element)
      (setf (children element) new-cons))
    (setf (current-child-cons element) new-cons)))

(defun current-child (element)
  (car (current-child-cons element)))

(defmethod initialize-instance :after ((parser parser) &key &allow-other-keys)
  (push-binding parser t (lambda (tok) (error "No binding for ~a in ~a" tok (bindings parser)))))

(defun open-frame (parser)
  (let ((marker (gensym "FRAME-")))
    (push-binding parser marker nil)
    marker))

(defun push-binding (parser key fn)
  (with-slots (bindings) parser
    (setf bindings (acons key fn bindings))))

(defun pop-bindings (parser frame-marker)
  (with-slots (bindings) parser
    (setf bindings (cdr (member frame-marker bindings :key #'car)))))

(defun find-binding (parser token)
  "Find the first binding that could handle the given token. A default
  binding can be established with the key (constantly t)"
  (with-slots (bindings) parser
    (cdr (assoc token bindings :test #'key-match))))

(defun key-match (token key)
  (etypecase key
    ((eql t) t)
    (character (eql token key))
    (function (funcall key token))
    (string (find token key))
    (symbol (eql token key))))

(defun open-element (parser tag &rest format-args)
  (with-slots (elements) parser
    (first (push (apply #'make-element (first elements) tag format-args) elements))))

(defun close-element (parser element)
  (with-slots (elements) parser
  (unless (eql (first elements) element)
    (error "~a is not the current element (~a)." element elements))
  (pop elements)))

(defun parse-file (file)
  (let ((parser (make-instance 'parser)))
    (let ((body (open-element parser "body")))
      (install-document-bindings parser)
      (let ((translator (make-basic-translator-chain (lambda (tok) (process-token parser tok)))))
        (with-open-file (in file)
          (loop for c = (read-char in nil nil)
             while c do (funcall translator c)))
        (funcall translator #\Newline)
        (funcall translator #\Newline)
        (funcall translator :eof)
        (to-sexp (close-element parser body))))))

(defun process-token (parser token)
  (funcall (find-binding parser token) token))

(defun make-indentation-> (amount)
  (lambda (token)
    (and (indentation-p token) (> (spaces token) amount))))

(defun install-document-bindings (parser)
  (let ((frame-marker (open-frame parser)))
    (push-binding parser :eof (lambda (token) 
                                (declare (ignore token))
                                (pop-bindings parser frame-marker)))
    (push-binding parser :blank (lambda (token) (ignore-token token)))
    (push-binding parser 
                  (lambda (token)
                    (and (indentation-p token) (= (spaces token) 2)))
                  (lambda (token)
                    (open-subdocument parser (spaces token) "blockquote")))

    (push-binding parser 
                  (lambda (token)
                    (and (indentation-p token) (= (spaces token) 4)))
                  (lambda (token)
                    (open-verbatim parser (spaces token) "pre")))

    (push-binding parser #'characterp 
                  (lambda (token)
                    (open-paragraph parser "p")
                    (process-token parser token)))
    (push-binding parser #\* (make-header-handler parser))))

(defun open-paragraph (parser tag)
  (let ((frame-marker (open-frame parser))
        (paragraph (open-element parser tag)))
    (push-binding parser :blank 
                  (lambda (token)
                    (declare (ignore token))
                    (pop-bindings parser frame-marker)
                    (close-element parser paragraph)))
    (push-binding parser #'characterp (lambda (token) (add-text parser token)))
    (push-binding parser #\Newline (lambda (token)
                                     (declare (ignore token))
                                     (add-text parser #\Space)))
    (push-binding parser #\\ (make-slash-handler parser))))

(defun open-block (parser tag)
  (let ((frame-marker (open-frame parser))
        (element (open-element parser tag)))
    (push-binding parser #\}
                  (lambda (token)
                    (declare (ignore token))
                    (pop-bindings parser frame-marker)
                    (close-element parser element)))))

(defun open-subdocument (parser indentation tag)
  ;; Should really compare indentation to our own indentation and then
  ;; pass along the indentation token if it is sufficiently low.
  (declare (ignore indentation))
  (let ((frame-marker (open-frame parser))
        (subdocument (open-element parser tag)))
    (push-binding parser 
                  (lambda (token)
                    (and (indentation-p token) (= (spaces token) 0)))
                  (lambda (token)
                    (declare (ignore token))
                    (pop-bindings parser frame-marker)
                    (close-element parser subdocument)))))

(defun open-verbatim (parser indentation tag)
  ;; Should really compare indentation to our own indentation and then
  ;; pass along the indentation token if it is sufficiently low.
  (let ((frame-marker (open-frame parser))
        (verbatim (open-element parser tag))
        (current-indentation indentation))

    (push-binding parser 
                  (lambda (token)
                    (and (indentation-p token) (= (spaces token) indentation)))
                  (lambda (token)
                    (setf current-indentation (spaces token))))

    (push-binding parser 
                  (lambda (token)
                    (and (indentation-p token) (< (spaces token) indentation)))
                  (lambda (token)
                    (declare (ignore token))
                    (pop-bindings parser frame-marker)
                    (close-element parser verbatim)))

    (push-binding parser 
                  (lambda (token)
                    (and (indentation-p token) (> (spaces token) indentation)))
                  (lambda (token)
                    (setf current-indentation (spaces token))
                    (loop repeat (- (spaces token) indentation) do (add-text parser #\Space))))

    (push-binding parser #'characterp (lambda (token) (add-text parser token)))
    (push-binding parser :blank (lambda (token)
                                  (declare (ignore token))
                                  (add-text parser #\Newline)
                                  (add-text parser #\Newline)))

    (push-binding parser #\Newline (lambda (token)
                                     (declare (ignore token))
                                     (add-text parser #\Newline)
                                     (loop repeat (- current-indentation indentation) do (add-text parser #\Space))))
    ))
    

(defun make-header-handler (parser)
  (let ((level 1))
    (lambda (token)
      (declare (ignore token))
      (let ((frame-marker (open-frame parser)))
        (push-binding parser t (lambda (token) (illegal-token token)))
        (push-binding parser #\* (lambda (token) (declare (ignore token)) (incf level)))
        (push-binding parser #\Space (lambda (token)
                                       (declare (ignore token))
                                       (pop-bindings parser frame-marker)
                                       (open-paragraph parser (format nil "h~d" level))))))))

(defun make-slash-handler (parser)
  (lambda (token)
    (declare (ignore token))
    (let ((frame-marker (open-frame parser)))
      (push-binding parser "\\{}*[]" (lambda (token)
                                       (pop-bindings parser frame-marker)
                                       (add-text parser token)))
      (push-binding parser #'tag-name-char-p 
                    (let ((name (make-text-buffer)))
                      (lambda (token)
                        (push-binding parser #\{
                                      (lambda (token)
                                        (declare (ignore token))
                                        (unless (plusp (length name))
                                          (error "Empty names not allowed."))
                                        (pop-bindings parser frame-marker)
                                        (open-block parser name)))
                        (append-text name token)))))))
      


(defun tag-name-char-p (char)
  (alphanumericp char))

(defun add-text (parser text)
  (let ((element (first (elements parser))))
    (if (stringp (current-child element))
        (append-text (current-child element) text)
        (append-child element (make-text-buffer text)))))

(defun make-text-buffer (&optional text)
  (let ((s (make-array 10 :element-type 'character :adjustable t :fill-pointer 0)))
    (when text (append-text s text))
    s))

(defun append-text (string text)
  (typecase text
    (character (vector-push-extend text string))
    (string (loop for c across text do (vector-push-extend c string)))
    (t (format t "~&Appending non text text: ~a" text))))

(defun ignore-token (token) (format t "~&Ignoring ~a" token))

(defun illegal-token (token) (error "Illegal token ~a" token))






;;
;; Character translators -- cleans up input and generates blanks and indentations
;;

(defun make-tab-translator (next)
  "Translate Tab characters to *spaces-per-tab* Space characters."
  (lambda (char)
    (case char
      (#\Tab (loop repeat *spaces-per-tab* do (funcall next #\Space)))
      (t (funcall next char)))))

(defun make-eol-translator (next)
  "Translate CRLF and CR to LF"
  (let ((after-cr nil))
    (lambda (char)
      (case char
        (#\Return (setf after-cr t))
        (t (cond
             (after-cr 
              (funcall next #\Newline)
              (unless (eql char #\Newline) (funcall next char)))
             (t (funcall next char)))

           (setf after-cr nil))))))

(defun make-trailing-space-translator (next)
  (let ((spaces-seen 0))
    (lambda (char)
      (case char
        (#\Space (incf spaces-seen))
        (t (unless (eql char #\Newline)
             (loop repeat spaces-seen do (funcall next #\Space)))
           (setf spaces-seen 0)
           (funcall next char))))))

(defun make-blank-translator (next)
  "Translate more than one consecutive newlines into :blank"
  (let ((newlines-seen 0))
    (lambda (char)
      (case char
        (#\Newline (incf newlines-seen))
        (t (cond
             ((= newlines-seen 1) (funcall next #\Newline))
             ((> newlines-seen 1) (funcall next :blank)))

           (setf newlines-seen 0)
           (funcall next char))))))

(defun make-indentation-translator (next)
  (let ((in-indentation t)
        (spaces-seen 0)
        (current-indentation 0))
    (lambda (char)
      (cond
        ((and in-indentation (eql char #\Space))
         (incf spaces-seen))
        ((or (eql char #\Newline) (eql char :blank))
         (setf spaces-seen 0)
         (setf in-indentation t)
         (funcall next char))
        (t
         (when (and in-indentation (not (eql char :eof)))
           (when (/= spaces-seen current-indentation)
             (funcall next (make-indentation spaces-seen))
             (setf current-indentation spaces-seen))
           (setf in-indentation nil))
         (funcall next char))))))

(defun make-basic-translator-chain (end)
  (make-tab-translator 
   (make-eol-translator
    (make-trailing-space-translator
     (make-blank-translator
      (make-indentation-translator end))))))

;;
;; Unit tests
;;


(defparameter *expected-failures* ())
(defparameter *to-skip* ())

(defun test-number (txt)
  (parse-integer (subseq (pathname-name txt) 5)))

(defun tests ()
  (loop for file in (directory "./tests/test_*.txt")
     for n = (test-number file)
     for ok = 
       (cond
         ((member n *to-skip*)
          (format t "~&Skipping test ~d." n)
          t)
         (t
          (or (test-file file) (member n *expected-failures*))))
     while ok))


(defun test (n)
  (test-file 
   (make-pathname
    :directory '(:relative "tests")
    :name (format nil "test_~2,'0d" n)
    :type "txt")))

(defun test-file (txt)
  (let* ((sexp (make-pathname :type "sexp" :defaults txt))
         (parsed (parse-file txt))
         (expected (file->sexp sexp))
         (ok (equal parsed expected))
         (n (test-number txt)))
    (if ok
        (format t "~&[~d] okay." n)
        (format t "~&[~d] FAIL:~2&Got:~2&~s~2&Expected:~2&~s" n parsed expected))
    ok))

(defun file->sexp (file)
  (with-open-file (in file)
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