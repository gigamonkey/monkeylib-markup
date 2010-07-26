;;
;; Copyright (c) 2010, Peter Seibel. All rights reserved.
;;

(in-package com.gigamonkeys.markup3)

(declaim (optimize (debug 3)))

(defparameter *spaces-per-tab* 8)

(defparameter *blank* (format nil "~c~c" #\Newline #\Newline))

(defclass parser ()
  ((bindings :initform () :accessor bindings)
   (elements :initform () :accessor elements)
   (current-indentation :initform 0 :accessor current-indentation)
   (subdocument-tags :initform '(:note :comment) :accessor subdocument-tags)))

(defclass element ()
  ((tag :initarg :tag :accessor tag)
   (current-child-cons :initform nil :accessor current-child-cons)
   (children :initform nil :accessor children)))

(defmethod print-object ((object element) stream)
  (print-unreadable-object (object stream)
    (format stream "tag: ~a" (tag object))))

(defclass indentation ()
  ((spaces :initarg :spaces :accessor spaces)))

(defmethod print-object ((object indentation) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~a" (spaces object))))

;;
;; Our main macro.
;; 

(defmacro with-bindings ((parser token) &body bindings)
  (with-gensyms (frame-marker)
    `(let ((,frame-marker (open-frame ,parser)))
       (flet ((pop-frame ()
                (close-frame ,parser ,frame-marker))
              (pop-frame-and-element (element)
                (close-frame ,parser ,frame-marker)
                (close-element ,parser element)))
         (declare (ignorable (function pop-frame) (function pop-frame-and-element)))
         ,@(loop for (key . body) in (reverse bindings) collect
                `(push-binding 
                  ,parser 
                  ,(etypecase key
                              (character key)
                              (symbol key)
                              (string key)
                              (cons `(lambda (,token) (declare (ignorable ,token)) ,key)))
                  (lambda (,token) (declare (ignorable ,token)) ,@body)))))))

(defun indentation-p (x) (typep x 'indentation))

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

(defun close-frame (parser frame-marker)
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

(defun open-element (parser tag)
  (with-slots (elements) parser
    (let ((parent (first elements))
          (element (make-instance 'element :tag (intern (string-upcase tag) :keyword))))
      (when parent (append-child parent element))
      (push element elements)
      element)))

(defun close-element (parser element)
  (with-slots (elements) parser
    (let ((tail (member element elements)))
      (unless tail (error "~a is not in elements (~a)." element elements))
      (setf elements (cdr tail)))
    element))

(defun parse-file (file)
  (let* ((parser (make-instance 'parser))
         (translator (make-basic-translator-chain (lambda (tok) (process-token parser tok))))
         (body (open-document parser)))
    (with-open-file (in file)
      (loop for c = (read-char in nil nil) while c do (funcall translator c)))
    (funcall translator #\Newline)
    (funcall translator #\Newline)
    (funcall translator :eof)
    (to-sexp body)))

(defun process-token (parser token)
  (funcall (find-binding parser token) token))

(defun open-document (parser)
  (let ((body (open-element parser "body")))
    (with-bindings (parser token)
      (#\* (open-header-handler parser))

      (#\- (open-possible-modeline-handler parser))

      ((or (text-char-p token) (eql token #\\))
       (open-paragraph parser "p")
       (process-token parser token))

      ((and (indentation-p token) (>= (spaces token) (+ (current-indentation parser) 4)))
       (incf (current-indentation parser) 4)
       (open-verbatim parser (- (spaces token) (current-indentation parser)) "pre"))
  
      ((and (indentation-p token) (= (spaces token) (+ (current-indentation parser) 2)))
       (incf (current-indentation parser) 2)
       (open-section parser (spaces token) "blockquote"))
  
      ((and (indentation-p token) (= (spaces token) (current-indentation parser))))
    
      (:blank)
      (:eof (pop-frame-and-element body)))
    body))

(defun open-paragraph (parser tag)
   (let ((paragraph (open-element parser tag)))
     (with-bindings (parser token)
       (#\\ (open-slash-handler parser))
       (#\Newline (add-text parser #\Space))
       ((text-char-p token) (add-text parser token))
       (:blank (pop-frame-and-element paragraph)))))

(defun open-block (parser tag)
  (when (string= tag "note")
    (break "elements: ~s; bindings: ~s" (elements parser) (bindings parser)))
  (let ((element (open-element parser tag)))
    (with-bindings (parser token)
      (#\} (pop-frame-and-element element)))))

(defun open-subdocument (parser tag)
  (let ((element (open-element parser tag))
        (original-indentation (current-indentation parser)))
    (with-bindings (parser token)
      (:eof (error "Subdocument ~a not closed." tag))

      (#\} 
       (setf (current-indentation parser) original-indentation)
       (pop-frame-and-element element))

      ;; We need these two binding that seemingly duplicate the ones
      ;; in open-document because the open-paragraph binding for
      ;; text-chars will shadow the open-document ones.
      (#\* (open-header-handler parser))
      
      ((or (text-char-p token) (eql token #\\))
       (open-paragraph parser "p")
       (process-token parser token)))))

(defun open-section (parser indentation tag)
  (let ((section (open-element parser tag)))
    (with-bindings (parser token)
      ("#-"
       (setf (tag section) (case token (#\# :ol) (#\- :ul)))
       (open-list parser token indentation)
       (process-token parser token))
      ((and (indentation-p token) (< (spaces token) indentation))
       (setf (current-indentation parser) (spaces token))
       (pop-frame-and-element section)
       (process-token parser token)))))

(defun open-list (parser list-marker indentation)
  (with-bindings (parser token)
    ((and (indentation-p token) (< (spaces token) indentation))
     (setf (current-indentation parser) (spaces token))
     (pop-frame)
     (process-token parser token))
    ((eql token list-marker)
     (with-bindings (parser token)
       (#\Space 
        (pop-frame)
        (setf (current-indentation parser) (+ indentation 2))
        (open-list-item parser list-marker (+ indentation 2)))
       (t (illegal-token token))))))

(defun open-list-item (parser list-marker indentation)
  (let ((item (open-element parser "li")))
    (with-bindings (parser token)
      ((and (indentation-p token) (< (spaces token) indentation))
       (setf (current-indentation parser) (spaces token))
       (pop-frame-and-element item)
       (process-token parser token))
      ((eql list-marker token)
       (pop-frame-and-element item)
       (process-token parser token)))))

(defun open-verbatim (parser extra-indentation tag)
  (let ((verbatim (open-element parser tag))
        (blanks 0)
        (bol t))

    (with-bindings (parser token)
      (#\Newline 
       (add-text parser token)
       (setf bol t))

      (:blank 
       (incf blanks)
       (setf bol t))

      ((characterp token)
       (when bol
         (loop repeat blanks do (add-text parser *blank*))
         (setf blanks 0)
         (loop repeat extra-indentation do (add-text parser #\Space))
         (setf bol nil))
       (add-text parser token))

      ((and (indentation-p token) (>= (spaces token) (current-indentation parser)))
       (setf extra-indentation (- (spaces token) (current-indentation parser))))

      ((and (indentation-p token) (< (spaces token) (current-indentation parser)))
       (setf (current-indentation parser) (spaces token))
       (pop-frame-and-element verbatim)
       (process-token parser token)))))

(defun open-header-handler (parser)
  (let ((level 1))
    (with-bindings (parser token)
      (#\* (incf level))
      (#\Space 
       (pop-frame)
       (open-paragraph parser (format nil "h~d" level)))
      (t (illegal-token token)))))

(defun open-possible-modeline-handler (parser)
  (let ((so-far (make-text-buffer "-")))
    (with-bindings (parser token)
      ((and (eql token #\*) (string= so-far "-"))
       (append-text so-far token))
      ((and (eql token #\-) (string= so-far "-*"))
       (append-text so-far token))
      ((string= so-far "-*-")
       (when (eql token :blank) (pop-frame)))
      (t
       (append-text so-far token)
       (pop-frame)
       (open-paragraph parser "p")
       (loop for c across so-far do (process-token parser c))))))

(defun open-slash-handler (parser)
  (with-bindings (parser token)
    ((tag-name-char-p token)
     (pop-frame)
     (open-tag-name-handler parser token))
    ("\\{}*[]#-"
     (pop-frame)
     (add-text parser token))
    (t (illegal-token token))))

(defun open-tag-name-handler (parser token)
  (let ((name (make-text-buffer token)))
    (with-bindings (parser token)
      (#\{
       (unless (plusp (length name))
         (error "Empty names not allowed."))
       (pop-frame)
       (if (find name (subdocument-tags parser) :test #'string-equal)
           (open-subdocument parser name)
           (open-block parser name)))
      ((tag-name-char-p token)
       (append-text name token)))))

(defun text-char-p (char)
  "Characters that can appear unescaped in non-verbatim sections."
  (and (characterp char) (not (find char "\\[]{}"))))

(defun tag-name-char-p (char)
  "Characters that can appear in tag names (i.e. between a '\' and a '{')."
  (and (characterp char) (alphanumericp char)))

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
        (spaces-seen 0))
    (lambda (char)
      (cond
        ((and in-indentation (eql char #\Space))
         (incf spaces-seen))
        ((or (eql char #\Newline) (eql char :blank))
         (setf spaces-seen 0)
         (setf in-indentation t)
         (funcall next char))
        (t
         (when in-indentation
           (funcall next (make-instance 'indentation :spaces spaces-seen))
           (setf in-indentation nil))
         (funcall next char))))))

(defun make-basic-translator-chain (end)
  (make-tab-translator 
   (make-eol-translator
    (make-trailing-space-translator
     (make-blank-translator
      (make-indentation-translator end))))))

