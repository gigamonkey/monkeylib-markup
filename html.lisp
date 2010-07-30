(in-package :com.gigamonkeys.markup3.html)

(defvar *tag-mappings*
  '((:book . :i)))

(defun render (file &key title stylesheet)
  (let ((sexps (parse-file file)))
    (with-output-to-file (out (make-pathname :type "html" :defaults file))
      (with-foo-output (out)
        (emit-xhtml
         (rewrite-sexps sexps (or title (guess-title sexps)) stylesheet))))))

(defun guess-title (sexps)
  (let ((possible-h1 (second sexps)))
    (cond
      ((and possible-h1 (eql (first possible-h1) :h1))
       (just-text possible-h1))
      (t "Title"))))

(defun just-text (sexp)
  (with-output-to-string (s)
    (labels ((walker (x)
               (typecase x
                 (string (write-string x s))
                 (cons (mapcar #'walker x)))))
      (walker sexp))))

(defun rewrite-sexps (sexps title stylesheet) 
  `(:html
     (:head
      (:title ,title)
      (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
      ,@(if stylesheet `((:link :rel "stylesheet" :href ,stylesheet :type "text/css")))
     ,(remap-tags (fix-notes sexps)))))

(defun remap-tags (sexp)
  (labels ((walker (x)
             (cond
               ((stringp x) x)
               ((symbolp x) 
                (let ((cons (assoc x *tag-mappings*)))
                  (cond
                    (cons (cdr cons))
                    (t x))))
               (t (mapcar #'walker x)))))
    (walker sexp)))
  

(defun fix-notes (sexp)
  (let ((note-num 0)
        (notes ()))

    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :note)
                  (push x notes)
                  (let ((num (incf note-num)))
                    `(:a :name ,(format nil "noteref_~d" num) (:a :href ,(format nil "#note_~d" num) (:sup ,(princ-to-string num))))))
                 (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))

      (let ((walked (walker sexp)))
        `(,@walked 
          ((:div :class "notes")
           ,@(loop for num from 1 
                for note in (nreverse notes)
                collect 
                  (destructuring-bind (notetag (ptag . prest) . nrest) note
                    (declare (ignore notetag))
                    `((:div :class "note")
                      (,ptag
                       (:a :name ,(format nil "note_~d" num) (:a :href ,(format nil "#noteref_~d" num) (:sup ,(princ-to-string num))))
                       ,@prest)
                      ,@nrest)))))))))