(in-package :com.gigamonkeys.markup3.html)

(defparameter *amazon-link* "http://www.amazon.com/gp/product/~a?ie=UTF8&tag=gigamonkeys-20&linkCode=as2&camp=1789&creative=9325&creativeASIN=~:*~a")

(defparameter *amazon-image-bug* "http://www.assoc-amazon.com/e/ir?t=gigamonkeys-20&l=as2&o=1&a=~a")

(defparameter *asins*
  (progn
    (let ((ht (make-hash-table :test #'equal)))
      (loop for (k v) in 
           '(("Peopleware" "0932633439")
             ("Practical Common Lisp" "1590592395")
             ("Rapid Development" "1556159005")
             ("Software Estimation" "0735605351")
             ("Software Estimation: Demystifying the Black Art" "0735605351")
             ("The Mythical Man Month" "0201835959")
             ("The Wisdom of Crowds" "0385503865")
             ("Founders at Work" "1590597141")
             ("Programmers at Work" "0914845713")
             ("Coders at Work" "1430219483" )
             ("Land of Lisp" "1593272006")
             ("Beautiful Code" "0596510047")
             ("Test Driven Development" "0321146530")
             ("The C++ Programming Language" "0201700735")
             ("The Design and Evolution of C++" "0201543303")
             ("Pink Brain, Blue Brain" "0618393110"))
           do (setf (gethash k ht) v))
      ht)))

(defun amazon-link (sexp)
  (let* ((asin (gethash (just-text sexp) *asins*))
         (href (format nil *amazon-link* asin)))
    `((:a :href ,href) (:i ,@(rest sexp)))))

(defun amazon-image-bug (sexp)
  (let* ((asin (gethash (just-text sexp) *asins*))
         (src (format nil *amazon-image-bug* asin)))
    `(:img :src ,src :width "1" :height "1" :alt "" :style "border:none !important; margin:0px !important;")))

(defun mailto-link (sexp)
  `(:a :href ,(format nil "mailto:~a" (just-text sexp)) ,@(rest sexp)))

(defparameter *tag-mappings*
  '((:book . amazon-link)
    (:amazon-image-bug . amazon-image-bug)
    (:email . mailto-link)
    (:n . (:span :class "name"))))

(defun render (file &key title stylesheets scripts (links t))
  "Render `file' to an html file."
  (with-output-to-file (out (make-pathname :type "html" :defaults file))
    (render-to-stream
     file out
     :title title
     :stylesheets stylesheets
     :scripts scripts
     :links links)))

(defun render-to-stream (file out &key title stylesheets scripts (links t))
  "Render `file' to the stream `out'."
  (render-sexps-to-stream
   (parse-file file :parse-links-p links) out
   :title title
   :stylesheets stylesheets
   :scripts scripts))

(defun render-sexps-to-stream (sexps out &key title stylesheets scripts)
  "Render `sexps' to `out' with a header made from `title', `stylesheets', and `scripts'."
  (with-foo-output (out)
    (emit-html
     `(:html
        ,(make-head (or title (guess-title sexps)) stylesheets scripts)
        ,(rewrite-body sexps)))))

(defun make-head (title stylesheets scripts)
  `(:head
    (:title ,title)
    (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")
    ,@(loop for stylesheet in stylesheets collect
           `(:link :rel "stylesheet" :href ,stylesheet :type "text/css"))
    ,@(loop for script in scripts collect `(:script :src ,script))))

(defun render-foo (file &key (parse-links-p t) (subdocument-tags '(:note :comment)))
  (multiple-value-bind (sexps links) (extract-link-defs (parse-file file
                                                                    :parse-links-p parse-links-p
                                                                    :subdocument-tags subdocument-tags))
    (loop for x in (rest
                    (fix-comments (fix-notes (rewrite-links (remap-tags (add-amazon-image-bugs sexps)) links))))
       do (emit-html x))))

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
      ,@(if stylesheet `((:link :rel "stylesheet" :href ,stylesheet :type "text/css"))))
     ,(rewrite-body sexps)))

(defun rewrite-body (sexps) 
  (multiple-value-bind (sexps links) (extract-link-defs sexps)
    (fix-comments (fix-notes (rewrite-links (remap-tags (add-amazon-image-bugs sexps)) links)))))

(defun remap-tags (sexp)
  (labels ((walker (x)
             (cond
               ((stringp x) x)
               ((consp sexp) (remap-one-tag x #'walker)))))
    (walker sexp)))

(defun remap-one-tag (sexp walker-fn)
  (destructuring-bind (tag . content) sexp
    (let ((mapper (cdr (assoc tag *tag-mappings*))))
      (typecase mapper
        (null `(,tag ,@(mapcar walker-fn content)))
        (keyword `(,mapper ,@(mapcar walker-fn content)))
        (cons `(,mapper ,@(mapcar walker-fn content)))
        (symbol (funcall mapper sexp))))))

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
                       (:a :name ,(format nil "note_~d" num) (:a :href ,(format nil "#noteref_~d" num) (:sup ,(princ-to-string num)))) " "
                       ,@prest)
                      ,@nrest)))))))))

(defun fix-comments (sexp)
  (let ((comment-num 0)
        (comments ()))

    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :comment)
                  (push x comments)
                  (let ((num (incf comment-num)))
                    `(:a :name ,(format nil "commentref_~d" num) (:a :href ,(format nil "#comment_~d" num) :class "comment_ref" "Comment " ,(princ-to-string num)))))
                 (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))

      (let ((walked (walker sexp)))
        `(,@walked 
          ((:div :class "comments")
           ,@(loop for num from 1 
                for comment in (nreverse comments)
                collect 
                  (destructuring-bind (commenttag (ptag . prest) . nrest) comment
                    (declare (ignore commenttag))
                    `((:div :class "comment")
                      (,ptag
                       (:a :name ,(format nil "comment_~d" num) (:a :href ,(format nil "#commentref_~d" num) :class "comment_number" ,(princ-to-string num))) " "
                       ,@prest)
                      ,@nrest)))))))))

(defun add-amazon-image-bugs (sexp)
  (let ((books ()))
    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :book)
                  (push (just-text x) books)
                  x)
                 (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))

      (let ((walked (walker sexp)))
        `(,@walked ,@(mapcar (lambda (x) `(:amazon-image-bug ,x)) (nreverse books)))))))


(defun extract-link-defs (sexp)
  (let ((links (make-hash-table :test #'equalp))
        (strip (gensym)))

    (labels ((walker (x)
               (cond
                 ((stringp x) x)
                 ((symbolp x) x)
                 ((eql (car x) :link_def)
                  (destructuring-bind (link url) (rest x)
                    (setf (gethash (just-text link) links) (just-text url)))
                  strip)
                 (t `(,(car x) ,@(remove strip (mapcar #'walker (cdr x))))))))
      (values (walker sexp) links))))

(defun rewrite-links (sexp links)
  (labels ((walker (x)
             (cond
               ((stringp x) x)
               ((symbolp x) x)
               ((eql (car x) :link)
                `((:a :href ,(link-url (link-key x) links)) ,@(rest (remove-key x))))
               (t `(,(car x) ,@(mapcar #'walker (cdr x)))))))
    (walker sexp)))

(defun link-key (link)
  (just-text (or (find-if (lambda (x) (and (consp x) (eql (car x) :key))) link) link)))

(defun remove-key (link)
  (remove-if (lambda (x) (and (consp x) (eql (car x) :key))) link))

(defun link-url (key links)
  (or
   (gethash key links)
   (progn
     (warn "No link definition for ~a" key)
     "nowhere.html")))

