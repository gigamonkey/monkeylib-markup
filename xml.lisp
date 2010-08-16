(in-package :com.gigamonkeys.markup3.xml)

(define-xml-language markup-xml 
  (:block-elements :body :blockquote :ol :ul :li :link_def)
  (:paragraph-elements :p :pre :h1 :h2 :h3 :h4 :h5 :h6 :h7 :h8 :h9))

(defun render (file)
  (let ((sexps (parse-file file :parse-links-p t)))
    (with-output-to-file (out (make-pathname :type "xml" :defaults file))
      (with-foo-output (out)
        (com.gigamonkeys.foo.xml::emit-for-language 'markup-xml sexps)))))

  