(:BODY
 (:P "This is a regular paragraph."
     (:COMMENT (:P "This is a comment"))
     (:NOTE (:P "This is a note")))
 
 (:P "This is a paragraph."
     (:NOTE (:P "This is a note." (:COMMENT (:P "With a comment in it"))))))
