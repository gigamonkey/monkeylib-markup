(:BODY
 (:P "This is a regular paragraph. With a second line.")

 (:BLOCKQUOTE
  (:P "This is a block paragraph."))

 (:P "This is a regular paragraph"
     (:NOTE
      (:P "This is a note.")
      (:P "With it's own paragraphs."))
  " And the end of the last regular paragraph."))