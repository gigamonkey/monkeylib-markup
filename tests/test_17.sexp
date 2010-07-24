(:BODY
 (:P "This is a paragraph" (:NOTE (:P "This is an inline note.")) " blah blah blah.")

 (:P "This is another paragraph."
     (:NOTE
      (:P "This is a note.")
      (:P "That has its own paragraphs.")
      (:P "And another."))
     " And this is the rest of the second main paragraph.")

 (:P "And this is yet another paragraph."
     (:NOTE
      (:P "This is a note.")
      (:P "That has its own paragraphs. With " (:I "formatting foo") " in them.")
      (:P "And another."))
     " And this is the rest of the third main paragraph.")

 (:P "And this is a fourth paragraph."
     (:NOTE
      (:P "This is a note.")
      (:P "That has its own paragraphs.")
      (:OL
       (:LI (:P "A a list in the note."))
       (:LI (:P "With a few items.")))
      (:P "And another paragraph."))
     " And this is the rest of the fourth main paragraph.")

 (:P "And this is a fifth paragraph."
     (:NOTE
      (:P "This is a note.")
      (:P "That has its own paragraphs.")
      (:OL
       (:LI (:P "A a list in the note."))
       (:LI (:P "With a few items."))
       (:LI (:P "That end the note"))))
     " And this is the rest of the fifth main paragraph.")

 (:P "And this is a sixth paragraph."
     (:NOTE
      (:P "This is a note.")
      (:P "That has its own paragraphs.")
      (:OL
       (:LI (:P "A a list in the note."))
       (:LI (:P "With a few items."))
       (:LI (:P "That end the note"))))
     " And this is the rest of the sixth main paragraph."))