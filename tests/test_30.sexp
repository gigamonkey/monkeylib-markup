(:BODY
 (:P (:I "Paragraph starting with italic.") " Not italic.")

 (:P "This is a regular paragraph."
  (:NOTE (:P (:I "Note starting with italic.") " Not italic."))
  " Rest of paragraph."))