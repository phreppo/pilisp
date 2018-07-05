(defun string-include (string1 string2)
  (cond
   ((eq (length string1) 0) nil)
   ((> (length string1) (length string2)) nil) 
   ((eq string1 (subseq string2 0 (length string1))) string1) 
   (t (string-include string1 (subseq string2 1)))))