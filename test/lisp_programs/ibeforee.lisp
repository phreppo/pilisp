(defun string-include (string1 string2)
  (cond
   ((zerop (length string1)) nil) ; string1 is empty (no need to test it every time)
   ((> (length string1) (length string2)) nil) ; string1 is longer than string2
   ((eq string1 (subseq string2 0 (length string1))) string1) ; string2 starts with string1
   (t (string-include string1 (subseq string2 1))))) ; otherwise shorten string2 by 1 and start over