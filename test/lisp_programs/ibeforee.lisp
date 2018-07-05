; CHALLANGE TOOK FROM: https://www.reddit.com/r/dailyprogrammer/comments/8q96da/20180611_challenge_363_easy_i_before_e_except/
; **********************************************
; *           I before E except after C
; **********************************************
; Background
; "I before E except after C" is perhaps the most famous English spelling rule. For the purpose of this ; challenge, the rule says:
; 
; if "ei" appears in a word, it must immediately follow "c".
; If "ie" appears in a word, it must not immediately follow "c".
; 
; A word also follows the rule if neither "ei" nor "ie" appears anywhere in the word. Examples of words that ; follow this rule are:
; ====================== 
; fiery hierarchy hieroglyphic
; ceiling inconceivable receipt
; daily programmer one two three
; ====================== 
;
; There are many exceptions that don't follow this rule, such as:
; ====================== 
; sleigh stein fahrenheit
; deifies either nuclei reimburse
; ancient juicier societies
; ====================== 
;
; Challenge
; Write a function that tells you whether or not a given word follows the "I before E except after C" rule.
; 
; check("a") => true
; check("zombie") => true
; check("transceiver") => true
; check("veil") => false
; check("icier") => false


(defun string-include (string1 string2)
  (cond
   ((eq (length string1) 0) nil)
   ((> (length string1) (length string2)) nil) 
   ((eq string1 (subseq string2 0 (length string1))) string1) 
   (t (string-include string1 (subseq string2 1)))))
  
(defun check
  (word)
  (cond
    ((string-include "cie" word) NIL)
    ((and (string-include "ei" word)
         (not (string-include "cei" word))) NIL)
    (t t)))