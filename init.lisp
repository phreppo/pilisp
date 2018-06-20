;( (nullo . 1) (n1 . 1) (n2 . 2) (n3 . 3) (s1 . "hi") (s2 . "hello") (s3 . "hey") (id . (lambda (x) x)) (plusone . (lambda (x) (+ x 1))) (ff . (lambda (x) (cond ((atom x) x ) (t (ff (car x)))))) )

;( (t . t) )