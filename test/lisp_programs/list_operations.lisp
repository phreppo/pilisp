(defun list_max_rec (l index act_max)
    (cond
        ((>= index (length l)) 
            act_max)
        ((> (nth index l) act_max) 
            (list_max_rec l (+ index 1) (nth index l)))
        (t
            (list_max_rec l (+ index 1) act_max))))

(defun list_max (l)
    (list_max_rec l 1 (nth 0 l)))

(defmacro get-from-list(list pred)
  `(let ((ans (first ,list)))
     (do ((i 1 (1+ i)))
         ((>= i (length ,list)) ans)
       (when (,pred (nth i ,list) ans)
         (setf ans (nth i ,list))))))

(defun list_max_rec (l index act_max pred)
    (cond
        ((>= index (length l)) 
            act_max)
        ((pred (nth index l) act_max) 
            (list_max_rec l (+ index 1) (nth index l)))
        (t
            (list_max_rec l (+ index 1) act_max))))
