
(defun extend (path)
    (mapcar
        #'(lambda (new-node) (cons new-node path))
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors))
    ))

(defun depth-first-search (start finish &optional (queue (list (list start))))
    (cond
        ((endp queue) nil)
        ((eq finish (first (first queue))) (reverse (first queue)))
        (t (depth-first-search start finish
                               (append (extend (first queue)) (rest queue))))))

(setf (get 'A 'neighbors) '(B C D E))
(setf (get 'B 'neighbors) '(A C E F))
(setf (get 'C 'neighbors) '(D E F G H I A))
(setf (get 'D 'neighbors) '(I H E A))

(print (depth-first-search 'A 'i))
