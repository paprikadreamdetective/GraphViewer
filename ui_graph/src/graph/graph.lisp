
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
(setf (get 'B 'neighbors) '(A C G F))
(setf (get 'C 'neighbors) '(A D F B))
(setf (get 'D 'neighbors) '(A E G F C))
(setf (get 'E 'neighbors) '(A H G D))
(setf (get 'F 'neighbors) '(B C D G J L))
(setf (get 'G 'neighbors) '(B D E H K J I F))
(setf (get 'H 'neighbors) '(E K G))
(setf (get 'I 'neighbors) '(B G J M L))
(setf (get 'J 'neighbors) '(F G K O N M I))
(setf (get 'K 'neighbors) '(H R O J G))
(setf (get 'L 'neighbors) '(F I M Q P))
(setf (get 'M 'neighbors) '(I J N P L))
(setf (get 'N 'neighbors) '(J O Q P M))
(setf (get 'O 'neighbors) '(J K R Q N))
(setf (get 'P 'neighbors) '(L M N Q S))
(setf (get 'Q 'neighbors) '(L N O R S P))
(setf (get 'R 'neighbors) '(O K S Q))
(setf (get 'S 'neighbors) '(P Q R))

(print (depth-first-search 'A 'S))
