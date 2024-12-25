(defun extend (path)
    (print (reverse path)) ; imprime camino
    (mapcar 
        #'(lambda (new-node) (cons new-node path)) ; Crea un nuevo camino
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors)) ; filtra los elementos
    )
) 
;; condicion impuesta por la
;; funcion lambda
(defun depth-first (start finish &optional (queue (list (list start))))
    (cond 
        ((endp queue) nil) ; cola vacia?
        ((eq finish (first (first queue))) (reverse (first queue))) 
        (t (depth-first start finish (append (extend (first queue)) (rest queue)))) ;; Brinca camino extendido
    )
) 

(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))


;; Ejecutar una búsqueda de profundidad entre A y E
(print (depth-first 's 'f))
(print '())
(print (breadth-first 's 'f))