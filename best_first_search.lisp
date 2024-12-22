

(defun distancia-recta (node1 node2)
  (let* ((coord1 (get node1 'coordinates))
         (coord2 (get node2 'coordinates))
         (dx (- (first coord1) (first coord2)))
         (dy (- (second coord1) (second coord2))))
    (sqrt (+ (* dx dx) (* dy dy)))))


(defun camino-mas-cercano-p (path1 path2)
  (< (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair))
                         (pairwise path1)))
     (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair))
                         (pairwise path2)))))

(defun pairwise (lst)
;;  "Genera pares consecutivos de elementos en la lista para calcular distancias."
  (mapcar #'list lst (rest lst)))


(defun best-first-search (start finish &optional (queue (list (list start))))
  (cond
    ((endp queue) nil) ;; Si la cola está vacía
    ((eq finish (first (first queue))) 
     (let ((path (reverse (first queue))))
       (format t "Camino encontrado: ~a~%" path)
       (format t "Costo del camino: ~a~%" (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair)) (pairwise path))))
       path)) ;; Si se encuentra el nodo objetivo
    (t
     (let* ((current-path (first queue))
            (extended-paths (extend current-path))
            (sorted-paths (sort extended-paths
                                #'(lambda (p1 p2)
                                    (< (distancia-recta (first p1) finish)
                                       (distancia-recta (first p2) finish))))))
       (format t "Explorando camino: ~a~%" (reverse (first queue)))
       (format t "Costo acumulado: ~a~%" (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair)) (pairwise (reverse (first queue))))))
       (best-first-search start finish (append (rest queue) sorted-paths))))))

;; Extendiendo caminos como en DFS y BFS
(defun extend (path)
  (mapcar #'(lambda (new-node) (cons new-node path))
          (remove-if #'(lambda (neighbor) (member neighbor path))
                     (get (first path) 'neighbors))))


;; Construyendo el grafo con vecinos y coordenadas
(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b g)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e g)
      (get 'g 'neighbors) '(c f))

(setf (get 's 'coordinates) '(0 3)
      (get 'a 'coordinates) '(4 6)
      (get 'b 'coordinates) '(7 6)
      (get 'c 'coordinates) '(11 6)
      (get 'd 'coordinates) '(3 0)
      (get 'e 'coordinates) '(6 0)
      (get 'f 'coordinates) '(11 3)
      (get 'g 'coordinates) '(14 6))

(format t "Best-First Search desde 's' hasta 'f': ~a~%" (best-first-search 's 'f))

