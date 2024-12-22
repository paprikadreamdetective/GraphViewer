(defun distancia-recta (node1 node2)
  (let* ((coord1 (get node1 'coordinates))
         (coord2 (get node2 'coordinates))
         (dx (- (first coord1) (first coord2)))
         (dy (- (second coord1) (second coord2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun pairwise (lst)
  "Genera pares consecutivos de elementos en la lista para calcular distancias."
  (mapcar #'list lst (rest lst)))

(defun extend (path)
  "Extiende los caminos considerando vecinos no visitados."
  (mapcar #'(lambda (new-node) (cons new-node path))
          (remove-if #'(lambda (neighbor) (member neighbor path))
                     (get (first path) 'neighbors))))

(defun guardar-en-txt (nombre-archivo contenido)
  "Guarda el contenido en un archivo TXT, sobrescribiendo si ya existe."
  (with-open-file (stream nombre-archivo
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~a" contenido)))

(defun generar-archivo-grafo ()
  "Genera un archivo con la representación del grafo."
  (let ((grafo ""))
    (dolist (nodo '(s a b c d e f g))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: ~a in ~a~%"
                                       nodo
                                       (get nodo 'neighbors)
                                       (get nodo 'coordinates)))))
    (guardar-en-txt "grafo.txt" grafo)))

(defun guardar-caminos-y-costos (camino costo)
  "Guarda los caminos recorridos y el costo acumulado en un archivo TXT."
  (let ((contenido (format nil "path: ~a cost: ~a~%"
                           camino costo)))
    (guardar-en-txt "caminos.txt" contenido)))

(defun best-first-search-con-archivos (start finish &optional (queue (list (list start))))
  "Realiza una búsqueda Best-First y genera los archivos correspondientes."
  (generar-archivo-grafo)
  (cond
    ((endp queue) nil) ;; Si la cola está vacía
    ((eq finish (first (first queue))) 
     (let ((path (reverse (first queue)))
           (cost (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair)) (pairwise (reverse (first queue)))))))
       (guardar-caminos-y-costos path cost)
       path)) ;; Si se encuentra el nodo objetivo
    (t
     (let* ((current-path (first queue))
            (extended-paths (extend current-path))
            (sorted-paths (sort extended-paths
                                #'(lambda (p1 p2)
                                    (< (distancia-recta (first p1) finish)
                                       (distancia-recta (first p2) finish))))))
       (best-first-search-con-archivos start finish (append (rest queue) sorted-paths))))))

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

;; Ejecutar la búsqueda y generar los archivos
(best-first-search-con-archivos 's 'f)


















