
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
  "Genera un archivo con la representacion del grafo."
  (let ((grafo ""))
    (dolist (nodo '(A B C D E F G H I J K L M N O P Q R S))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: ~a~%"  ;;in ~a~%"
                                       nodo
                                       (get nodo 'neighbors)
                                       (get nodo 'coordinates)))))
    (guardar-en-txt "grafo.txt" grafo) ))

(defun guardar-caminos-y-costos (camino costo)
  "Guarda los caminos recorridos y el costo acumulado en un archivo TXT.
  Sobrescribe el archivo en cada ejecucion."
  (with-open-file (stream "caminos.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: ~a cost: ~a~%" camino costo)))

(defun best-first-search-con-archivos (start finish &optional (queue (list (list start))))
  "Realiza una busqueda Best-First y genera los archivos correspondientes."
  (generar-archivo-grafo)
  (cond
    ((endp queue) nil) ;; Si la cola esta vacia
    ((eq finish (first (first queue))) 
     (let ((path (reverse (first queue)))
           (cost (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair)) (pairwise (reverse (first queue)))))))
       (guardar-caminos-y-costos path cost)
       path)) ;; Si se encuentra el nodo objetivo
    (t
     (let* ((current-path (first queue))
            (cost (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair)) (pairwise (reverse current-path)))))
            (extended-paths (extend current-path))
            (sorted-paths (sort extended-paths
                                #'(lambda (p1 p2)
                                    (< (distancia-recta (first p1) finish)
                                       (distancia-recta (first p2) finish))))))
        (guardar-caminos-y-costos (reverse current-path) cost)
       (best-first-search-con-archivos start finish (append (rest queue) sorted-paths))))))

;; Configurar el grafo con vecinos y coordenadas
(setf (get 'A 'neighbors) '(B C D E))
(setf (get 'B 'neighbors) '(A C F G I))
(setf (get 'C 'neighbors) '(A B D F))
(setf (get 'D 'neighbors) '(A C E F G))
(setf (get 'E 'neighbors) '(A D G H))
(setf (get 'F 'neighbors) '(B C D G J L))
(setf (get 'G 'neighbors) '(B D E F H I J K))
(setf (get 'H 'neighbors) '(E G K))
(setf (get 'I 'neighbors) '(B G J L M))
(setf (get 'J 'neighbors) '(F G I K M N O))
(setf (get 'K 'neighbors) '(G H J O R))
(setf (get 'L 'neighbors) '(F I M P Q))
(setf (get 'M 'neighbors) '(I J L N P))
(setf (get 'N 'neighbors) '(J M O P Q))
(setf (get 'O 'neighbors) '(J K N Q R))
(setf (get 'P 'neighbors) '(L M N Q S))
(setf (get 'Q 'neighbors) '(L N O P R S))
(setf (get 'R 'neighbors) '(K O Q S))
(setf (get 'S 'neighbors) '(P Q R))


(setf (get 'a 'coordinates) '(1 5)
      (get 'b 'coordinates) '(3 8)
      (get 'c 'coordinates) '(3 6)
      (get 'd 'coordinates) '(3 4)
      (get 'e 'coordinates) '(3 2)
      (get 'f 'coordinates) '(6 8)
      (get 'g 'coordinates) '(6 5)
      (get 'h 'coordinates) '(6 2)
      (get 'i 'coordinates) '(9 7)
      (get 'j 'coordinates) '(9 4)
      (get 'k 'coordinates) '(9 1)
      (get 'l 'coordinates) '(12 9)
      (get 'm 'coordinates) '(12 7)
      (get 'n 'coordinates) '(12 5)
      (get 'o 'coordinates) '(12 3)
      (get 'p 'coordinates) '(15 7)
      (get 'q 'coordinates) '(15 5)
      (get 'r 'coordinates) '(15 3)
      (get 's 'coordinates) '(18 6)
      )


(with-open-file (stream "caminos.txt"
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "")) 

;; Busqueda Best-First con salida en archivos
(best-first-search-con-archivos 'A 'S)
