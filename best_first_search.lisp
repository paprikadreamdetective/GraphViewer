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
    (dolist (nodo '(a b c d e f g h i j k l m n o p q r s))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: ~a~%"  ;;in ~a~%"
                                       nodo
                                       (get nodo 'neighbors)
                                       (get nodo 'coordinates)))))
    (guardar-en-txt "grafo.txt" grafo) ))

(defun guardar-caminos-y-costos (camino costo)
  "Guarda los caminos recorridos y el costo acumulado en un archivo TXT.
  Sobrescribe el archivo en cada ejecución."
  (with-open-file (stream "caminos.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: ~a cost: ~a~%" camino costo)))

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
            (cost (reduce #'+ (mapcar #'(lambda (pair) (apply #'distancia-recta pair)) (pairwise (reverse current-path)))))
            (extended-paths (extend current-path))
            (sorted-paths (sort extended-paths
                                #'(lambda (p1 p2)
                                    (< (distancia-recta (first p1) finish)
                                       (distancia-recta (first p2) finish))))))
        (guardar-caminos-y-costos (reverse current-path) cost)
       (best-first-search-con-archivos start finish (append (rest queue) sorted-paths))))))

;; Construyendo el grafo con vecinos y coordenadas

(setf (get 'a 'neighbors) '(b c d e)
      (get 'b 'neighbors) '(a c g f)
      (get 'c 'neighbors) '(a d f b)
      (get 'd 'neighbors) '(a e g f c)
      (get 'e 'neighbors) '(a h g d)
      (get 'f 'neighbors) '(b c d g j l)
      (get 'g 'neighbors) '(b d e h k j i f)
      (get 'h 'neighbors) '(e k g)
      (get 'i 'neighbors) '(b g j m l)
      (get 'j 'neighbors) '(f g k o n m i)
      (get 'k 'neighbors) '(h r o j g)
      (get 'l 'neighbors) '(f i m q p)
      (get 'm 'neighbors) '(i j n p l)
      (get 'n 'neighbors) '(j o q p m)
      (get 'o 'neighbors) '(j k r q n)
      (get 'p 'neighbors) '(l m n q s)
      (get 'q 'neighbors) '(l n o r s p)
      (get 'r 'neighbors) '(o k s q)
      (get 's 'neighbors) '(p q r)
      )

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
    (format stream "")) ;; Deja el archivo vacío
;; Ejecutar la búsqueda y generar los archivos
(best-first-search-con-archivos 'a 's)


















