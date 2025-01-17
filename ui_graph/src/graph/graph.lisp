
(defun extend (path)
    (print (reverse path))
    (mapcar
        #'(lambda (new-node) (cons new-node path))
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors))
    ))

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
                               (format nil "~a: (~{~A~^ ~})~%" nodo (get nodo 'neighbors)))))
    (guardar-en-txt "grafo_dfs.txt" grafo)))

(defun guardar-caminos (camino)
  "Guarda los caminos recorridos en un archivo TXT."
  (with-open-file (stream "rutas_recorridas_dfs.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: (~{~A~^ ~})~%" camino)))

(defun depth-first-search (start finish &optional (queue (list (list start))))
  "Algoritmo DFS que genera los archivos correspondientes."
  (generar-archivo-grafo)
  (cond
    ((endp queue) nil)
    ((eq finish (first (first queue)))
     (let ((path (reverse (first queue))))
       (guardar-caminos path)
       path))
    (t
     (let ((current-path (first queue)))
       (guardar-caminos (reverse current-path))
       (depth-first-search start finish
                           (append (extend current-path) (rest queue)))))))

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

(with-open-file (stream "rutas_recorridas_dfs.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream ""))

(print (depth-first-search 'A 'S))
