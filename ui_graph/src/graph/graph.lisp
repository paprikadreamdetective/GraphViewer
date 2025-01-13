
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

(with-open-file (stream "rutas_recorridas_dfs.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream ""))

(print (depth-first-search 'A 'S))
