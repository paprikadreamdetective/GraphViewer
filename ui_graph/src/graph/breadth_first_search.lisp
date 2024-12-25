(defun extend (path)
    (print (reverse path)) ; imprime camino
    (mapcar 
        #'(lambda (new-node) (cons new-node path)) ; Crea un nuevo camino
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors)) ; filtra los elementos
    )
) 

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
    (dolist (nodo '(s a b c d e f))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: (~{~A~^ ~})~%" nodo (get nodo 'neighbors)))))
    (guardar-en-txt "grafo_bfs.txt" grafo)))

(defun guardar-caminos (camino)
  "Guarda los caminos recorridos en un archivo TXT."
  (with-open-file (stream "rutas_recorridas_bfs.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: (~{~A~^ ~})~%" camino)))

;(defun breadth-first-search (start finish &optional (queue (list (list start))))
;    (cond 
;        ((endp queue) nil)     ; cola vacia?
;        ((eq finish (first (first queue))) (reverse (first queue))) ; encontramos finish? Entonces devuelve el camino hallado
;        (t (breadth-first-search start finish (append (rest queue) (extend (first queue))))) ; Brinca camino extendido y agrega nuevos caminos al final
;    )
;)

(defun breadth-first-search (start finish &optional (queue (list (list start))))
  "Algoritmo BFS que genera los archivos correspondientes."
  (generar-archivo-grafo) ; Generar archivo del grafo
  (cond
    ((endp queue) nil) ; Si la cola está vacía
    ((eq finish (first (first queue))) 
     (let ((path (reverse (first queue))))
       (guardar-caminos path) ; Guardar el camino final
       path))
    (t
     (let ((current-path (first queue)))
       (guardar-caminos (reverse current-path)) ; Guardar cada camino recorrido
       (breadth-first-search start finish
                                          (append (rest queue) (extend current-path)))))))

(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

;; Limpiar el archivo de rutas antes de ejecutarlo
(with-open-file (stream "rutas_recorridas_bfs.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream "")) ; Deja el archivo vacío
;; Ejecutar una búsqueda de profundidad entre A y E

(print (breadth-first-search 's 'f))