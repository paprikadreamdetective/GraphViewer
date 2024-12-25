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
    (guardar-en-txt "grafo_dfs.txt" grafo)))
;; condicion impuesta por la

(defun guardar-caminos (camino)
  "Guarda los caminos recorridos en un archivo TXT."
  (with-open-file (stream "rutas_recorridas.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: (~{~A~^ ~})~%" camino)))
;; funcion lambda
;(defun depth-first (start finish &optional (queue (list (list start))))
;    (cond 
;        ((endp queue) nil) ; cola vacia?
;        ((eq finish (first (first queue))) (reverse (first queue))) 
;        (t (depth-first start finish (append (extend (first queue)) (rest queue)))) ;; Brinca camino extendido
;    )
;)

(defun depth-first-search (start finish &optional (queue (list (list start))))
  "Algoritmo DFS que genera los archivos correspondientes."
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
       (depth-first-search start finish
                                 (append (extend current-path) (rest queue)))))))

(setf (get 's 'neighbors) '(a d)
      (get 'a 'neighbors) '(s b d)
      (get 'b 'neighbors) '(a c e)
      (get 'c 'neighbors) '(b)
      (get 'd 'neighbors) '(s a e)
      (get 'e 'neighbors) '(b d f)
      (get 'f 'neighbors) '(e))

;; Limpiar el archivo de rutas antes de ejecutarlo
(with-open-file (stream "rutas_recorridas.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream "")) ; Deja el archivo vacío
;; Ejecutar una búsqueda de profundidad entre A y E
(print (depth-first-search 's 'f))

