;; Inicializar la semilla aleatoria con el tiempo actual
(setf *random-state* (make-random-state t))

;(defun random-element (list)
;  "Selecciona un elemento aleatorio de una lista."
;  (nth (random (length list)) list))


(defun shuffle (list)
  "Baraja una lista utilizando un algoritmo simple de Fisher-Yates."
  (let ((vec (coerce list 'vector)))
    (loop for i from (1- (length vec)) downto 1
          do (rotatef (aref vec i)
                      (aref vec (random (1+ i) *random-state*))))
    (coerce vec 'list)))

(defun extend (path)
  (print (reverse path)) ; imprime el camino actual
  (let ((neighbors (get (first path) 'neighbors)))
    (mapcar 
     #'(lambda (new-node) (cons new-node path)) ; Crea un nuevo camino
     (remove-if #'(lambda (neighbor) (member neighbor path)) ; Evita ciclos
                (shuffle neighbors))))) ; Baraja los vecinos antes de procesarlos


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
                               (format nil "~a: (~{~A~^ ~})~%" nodo (get nodo 'neighbors)))))
    (guardar-en-txt "grafo_bfs_random.txt" grafo)))

(defun guardar-caminos (camino)
  "Guarda los caminos recorridos en un archivo TXT."
  (with-open-file (stream "rutas_recorridas_bfs_random.txt"
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

;(setf (get 's 'neighbors) '(a d)
;      (get 'a 'neighbors) '(s b d)
;      (get 'b 'neighbors) '(a c e)
;      (get 'c 'neighbors) '(b)
;      (get 'd 'neighbors) '(s a e)
;      (get 'e 'neighbors) '(b d f)
;      (get 'f 'neighbors) '(e))

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


;; Limpiar el archivo de rutas antes de ejecutarlo
(with-open-file (stream "rutas_recorridas_bfs_random.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream "")) ; Deja el archivo vacío
;; Ejecutar una búsqueda de profundidad entre A y E

(print (breadth-first-search 'a 's))


;; Probar `shuffle` para verificar que es realmente aleatorio

(defvar LISTA '(a b c d e))

(print (shuffle '(a b c d e)))
(print (random-element LISTA))