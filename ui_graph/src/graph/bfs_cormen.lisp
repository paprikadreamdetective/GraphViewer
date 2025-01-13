(defun initialize-graph (graph start)
  "Inicializa los vértices del grafo para BFS."
  (mapc (lambda (node)
          (setf (get node 'color) 'WHITE
                (get node 'distance) 'infinity
                (get node 'predecessor) nil))
        graph)
  (setf (get start 'color) 'GRAY
        (get start 'distance) 0
        (get start 'predecessor) nil))

(defun bfs-with-all-movements (graph start output-file goal)
  "Implementa BFS y escribe todos los movimientos realizados hasta encontrar el nodo objetivo."
  (initialize-graph graph start)
  (let ((queue (list (list start)))  ; La cola ahora contiene listas de caminos
        (goal-reached nil))         ; Bandera para detener cuando se alcance el objetivo
    (with-open-file (stream output-file
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (loop while (and queue (not goal-reached)) do
            (let ((path (pop queue)))
              (let ((u (car (last path))))  ; Último nodo del camino actual
                ;; Escribe el camino actual en el archivo
                (format stream "path: ~a~%" path)
                ;; Si alcanzamos el nodo objetivo, registramos el camino y detenemos
                (when (eq u goal)
                  ;(format stream "path: ~a~%" path)
                  (setf goal-reached t))
                ;; Explora los vecinos del nodo actual si no se alcanzó el objetivo
                (unless goal-reached
                  (dolist (v (get u 'neighbors))
                    (when (eq (get v 'color) 'WHITE)
                      ;; Marca el vecino como visitado y añade el nuevo camino a la cola
                      (setf (get v 'color) 'GRAY
                            (get v 'distance) (+ 1 (get u 'distance))
                            (get v 'predecessor) u)
                      (push (append path (list v)) queue)))
                  ;; Marca el nodo como procesado
                  (setf (get u 'color) 'BLACK))))))))

;; Definimos las conexiones del grafo.
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
      (get 's 'neighbors) '(p q r))

;; Ejecutamos BFS desde el nodo 'a' hasta 's' y guardamos todos los movimientos en el archivo.
(bfs-with-all-movements '(a b c d e f g h i j k l m n o p q r s) 'a "movements.txt" 's)




