
    (defun initialize-graph (graph start)
        "Inicializa los vertices del grafo para BFS."
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
                    (let ((u (car (last path))))  ; Ultimo nodo del camino actual
                        ;; Escribe el camino actual en el archivo
                        (format stream "path: ~a~%" path)
                        ;; Si alcanzamos el nodo objetivo, registramos el camino y detenemos
                        (when (eq u goal)
                        ;(format stream "path: ~a~%" path)
                        (setf goal-reached t))
                        ;; Explora los vecinos del nodo actual si no se alcanzo el objetivo
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

    (bfs-with-all-movements '(a b c d e f g h i j k l m n o p q r s) 'B "movements.txt" 'K)

    