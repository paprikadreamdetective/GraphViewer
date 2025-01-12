(defun initialize-graph (graph start)
  "Inicializa los vértices del grafo para BFS"
  (mapc (lambda (node)
          (setf (get node 'color) 'WHITE
                (get node 'distance) 'infinity
                (get node 'predecessor) nil))
        graph)
  (setf (get start 'color) 'GRAY
        (get start 'distance) 0
        (get start 'predecessor) nil))

(defun bfs (graph start)
  "Implementa el algoritmo BFS según el libro de Cormen."
  (initialize-graph graph start)
  (let ((queue (list start)))
    (loop while queue do
          (let ((u (pop queue)))
            (dolist (v (get u 'neighbors))
              (when (eq (get v 'color) 'WHITE)
                (setf (get v 'color) 'GRAY
                      (get v 'distance) (+ 1 (get u 'distance))
                      (get v 'predecessor) u)
                (push v queue)))
            (setf (get u 'color) 'BLACK)))))

(defun get-path (start goal)
  "Devuelve el camino desde el nodo inicial hasta el nodo objetivo como una lista."
  (if (eq start goal)
      (list start)
      (if (not (get goal 'predecessor))
          nil ; No hay camino disponible
          (append (get-path start (get goal 'predecessor)) (list goal)))))

;; Definimos las conexiones del grafo.
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

;; Ejecutamos BFS desde el nodo 's'
(bfs '(a b c d e f g h i j k l m n o p q r s) 'a)

;; Obtenemos la ruta de 's' a 'f'
(let ((path (get-path 'a 's)))
  (if path
      (format t "Ruta encontrada: ~a~%" path)
      (format t "No hay camino entre los nodos dados.~%")))


