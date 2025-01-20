from flask import Flask, request, jsonify
from flask_cors import CORS

import subprocess
import os

app = Flask(__name__)
CORS(app)
# Ruta para recibir el grafo y nodos
@app.route('/process-graph', methods=['POST'])
def process_graph():
    data = request.get_json()
    print(data)
    adjacency_list = data.get('adjacency_list')
    start_node = data.get('start_node')
    end_node = data.get('end_node')
    algorithm = data.get('algorithm')

    for key in adjacency_list:
        adjacency_list[key] = sorted(adjacency_list[key])

    adjacency_list = dict(sorted(adjacency_list.items())) 
    print("ORDENADOS")
    print(adjacency_list)
    if not adjacency_list or not start_node or not end_node:
        return jsonify({"error": "Invalid input"}), 400

    try:
        # Crear archivo Lisp con la representación del grafo
        #lisp_code = generate_lisp_code(adjacency_list, start_node, end_node)
        lisp_code = str()
        if algorithm == "defgraph":
            lisp_code = lisp_callback_def_graph(adjacency_list)
        elif algorithm == "Depth-First-Search":
            lisp_code = generate_lisp_code_dfs(adjacency_list, start_node, end_node)
        elif algorithm == "Breadth-First-Search":
            lisp_code = generate_lisp_code_bfs(adjacency_list, start_node, end_node)
        elif algorithm == "Best-First-Search":
            lisp_code = generate_lisp_code_best_first_search(adjacency_list, start_node, end_node)
        elif algorithm == "Random-Breadth-First-Search":
            lisp_code = generate_lisp_code_random_bfs(adjacency_list, start_node, end_node)
        elif algorithm == "Cormen-Breadth-First-Search":
            lisp_code = generate_lisp_code_bfs_cormen(adjacency_list, start_node, end_node)
        
        
        with open("graph.lisp", "w") as f:
            f.write(lisp_code)
        # Ejecutar código Lisp
        result = subprocess.run(["sbcl", "--script", "graph.lisp"], capture_output=True, text=True)
        print(result)
        if result.returncode != 0:
            print("error aqui")
            return jsonify({"error": "Error executing Lisp code", "details": result.stderr}), 500
        print(result.stdout)
        # return jsonify({"output": result.stdout})
        return jsonify({"output": "Update Graph", "is_computed":True})
    except Exception as e:
        print("error aqui")
        return jsonify({"error": str(e)}), 500

def lisp_callback_def_graph(adjacency_list):
    print(adjacency_list)
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )


    lisp_code=f"""
    

    {neighbors_code}




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
    (dolist (nodo '({' '.join(adjacency_list.keys())}))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: (~{{~A~^ ~}})~%" nodo (get nodo 'neighbors)))))
    (guardar-en-txt "grafo_dfs.txt" grafo)))



    (generar-archivo-grafo)

    """
    return lisp_code


def generate_lisp_code_dfs(adjacency_list, start_node, end_node):
    print(adjacency_list)
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )
    print(neighbors_code)
    '''
    lisp_template = f"""
(defun extend (path)
    (mapcar
        #'(lambda (new-node) (cons new-node path))
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors))
    ))

(defun depth-first-search (start finish &optional (queue (list (list start))))
    (cond
        ((endp queue) nil)
        ((eq finish (first (first queue))) (reverse (first queue)))
        (t (depth-first-search start finish
                               (append (extend (first queue)) (rest queue))))))

{neighbors_code}

(print (depth-first-search '{start_node} '{end_node}))
"""

    '''

    lisp_template = f"""
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
    (dolist (nodo '({' '.join(adjacency_list.keys())}))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: (~{{~A~^ ~}})~%" nodo (get nodo 'neighbors)))))
    (guardar-en-txt "grafo_dfs.txt" grafo)))

(defun guardar-caminos (camino)
  "Guarda los caminos recorridos en un archivo TXT."
  (with-open-file (stream "rutas_recorridas_dfs.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: (~{{~A~^ ~}})~%" camino)))

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

{neighbors_code}

(with-open-file (stream "rutas_recorridas_dfs.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream ""))

(print (depth-first-search '{start_node} '{end_node}))
"""
    
    return lisp_template



def generate_lisp_code_bfs(adjacency_list, start_node, end_node):
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )
    

    '''
    lisp_template = f"""
(defun extend (path)
    (mapcar
        #'(lambda (new-node) (cons new-node path))
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors))
    ))

(defun depth-first-search (start finish &optional (queue (list (list start))))
    (cond
        ((endp queue) nil)
        ((eq finish (first (first queue))) (reverse (first queue)))
        (t (depth-first-search start finish
                               (append (extend (first queue)) (rest queue))))))

{neighbors_code}

(print (depth-first-search '{start_node} '{end_node}))
"""

    '''

    lisp_template = f"""
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
    (dolist (nodo '({' '.join(adjacency_list.keys())}))
      (setf grafo (concatenate 'string grafo
                               (format nil "~a: (~{{~A~^ ~}})~%" nodo (get nodo 'neighbors)))))
    (guardar-en-txt "grafo_bfs.txt" grafo)))

(defun guardar-caminos (camino)
  "Guarda los caminos recorridos en un archivo TXT."
  (with-open-file (stream "rutas_recorridas_bfs.txt"
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (format stream "path: (~{{~A~^ ~}})~%" camino)))

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

{neighbors_code}

(with-open-file (stream "rutas_recorridas_bfs.txt"
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
  (format stream ""))

(breadth-first-search '{start_node} '{end_node})
"""
    
    return lisp_template

def generate_lisp_code_best_first_search(adjacency_list, start_node, end_node):
    
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )

    lisp_template = f"""
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
    (dolist (nodo '({' '.join(adjacency_list.keys())}))
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
{neighbors_code}


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
(best-first-search-con-archivos '{start_node} '{end_node})
"""
    return lisp_template



def generate_lisp_code_random_bfs(adjacency_list, start_node, end_node):
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )

    lisp_template = f"""
        (setf *random-state* (make-random-state t))

        (defun distancia-recta (node1 node2)
        (let* ((coord1 (get node1 'coordinates))
                (coord2 (get node2 'coordinates))
                (dx (- (first coord1) (first coord2)))
                (dy (- (second coord1) (second coord2))))
            (sqrt (+ (* dx dx) (* dy dy)))))

        (defun shuffle (list)
            "Baraja una lista utilizando un algoritmo simple de Fisher-Yates."
            (let ((vec (coerce list 'vector)))
            (loop for i from (1- (length vec)) downto 1
                do (rotatef (aref vec i)
                            (aref vec (random (1+ i) *random-state*))))
            (coerce vec 'list)))

        (defun extend (path)
            (print (reverse path)) 
            (let ((neighbors (get (first path) 'neighbors)))
            (mapcar 
                #'(lambda (new-node) (cons new-node path)) 
                (remove-if #'(lambda (neighbor) (member neighbor path)) 
                (shuffle neighbors))))) 

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
                (dolist (nodo '({' '.join(adjacency_list.keys())}))
                (setf grafo (concatenate 'string grafo
                                        (format nil "~a: (~{{~A~^ ~}})~%" nodo (get nodo 'neighbors)))))
                (guardar-en-txt "grafo_bfs_random.txt" grafo)))
            
        (defun guardar-caminos (camino)
            "Guarda los caminos recorridos en un archivo TXT."
            (with-open-file (stream "rutas_recorridas_bfs_random.txt"
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
                (format stream "path: (~{{~A~^ ~}})~%" camino)))

        (defun breadth-first-search (start finish &optional (queue (list (list start))))
        "Algoritmo BFS que genera los archivos correspondientes."
        (generar-archivo-grafo) ; Generar archivo del grafo
        (cond
            ((endp queue) nil) ; Si la cola esta vacia
            ((eq finish (first (first queue))) 
            (let ((path (reverse (first queue))))
            (guardar-caminos path) ; Guardar el camino final
            path))
            (t
            (let ((current-path (first queue)))
            (guardar-caminos (reverse current-path)) 
            (breadth-first-search start finish
                                                (append (rest queue) (extend current-path)))))))

        {neighbors_code}    

        (with-open-file (stream "rutas_recorridas_bfs_random.txt"
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
        (format stream "")) ; Deja el archivo vacio
        
        (breadth-first-search '{start_node} '{end_node})
    """
    return lisp_template

def generate_lisp_code_bfs_cormen(adjacency_list, start_node, end_node):
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )

    lisp_template = f"""
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
    
    {neighbors_code}

    (bfs-with-all-movements '(a b c d e f g h i j k l m n o p q r s) '{start_node} "movements.txt" '{end_node})

    """
    return lisp_template

if __name__ == '__main__':
    app.run(debug=True)
