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

    if not adjacency_list or not start_node or not end_node:
        return jsonify({"error": "Invalid input"}), 400

    try:
        # Crear archivo Lisp con la representación del grafo
        lisp_code = generate_lisp_code(adjacency_list, start_node, end_node)
        with open("graph.lisp", "w") as f:
            f.write(lisp_code)

        # Ejecutar código Lisp
        result = subprocess.run(["sbcl", "--script", "graph.lisp"], capture_output=True, text=True)
        print(result)
        if result.returncode != 0:
            return jsonify({"error": "Error executing Lisp code", "details": result.stderr}), 500

        
        return jsonify({"output": result.stdout})
    except Exception as e:
        return jsonify({"error": str(e)}), 500


def generate_lisp_code(adjacency_list, start_node, end_node):
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


if __name__ == '__main__':
    app.run(debug=True)
