def generate_lisp_code_bfs(adjacency_list, start_node, end_node):
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )
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