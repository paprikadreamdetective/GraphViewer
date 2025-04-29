def generate_lisp_code_dfs(adjacency_list, start_node, end_node):
    print(adjacency_list)
    neighbors_code = "\n".join(
        f"(setf (get '{node} 'neighbors) '({' '.join(neighbors)}))" for node, neighbors in adjacency_list.items()
    )
    print(neighbors_code)

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