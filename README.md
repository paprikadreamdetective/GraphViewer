# Grafo de Caminos y Rutas Óptimas: Visualización con React

Este proyecto combina **Common Lisp** y **React** para generar y visualizar grafos que representan caminos y rutas óptimas. Los caminos se calculan implementando cinco algoritmos: **DFS (Depth First Search)**, **BFS (Breadth First Search)**, **Best First Search**, **Random Breadth First Search (r-BFS)** y **BFS de Cormen**. El proyecto incluye la generación de datos sobre los caminos que son exportados a un archivo de texto y la visualización dinámica con el componente `reagraph` en React.

---
## 📋 Descripción del Proyecto
El flujo del proyecto consta de dos partes principales:
1. **Generación de Datos con Common Lisp:**
   - Los algoritmos DFS, BFS, Best First Search, r-BFS y BFS de Cormen se ejecutan para explorar un grafo, encontrar los caminos y calcular la mejor ruta.
   - Los resultados (grafo, caminos y ruta óptima) se exportan a un archivo `.txt`.
2. **Visualización con React:**
   - El archivo generado por los scripts de Lisp se importa en la aplicación React.
   - La biblioteca `reagraph` se utiliza para renderizar el grafo y visualizar los caminos y rutas generados.
Este proyecto permite explorar y analizar visualmente los caminos recorridos por diferentes algoritmos de búsqueda.
---

## 🚀 Características

- Implementación de cinco algoritmos de búsqueda (DFS, BFS , Best-First Search, BFS random y BFS de Cormen) en **Common Lisp**.
- Generación de datos en formato legible para su visualización.
- Visualización interactiva de grafos en una aplicación React usando `reagraph`.
- Interfaz clara para explorar nodos, caminos y rutas óptimas.
---
## 🛠️ Requisitos Previos
1. **Lenguajes y Herramientas Necesarias:**
   - [Common Lisp](https://common-lisp.net/)
   - [Node.js](https://nodejs.org/) (v16 o superior recomendado)
   - [React](https://reactjs.org/)
   - [reagraph](https://www.npmjs.com/package/reagraph)

2. **Editor de Texto o IDE:**
   - Recomendado: [Visual Studio Code](https://code.visualstudio.com/) o cualquier editor que soporte Lisp y JavaScript.
---

## Descripción de los algoritmos de búsqueda - Implementación en Common Lisp

Los grafos fueron representados mediante listas de adyacencia, empleando una estructura similar a un diccionario. En esta representación, cada clave `key` corresponde a un nodo, y su valor asociado es una lista que contiene los nodos vecinos.  
   - La siguiente estructura define un nodo y su lista de vecinos en Common Lisp:  
      ```(setf (get 'node 'neighbors) '(b c d)) ```
      
1. **Breadth First Search (BFS).**  
La exploración de grafos en amplitud es uno de los algoritmos más simples para búsqueda de caminos en grafos y el arquetipo de muchos algoritmos de grafos importantes modernos.  
Partiendo de un nodo inicial o raíz, se exploran todos sus nodos vecinos adyacentes (nivel 1) antes de profundizar al siguiente nivel. Una vez que todos los vecinos cercanos se han visitado, se avanza de nivel (nivel 2 - vecinos de vecinos), y se continúa sucesivamente, lo que garantiza que los nodos más cercanos al inicial se visiten primero. Este algoritmo emplea una cola como estructura de datos, manteniendo el orden de los nodos por visitar.  
```
## Implementación en pseudocódigo:
BFS(Grafo, Nodo, Objetivo)
''' Crear un diccionario 'Visitados' para registrar los nodos visitados y de dónde se visitaron.
    Crear una cola 'Cola' (estructura FIFO).
    Inicializar 'Visitados[Nodo]' con 'NULO' (El nodo raíz no tiene predecesor).
    Agregar 'Nodo' a 'Cola'. '''
Mientras 'Cola' no esté vacía:
      m = DESENCOLAR(Cola)  # Obtener el siguiente nodo
       Si m == Objetivo:  # Si encontramos el objetivo
           Crear una lista 'Camino' vacía.
           Mientras m no sea 'NULO':
               Agregar m a 'Camino'.
               m = Visitados[m]  # Retroceder al nodo predecesor.
           Devolver 'Camino' invertido.
       Para cada Vecino en Grafo[m]:
           Si Vecino no está en 'Visitados':
               Visitados[Vecino] = m  # Registrar el predecesor.
               ENCOLAR(Cola, Vecino).
Fin Mientras
   Devolver 'NULO' si no se encuentra un camino.
```

2. **Depth First Search (DFS).**  
Estratégia de búsqueda en profundidad en un grafo, donde se recorren todos los vértices adyacentes de uno a uno. DFS explora tan lejos como sea posible a lo largo de un mismo camino antes de retroceder para explorar los subyacentes, siguiendo el principio de "profundidad antes que amplitud".  
Su funcionamiento se basa en partir de un nodo inicial o raíz, que es marcado como "visitado" para no procesarlo nuevamente. Si el nodo vecino no ha sido visitado, se realiza una llamada recursiva para ese nodo, cuando ya ha sido visitado, este es omitido. Cuando no quedan más vecinos por explorar, el algoritmo retrocede al nodo anterior, y al encontrar el objetivo se retorna el camino. Emplea una pila como estructura de datos.  
```
## Implementación en pseudocódigo:
DFS(Grafo, Nodo, Objetivo)
''' Crear un diccionario 'Visitados' para registrar los nodos visitados.
    Crear una pila 'Pila' (estructura LIFO).
    Inicializar 'Visitados[Nodo]' con 'FALSO' (El nodo raíz no ha sido visitado aún).
    Agregar 'Nodo' a 'Pila'. '''
Mientras 'Pila' no esté vacía:
      m = DESAPILAR(Pila)  # Obtener el siguiente nodo
       Si m == Objetivo:  # Si encontramos el objetivo
           Crear una lista 'Camino' vacía.
           Mientras m no sea 'NULO':
               Agregar m a 'Camino'.
               m = Visitados[m]  # Retroceder al nodo predecesor.
           Devolver 'Camino' invertido.
       Para cada Vecino en Grafo[m]:
           Si Vecino no está en 'Visitados':
               Visitados[Vecino] = m  # Registrar el predecesor.
               APILAR(Pila, Vecino).
Fin Mientras
   Devolver 'NULO' si no se encuentra un camino.
```

3. **Best First Search.**  
A diferencia de los dos algoritmos descritos previamente, el Best First Search se incluye en la categoría de **búsqueda heurística o búsqueda informada** para la exploración de caminos en grafos.
Para explorar el grafo de manera informada, se emplea una _cola de prioridad_ para almacenar los costos de los nodos que tienen el valor de función de evaluación más bajo. Por lo tanto, la implementación es una variación del BFS, pero cambiando la cola original por una cola de prioridad.
Este algoritmo emplea una heurística para elegir el mejor camino a seguir, basado en la distancia del nodo actual al nodo objetivo. La prioridad es siempre expandir el camino que parece más prometedor en términos de proximidad al objetivo. En el código implementado, la heurística utilizada fue la distancia recta (distancia euclidiana) entre el nodo actual y el nodo objetivo.
Si la cola está vacía, finaliza la búsqueda. Si el nodo objetivo se encuentra en el primer camino de la cola, se calcula el costo total del camino y se guarda en el archivo, si no, se extiende el camino actual, se calcula su costo, y se agrega a la cola después de ordenar los caminos según la heurística (distancia hasta el nodo objetivo). Finalmente, retorna el camino más corto desde el nodo de inicio hasta el nodo objetivo.  
```
## Implementación en pseudocódigo:
Función BestFirstSearch(inicio, objetivo)
    Crear una cola con el camino de inicio.
    Mientras la cola no esté vacía:
        Extraer el primer camino de la cola (camino actual).
        Si el nodo final del camino es el objetivo:
            Calcular el costo total del camino
            Guardar el camino y el costo en un archivo
            Devolver el camino.
        Si no:
            Extender el camino actual agregando los vecinos no visitados.
            Ordenar los caminos extendidos por la distancia heurística al objetivo.
            Añadir los caminos ordenados a la cola.
    Si la cola se vacía, no se encuentra solución.
    Devolver NULO.
```
Para desarrollar adecuadamente el método, se hizo uso de funciones auxiliares en **Common Lisp** que permitieron la modificación del método clásico. Estas se describen a continuación:  
   - 

4. **Random Breadth First Search (r-BFS).**  
Variante de BFS que sigue el mismo principio de búsqueda sistemática en amplitud, pero introduce una componente aleatoria al proceso de selección de los vecinos a explorar. En lugar de seguir un orden fijo, mezcla aleatorialmente el orden de aparición de los vecinos antes de añadirlos a la cola.
```
## Implementación en pseudocódigo:
r-BFS(Grafo, Nodo, Objetivo)
''' Crear un diccionario 'Visitados' para registrar los nodos visitados y de dónde se visitaron.
    Crear una cola 'Cola' (estructura FIFO).
    Inicializar 'Visitados[Nodo]' con `NULO` (Nodo raíz no tiene predecesor). '''
 Agregar 'Nodo' a 'Cola'.
 Mientras 'Cola' no esté vacía:
        m = DESENCOLAR(Cola)  # Obtener el siguiente nodo
        Si m == Objetivo:  # Si encontramos el objetivo
            Crear una lista 'Camino' vacía.
            Mientras m no sea 'NULO':
                Agregar m a 'Camino'.
                m = Visitados[m]  # Retroceder al nodo predecesor.
            Devolver 'Camino' invertido.
        Vecinos = Lista de vecinos de Grafo[m].
        Barajar Vecinos aleatoriamente.  # Introducir componente aleatoria.
        Para cada Vecino en Vecinos:
            Si Vecino no está en 'Visitados':
                Visitados[Vecino] = m  # Registrar el predecesor.
                ENCOLAR(Cola, Vecino).
Fin Mientras
   Devolver 'NULO' si no se encuentra un camino.
```
Como se observa, esta modificación requiere realizar una mezcla de los vecinos en la lista de caminos para aleatorizar la selección del siguiente nodo a visitar, lo que puede reducir el tamaño de la exploración en contraste con el método sistemático. Para este fin, se implementó una función auxiliar en **Common Lisp**  

5. **BFS de Cormen.**  
Consiste en implementar el algoritmo **Breadth First Search** propuesto en el libro [_Introduction to Algorithms_](https://dl.ebooksworld.ir/books/Introduction.to.Algorithms.4th.Leiserson.Stein.Rivest.Cormen.MIT.Press.9780262046305.EBooksWorld.ir.pdf) de Thomas H. Cormen.  
Sigue un enfoque sistemático mediante el uso de un método de coloreado de los nodos:  
   - Blancos = no visitados,  
   - Grises = en la cola,  
   - Negros = completamente visitados.
       
Inicialmente, todos los nodos, a excepción del nodo raíz, se marcan como blancos, se les asigna una distancia inicial de 1 y sus predecesores se establecen en NIL. El nodo raíz se marca como gris, su distancia se establece en 0 y su predecesor en NIL. Posteriormente, se crea una cola Q, que inicialmente contiene solo el nodo raíz. Deben registrarse las distancias desde el nodo inicial y el nodo predecesor para cada nodo.  
El algoritmo es de carácter iterativo y continúa su ejecución mientras existan vértices grises (en la frontera de exploración). En cada iteración, se extrae un vértice gris de la cola y se procesan sus vecinos. Una vez que todos los vecinos de un nodo han sido explorado, este se marca como negro (completamente explorado). El algoritmo garantiza que cada vértice alcanzable se descubra una sola vez y que las distancias calculadas sean las mínimas posibles. El árbol de búsqueda en amplitud generado puede variar dependiendo del orden en que se visiten los vecinos. 
```
## Implementación en pseudocódigo:
BFS(G, s)
    Para cada vértice u ∈ G.V - {s}:
        u.color = BLANCO
        u.d = ∞
        u.π = NIL
    s.color = GRIS
    s.d = 0
    s.π = NIL
    Q = ∅
    ENCOLAR(Q, s)
    Mientras Q ≠ ∅:
        u = DESENCOLAR(Q)
        Para cada vértice v en G.Adj[u]:
            Si v.color == BLANCO:
                v.color = GRIS
                v.d = u.d + 1
                v.π = u
                ENCOLAR(Q, v)
        u.color = NEGRO
```


## **Notas adicionales:**  
Para el correcto desempeño del programa, cada archivo .lisp contiene, además de la implementación del algoritmo indicado, una serie de funciones auxiliares para la correcta generación del grafo, los caminos y la ruta seguida del nodo inicial al objetivo.  

1. Función **_extend (path):_**  
Genera los caminos posibles a partir de un nodo, extendiendo el camino actual hacia sus vecinos.  
```
Funcion extend(path)
    Imprime el camino invertido
    Para cada nuevo nodo en los vecinos del primer nodo de path (sin repetir los nodos en path):
        Crear un nuevo camino que agregue ese nodo al principio de path
    Retornar los nuevos caminos generados
Fin Funcion
```

2. Función **_guardar-en-txt:_**  
Permite guardar información en un archivo de texto, sobrescribiendo el archivo si ya existe.    
```
Funcion guardar-en-txt(nombre-archivo, contenido)
    Abrir el archivo con el nombre 'nombre-archivo' para escritura.
    Si el archivo ya existe, sobrescribirlo; si no, crearlo.
    Escribir el 'contenido' en el archivo.
    Cerrar el archivo.
Fin Funcion
```

3. Función **_generar-archivo-grafo:_**  
Su principal objetivo es generar una lista de adyacencia con la representación textual del grafo, con el fin de guardar la estructura del grafo en un formato legible y poder analizarlo o procesarlo después.  
```
Funcion generar-archivo-grafo()
    Crear una variable vacía 'grafo' para almacenar la representación del grafo.
    Para cada nodo en la lista de nodos predefinidos:
        Obtener los vecinos de ese nodo y agregarlos a la variable 'grafo' en formato adecuado.
    Guardar el contenido de 'grafo' en un archivo de texto con nombre "grafo_algoritmo.txt".
Fin Funcion
```

4. Función **_guardar-caminos:_**  
Esta función es útil para almacenar y llevar un registro de todos los caminos explorados en el grafo, permitiendo revisar los resultados después de la ejecución del algoritmo.  
```
Funcion guardar-caminos(camino)
    Abrir el archivo "rutas_recorridas_dfs.txt" para agregar contenido al final del archivo.
    Escribir el camino dado en el archivo, con un formato adecuado.
    Cerrar el archivo.
Fin Funcion
```
---
# 📚 Proyecto de Búsqueda en Grafos

¡Bienvenido! Este proyecto permite explorar algoritmos de búsqueda en grafos usando una interfaz gráfica basada en React y un backend con Flask que ejecuta código en Common Lisp.

---

## 🛠️ Requisitos

Antes de empezar, asegúrate de tener los siguientes componentes instalados:

### 🖥️ Frontend
- [Node.js](https://nodejs.org/) (recomendado LTS)

### 🐍 Backend
- Python 3.x
- Flask
- Flask-CORS

### 🏛️ Lenguaje de Soporte
- [Steel Bank Common Lisp (SBCL)](http://www.sbcl.org/)

---

## ⚙️ Instalación

Sigue los pasos para configurar tu entorno.

### 1️⃣ Clona el Repositorio
```bash
git clone https://github.com/paprikadreamdetective/GraphViewer.git
```

### 2️⃣ Configuración del Frontend
1. Navega a la carpeta del frontend:
   ```bash
   cd GraphViewer
   cd ui_graph
   ```
2. Instala las dependencias:
   ```bash
   npm install
   ```
3. Inicia el servidor de desarrollo:
   ```bash
   npm start
   ```
   Esto abrirá la aplicación en tu navegador en `http://localhost:3000`.

### 3️⃣ Configuración del Backend
1. Asegúrate de estar en la carpeta raíz del proyecto.
    ```bash
   cd GraphViewer
   cd ui_graph
   cd src
   cd graph
   ```
3. Instala las dependencias de Python:
   ```bash
   pip install Flask Flask-CORS
   ```
4. Ejecuta el servidor:
   ```bash
   python app.py
   ```
   El servidor Flask estará disponible en `http://localhost:5000`.

### 4️⃣ Instalación de SBCL para Ejecutar Common Lisp
1. Descarga el instalador para Windows desde [SBCL Releases](http://www.sbcl.org/platform-table.html).
2. Sigue las instrucciones del instalador.
3. Agrega `sbcl` a las variables de entorno de tu sistema para ejecutarlo desde la línea de comandos.

---

## 🚀 Cómo Ejecutar el Proyecto

1. Asegúrate de tener tanto el frontend como el backend corriendo.
2. Interactúa con la interfaz gráfica para enviar grafos y seleccionar algoritmos.
3. Los resultados y archivos generados estarán disponibles en la carpeta raíz del backend.

---

## 💡 Notas
- Asegúrate de tener permisos para escribir archivos en el directorio del proyecto.
- Los algoritmos disponibles incluyen: `DFS`, `BFS` y `Best-First Search`.

---

## 🛑 Resolución de Problemas
- **Error al ejecutar SBCL:** Verifica que `sbcl` esté en las variables de entorno.
- **Dependencias faltantes:** Asegúrate de ejecutar `pip install` o `npm install` según corresponda.

---

¡Disfruta explorando algoritmos de búsqueda! 🎉






