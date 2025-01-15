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

2. **Depth First Search (DFS).**  
Estratégia de búsqueda en profundidad en un gráfo
Implementación en pseudocódigo:  
```

```

3. **Best First Search.**  
Método heurístico  
Implementación en pseudocódigo:  
```

```

4. **Random Breadth First Search (r-BFS).**  
Variante de BFS que sigue el mismo principio de búsqueda sistemática en amplitud, pero introduce una componente aleatoria al proceso de selección de los vecinos a explorar. En lugar de seguir un orden fijo, mezcla aleatorialmente el orden de aparición de los vecinos antes de añadirlos a la cola.
Implementación en pseudocódigo:  
```

```

5. **BFS de Cormen.**  
Consiste en implementar el algoritmo **Breadth First Search** propuesto en el libro [_Introduction to Algorithms_](https://dl.ebooksworld.ir/books/Introduction.to.Algorithms.4th.Leiserson.Stein.Rivest.Cormen.MIT.Press.9780262046305.EBooksWorld.ir.pdf) de Thomas H. Cormen.  
Sigue un enfoque sistemático mediante el uso de un método de coloreado de los nodos:  
- Blancos = no visitados,  
- Grises = en la cola,  
- Negros = completamente visitados.  
Inicialmente, todos los nodos, a excepción del nodo raíz, se marcan como blancos, se les asigna una distancia inicial de 1 y sus predecesores se establecen en NIL. El nodo raíz se marca como gris, su distancia se establece en 0 y su predecesor en NIL. Posteriormente, se crea una cola Q, que inicialmente contiene solo el nodo raíz. Deben registrarse las distancias desde el nodo inicial y el nodo predecesor para cada nodo.  
El algoritmo es de carácter iterativo y continúa su ejecución mientras existan vértices grises (en la frontera de exploración). En cada iteración, se extrae un vértice gris de la cola y se procesan sus vecinos. Una vez que todos los vecinos de un nodo han sido explorado, este se marca como negro (completamente explorado). El algoritmo garantiza que cada vértice alcanzable se descubra una sola vez y que las distancias calculadas sean las mínimas posibles. El árbol de búsqueda en amplitud generado puede variar dependiendo del orden en que se visiten los vecinos. 
```

```


## **Notas:**  
Para el correcto desempeño del programa, cada archivo .lisp contiene, además de la implementación del algoritmo indicado, una serie de funciones auxiliares para la correcta generación del grafo, los caminos y la ruta seguida del nodo inicial al objetivo.
I. Función de  

---
## Descripción del funcionamiento de la interfaz 



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






