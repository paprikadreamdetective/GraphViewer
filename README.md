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
La siguiente estructura define un nodo y su lista de vecinos en Common Lisp:  
```(setf (get 'node 'neighbors) '(b c d)) ```
      
1. **Breadth First Search (BFS).**

2. **Depth First Search (DFS).**  
Estratégia de búsqueda en profundidad en un gráfo

3. **Best First Search.**  
Heurística


4. **Random Breadth First Search (r-BFS).**  
Sigue el mismo principio que BFS, pero introduce una componente aleatorio al...

5. **BFS de Cormen**

## **Notas:**  
Para el correcto desempeño del programa, cada archivo .lisp contiene, además de la implementación del algoritmo indicado, una serie de funciones auxiliares para la correcta generación del grafo, los caminos y la ruta seguida del nodo inicial al objetivo.

---
## Descripción del funcionamiento de la interfaz 

---
# Manual de usuario
