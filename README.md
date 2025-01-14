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

Los grafos son representados como listas de adyacencia, como una estructura tipo diccionario, donde cada key es un nodo y sus valores correspondientes son una lista de vecinos.  
**Estructura en Common Lisp:**  
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
