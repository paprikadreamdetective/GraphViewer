# Grafo de Caminos y Rutas Óptimas: Visualización con React

Este proyecto combina **Common Lisp** y **React** para generar y visualizar grafos que representan caminos y rutas óptimas. Los caminos se calculan utilizando tres algoritmos: **DFS (Depth First Search)**, **BFS (Breadth First Search)** y **Best First Search**. El proyecto genera los datos en un archivo de texto y los visualiza dinámicamente con el componente `reagraph` en React.

---

## 📋 Descripción del Proyecto

El flujo del proyecto consta de dos partes principales:
1. **Generación de Datos con Common Lisp:**
   - Los algoritmos DFS, BFS y Best First Search se ejecutan para explorar un grafo, encontrar los caminos y calcular la mejor ruta.
   - Los resultados (grafo, caminos y ruta óptima) se exportan a un archivo `.txt`.

2. **Visualización con React:**
   - El archivo generado por los scripts de Lisp se importa en la aplicación React.
   - La biblioteca `reagraph` se utiliza para renderizar el grafo y visualizar los caminos y rutas generados.

Este proyecto permite explorar y analizar visualmente los caminos recorridos por diferentes algoritmos de búsqueda.

---

## 🚀 Características

- Implementación de tres algoritmos de búsqueda (DFS, BFS y Best First Search) en **Common Lisp**.
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

