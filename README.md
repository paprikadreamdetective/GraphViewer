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
# Manual de Instalación y Ejecución del Proyecto

Este documento proporciona los pasos necesarios para instalar y ejecutar el proyecto de visualización de algoritmos de búsqueda en grafos utilizando **React**, **Flask** y **Common Lisp** en un sistema operativo **Windows**.

---

## Requisitos Previos

Antes de comenzar, asegúrate de que tu sistema tenga lo siguiente:

### 1. Node.js y npm
- Descarga e instala Node.js desde [Node.js](https://nodejs.org/).
- Verifica la instalación:
  ```bash
  node -v
  npm -v
  ```

### 2. Python
- Descarga e instala Python 3.9 o superior desde [Python.org](https://www.python.org/downloads/).
- Durante la instalación, asegúrate de seleccionar la opción **"Add Python to PATH"**.
- Verifica la instalación:
  ```bash
  python --version
  pip --version
  ```

### 3. Common Lisp (SBCL)
- Descarga e instala Steel Bank Common Lisp (SBCL) desde [SBCL](http://www.sbcl.org/platform-table.html).
- Añade la ruta de SBCL a las variables de entorno del sistema para poder ejecutar el comando `sbcl` desde cualquier ubicación.
- Verifica la instalación:
  ```bash
  sbcl --version
  ```

### 4. Git (opcional, para clonar el repositorio)
- Descarga e instala Git desde [Git for Windows](https://git-scm.com/download/win).

---

## Instalación del Proyecto

### 1. Clonar el Repositorio (opcional)
Si tienes acceso al repositorio remoto, puedes clonarlo:
```bash
git clone https://github.com/tu-usuario/tu-repositorio.git
cd tu-repositorio
```

Si no, coloca los archivos del proyecto en una carpeta de tu elección.

### 2. Instalación del Frontend (React)

#### Paso 1: Navegar al directorio del frontend
```bash
cd frontend
```

#### Paso 2: Instalar dependencias
```bash
npm install
```

#### Paso 3: Ejecutar la aplicación React
```bash
npm start
```
Esto abrirá una ventana del navegador con la interfaz de usuario del proyecto.

### 3. Instalación del Backend (Flask)

#### Paso 1: Crear un entorno virtual (opcional, recomendado)
```bash
python -m venv venv
```
Activa el entorno virtual:
```bash
venv\Scripts\activate
```

#### Paso 2: Instalar dependencias de Python
```bash
pip install -r requirements.txt
```
El archivo `requirements.txt` debe contener las siguientes dependencias:
```
Flask
Flask-Cors
```

#### Paso 3: Ejecutar la aplicación Flask
En el directorio del backend:
```bash
python app.py
```
Esto iniciará el servidor en `http://127.0.0.1:5000/`.

---

## Configuración y Ejecución del Proyecto

1. Asegúrate de que el frontend y el backend estén ejecutándose correctamente.
2. Interactúa con la aplicación desde el navegador para enviar grafos y ejecutar los algoritmos.

---

## Problemas Comunes

### Error: "sbcl no reconocido como comando"
Solución: Asegúrate de que SBCL está agregado al PATH del sistema.

### Error de dependencias de Python
Solución: Ejecuta `pip install -r requirements.txt` nuevamente o instala manualmente cada paquete.

### Puertos en uso
Solución: Detén cualquier proceso que esté usando los puertos 3000 (React) o 5000 (Flask).

---

Con estos pasos, deberías tener tu proyecto completamente configurado y ejecutándose en tu máquina Windows.




