import './GraphArea.css';
//import graph from '../graph/grafo.txt'
import React, { useState, useEffect } from 'react';
import { darkTheme, GraphCanvas, directionalLight } from 'reagraph';

const GraphArea = ({ title, graph, visitedPaths, delayAnimation}) => {
  
  const [graphData, setGraphData] = useState({ nodes: [], edges: [] });
  const [animationPath, setAnimationPath] = useState([]); // Guarda la secuencia de nodos
  const [selectedNodes, setSelectedNodes] = useState([]); // Nodo seleccionado actualmente
  const [isAnimationReady, setIsAnimationReady] = useState(false); // Controla el estado del botón de animación

  const handleFileUpload = async (event) => {
    const file = event.target.files[0];
    if (file) {
      const text = await file.text();
      const parsedGraph = parseGraphData(text);
      setGraphData(parsedGraph);
    }
  };

  const handleFilePathsUpload = async (event) => {
    const file = event.target.files[0];
    //const file = visitedPaths;
    if (file) {
      const text = await file.text();
      if (text.includes('path:')) {
        const parsedPaths = parsePathFile(text);
        setAnimationPath(parsedPaths); // Almacena rutas para animación
        setIsAnimationReady(true); // Habilitar botón de animación
      } else {
        const parsedGraph = parseGraphData(text);
        setGraphData(parsedGraph);
      }
    }
  };

   // Función para parsear el archivo de rutas
   const parsePathFile = (text) => {
    const lines = text.trim().split('\n');
    const paths = lines.map((line) => {
      const match = line.match(/path:\s*\(([^)]+)\)/);
      return match ? match[1].split(' ') : [];
    });
    return paths;
  };

  const parseGraphData = (text) => {
    const nodesSet = new Set();
    const edgesSet = new Set();
    const processedEdges = new Set();
    const lines = text.trim().split('\n');
    console.log("Archivo de entrada procesado línea por línea:");
    lines.forEach((line) => {
      console.log(line); // Imprimir cada línea del archivo
      const [node, neighbors] = line.split(':');
      const nodeId = node.trim();
      const neighborsList = neighbors.replace(/[()]/g, '').trim().split(' ');
      // Add the node
      nodesSet.add(nodeId);
      // Add edges (only from the first occurrence)
      neighborsList.forEach((neighbor) => {
        const neighborId = neighbor.trim();
        const edge = [nodeId, neighborId].sort().join('-'); // Sort to maintain consistency
  
        if (!processedEdges.has(edge)) {
          edgesSet.add(edge);
          processedEdges.add(edge);
        }
      });
    });
    console.log("Nodos finales:", Array.from(nodesSet));
    console.log("Aristas finales:", Array.from(edgesSet));
    // Convert Sets to arrays with unique keys
    const nodes = Array.from(nodesSet).map((id) => ({ id, label: id, key: id }));
    const edges = Array.from(edgesSet).map((edge) => {
      const [source, target] = edge.split('-');
      return { id: `${source}-${target}`, source, target, key: `${source}-${target}` };
    });
    return { nodes, edges };
  };
  
  // Función para iniciar la animación
  const startAnimation = () => {
    if (animationPath.length > 0) {
      let index = 0;
      const animate = () => {
         // Detener la animación si se reseteó
        if (index < animationPath.length) {
          setSelectedNodes(animationPath[index]); // Actualiza nodos seleccionados
          index++;
          //setTimeout(animate, 250); // Pausa de 250 milisegundos
          setTimeout(animate, delayAnimation);
          
        } else {
          if (animationPath.length > 0) {
            setSelectedNodes(animationPath[animationPath.length - 1]);
          }
        }
      };
      animate(); 
    }
  };

  const resetAnimation = () => {
    setSelectedNodes([]); 
  };

  useEffect(() => {
    // Función para leer el archivo al cambiar a la pestaña
    const loadGraphFromFile = async () => {
      try {
        const response = await fetch(graph);
        console.log(response);
        if (response.ok) {
          const text = await response.text();
          console.log(text);
          const parsedGraph = parseGraphData(text);
          setGraphData(parsedGraph);
        } else {
          console.error('No se pudo leer el archivo:', response.statusText);
        }
      } catch (error) {
        console.error('Error al cargar el archivo:', error);
      }
    };

    const loadPathsFromFile = async () => {
      /*try {
        const response = await fetch(visitedPaths);
        console.log(response);
        if (response.ok) {
          const text = await response.text();
          console.log(text);
          const parsedGraph = parseGraphData(text);
          setGraphData(parsedGraph);
        } else {
          console.error('No se pudo leer el archivo:', response.statusText);
        }
      } catch (error) {
        console.error('Error al cargar el archivo:', error);
      }
    };*/

    //const file = event.target.files[0];
    //const file = visitedPaths;
    const file_path = await fetch(visitedPaths);
    if (file_path) {
      const text = await file_path.text();
      if (text.includes('path:')) {
        const parsedPaths = parsePathFile(text);
        setAnimationPath(parsedPaths); // Almacena rutas para animación
        setIsAnimationReady(true); // Habilitar botón de animación
      } else {
        const parsedGraph = parseGraphData(text);
        setGraphData(parsedGraph);
      }
    }
  }


    loadGraphFromFile();
    loadPathsFromFile();
  }, [graph, visitedPaths]); // Se ejecuta cada vez que FILE_PATH cambie o se renderice el componente.

  return (
    <div className="graph-area">
      <h3>{title}</h3>
      {/*<input type="file" accept=".txt" onChange={handleFileUpload} />*/}
      {/*<input type="file" accept=".txt" onChange={handleFilePathsUpload} />*/}
      <button onClick={startAnimation} disabled={!isAnimationReady}> 
        Iniciar Animación
      </button>
      <button onClick={resetAnimation}> 
        Reset
      </button>
        <div className="graph-canvas">
          <GraphCanvas
            draggable
            nodes={graphData.nodes}
            edges={graphData.edges}
            theme={darkTheme}
            layoutType="forceDirected3d"
            selections={selectedNodes} // Resaltar nodos seleccionados
            sizingType="none"
            edgeArrowPosition="none" // No arrows for undirected graph
            cameraMode="rotate"
          >
            <directionalLight position={[0, -4, 5]} intensity={1} />
          </GraphCanvas>
      </div>
    </div>
  );
};

export default GraphArea;



