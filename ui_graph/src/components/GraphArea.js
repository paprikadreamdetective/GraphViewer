import './GraphArea.css';
import './ControlPanel.css';
import StartEndInput from './StartEndInput';
//import graph from '../graph/grafo.txt'
import React, { useState, useEffect } from 'react';
import axios from 'axios';
import { darkTheme, GraphCanvas, directionalLight } from 'reagraph';

const GraphArea = ({ title, graph, visitedPaths, delayAnimation}) => {
  
  const [graphData, setGraphData] = useState({ nodes: [], edges: [] });
  const [animationPath, setAnimationPath] = useState([]); // Guarda la secuencia de nodos
  const [selectedNodes, setSelectedNodes] = useState([]); // Nodo seleccionado actualmente
  
  const [showPath, setShowPath] = useState(false);
  const [isAnimationReady, setIsAnimationReady] = useState(false); // Controla el estado del botón de animación
  const [isAnimating, setIsAnimating] = useState(false); // Nuevo estado
  const [isComputed, setIsComputed] = useState(false); // Nuevo estado

  const [algorithm, setAlgorithm] = useState('');
  const [startNode, setStartNode] = useState('');
  const [endNode, setEndNode] = useState('');
  const [result, setResult] = useState("");
  const [nodes, setNodes] = useState([{ node: "", neighbors: [] }]);

  const handleAlgorithmChange = (e) => setAlgorithm(e.target.value);
  const handleStartNodeChange = (e) => setStartNode(e.target.value);
  const handleEndNodeChange = (e) => setEndNode(e.target.value);

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
    
    setShowPath(false);
    setIsAnimating(true); // Deshabilita el botón
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
          setTimeout(() => {
            setShowPath(true); 
            setIsAnimating(true);
          }, 500);
        }
      };
      animate(); 
    }
  };
  /*
  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const adjacencyList = {};
      nodes.forEach((node) => {
        if (node.node) {
          adjacencyList[node.node] = node.neighbors;
        }
      });

      const response = await axios.post("http://localhost:5000/process-graph", {
        adjacency_list: adjacencyList,
        start_node: startNode,
        end_node: endNode,
        algorithm: title,
      });

      setResult(response.data.output);
      //startAnimation();
      //setIsEditing(false);
    } catch (error) {
      console.error(error);
      setResult("Error processing graph.");
    }

  };*/

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const adjacencyList = {};
      /*graphData.edges.forEach(({ source, target }) => {
        if (!adjacencyList[source]) adjacencyList[source] = [];
        adjacencyList[source].push(target);
        if (!adjacencyList[target]) adjacencyList[target] = [];
        adjacencyList[target].push(source);  // Solo si es grafo no dirigido
        
      });*/

      const uniqueEdges = graphData.edges.filter(
        (edge, index, self) =>
          index === self.findIndex(
            (e) => (e.source === edge.source && e.target === edge.target) || 
                   (e.source === edge.target && e.target === edge.source) // Verifica en ambos sentidos
          )
      );
      
      console.log("Datos únicos del grafo (edges):");
      console.log(uniqueEdges);

      graphData.edges.forEach(({ source, target }) => {
        // Crear listas vacías si no existen para los nodos
        if (!adjacencyList[source]) adjacencyList[source] = [];
        if (!adjacencyList[target]) adjacencyList[target] = [];
  
        // Agregar conexiones solo si no están ya presentes
        if (!adjacencyList[source].includes(target)) {
          adjacencyList[source].push(target);
        }
        if (!adjacencyList[target].includes(source)) {
          adjacencyList[target].push(source); // Solo para grafos no dirigidos
        }
      })

      
      console.log("parseador nativo");
      console.log(graph);

      console.log("Lista antes de enviar: ");
      console.log(adjacencyList);
      const response = await axios.post("http://localhost:5000/process-graph", {
        adjacency_list: adjacencyList,
        start_node: startNode.trim(),
        end_node: endNode.trim(),
        algorithm: title,  // Asegúrate de usar el valor correcto
      });
  
      if (response.data.output) {
        setResult(response.data.output);
        setIsComputed(true);
      } else {
        setResult("No path found or an error occurred.");
      }
      
    } catch (error) {
      console.error('Error:', error);
      setResult("Error processing graph.");
    }
  };
  

  const resetAnimation = () => {
    setSelectedNodes([]); 
    setShowPath(false); // Oculta el mensaje al reiniciar
    setIsAnimating(false); // Habilitar el botón al reiniciar
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
          console.log(graphData);
          setNodes(parsedGraph.nodes.map(({ id, neighbors }) => ({ node: id, neighbors })));
          console.log(nodes);
        } else {
          console.error('No se pudo leer el archivo:', response.statusText);
        }
      } catch (error) {
        console.error('Error al cargar el archivo:', error);
      }
    };

    const loadPathsFromFile = async () => {
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

      <div className="controls-container">
      <h3>Status: 
        <br/>
        {result}
      </h3>
        <div className="left-controls">
        
          <form onSubmit={handleSubmit}>
            
          <div className="start-end-inputs">
       
          
            <label>Start Node:  </label>
            <input
              type="text"
              value={startNode}
              onChange={(e) => setStartNode(e.target.value)}
              placeholder="Start node"
              required
            />
          
          
            <label>End Node:  </label>
            <input
              type="text"
              value={endNode}
              onChange={(e) => setEndNode(e.target.value)}
              placeholder="End node"
              required
            />
            
          <div className='result-section'>
          <h3>Path: {selectedNodes.join(' ')}</h3>
         
            <button className="modern-button" type="submit">Compute Path</button>
            
          </div>
        </div>
        
        </form>
    </div>
        
        <div className="right-controls">
          <button 
            onClick={startAnimation} 
            disabled={(!isAnimationReady || isAnimating)} 
            className="modern-button">
            <span className="icon">🎬</span> Start Animation
          </button>
          <button onClick={resetAnimation} className="modern-button">
            <span className="icon">🔄</span> Reset
          </button>
            
          
          </div>
          
          </div>
          
  {showPath && (
        <div className="path-message">
          <p>Path Traversed ({startNode} to {endNode}): ( <strong>{selectedNodes.join(' ')}</strong> ) </p>
        </div>
      )}
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
            edgeStyle={(edge) => ({
              strokeWidth: 20,  // Cambia el valor para ajustar el grosor
              stroke: 'black', // Color del borde
            })}
          >
            
            <directionalLight position={[0, -4, 5]} intensity={1} />
          </GraphCanvas>
      </div>
    </div>
  );
};

export default GraphArea;



