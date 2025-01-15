import React, { useState, useEffect } from "react";
import axios from "axios";
import NodeList from "./NodeList";
import StartEndInput from "./StartEndInput";
import AdjacencyList from "./AdjacencyList";
import { parseGraphData } from "./utils"; // Importamos la función de utilidad
import grafito from "../graph/grafo_dfs.txt";

import './GraphForm.css';

function GraphForm() {
  const [nodes, setNodes] = useState([{ node: "", neighbors: [] }]);
  const [startNode, setStartNode] = useState("");
  const [endNode, setEndNode] = useState("");
  const [result, setResult] = useState("");
  const [isEditing, setIsEditing] = useState(true);


  
     // Precargar el grafo desde el archivo grafo.txt
  useEffect(() => {
    const loadGraphFromFile = async () => {
      try {
        const response = await fetch(grafito);
        if (response.ok) {
          const text = await response.text();
          const parsedGraph = parseGraphData(text);
          setNodes(parsedGraph.nodes.map(({ id, neighbors }) => ({ node: id, neighbors })));
        } else {
          console.error("No se pudo leer el archivo:", response.statusText);
        }
      } catch (error) {
        console.error("Error al cargar el archivo:", error);
      }
    };

    loadGraphFromFile();
  }, []);

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const adjacencyList = {};
      nodes.forEach((node) => {
        if (node.node) {
          adjacencyList[node.node] = node.neighbors;
        }
      });
      Object.keys(adjacencyList).forEach((key) => {
        adjacencyList[key].sort();  // Orden alfabético
      });
      
      console.log("Lista antes de enviar: ");
      console.log(adjacencyList);
      const response = await axios.post("http://localhost:5000/process-graph", {
        adjacency_list: adjacencyList,
        start_node: startNode,
        end_node: endNode,
        algorithm: "defgraph",
      });

      setResult(response.data.output);
      setIsEditing(false);
    } catch (error) {
      console.error(error);
      setResult("Error processing graph.");
    }
  };

  return (
    <>
    
    <div className="form-container">
        
      
      
        <form onSubmit={handleSubmit}>
        <div className="quadrant-1">
          <NodeList nodes={nodes} setNodes={setNodes} />
          </div>
          <StartEndInput
            startNode={startNode}
            setStartNode={setStartNode}
            endNode={endNode}
            setEndNode={setEndNode}
          />
          <h2>Result:</h2>
          <pre>{result}</pre>
          <button type="submit">Submit</button>
        </form>
        <AdjacencyList nodes={nodes} />
        
            
            {/*<div className="quadrant-2">
                <AdjacencyList nodes={nodes} />
                <div>
                    <h2>Result:</h2>
                    <pre>{result}</pre>
                </div>
            </div>*/}

        </div>
        </>
  );
}

export default GraphForm;



