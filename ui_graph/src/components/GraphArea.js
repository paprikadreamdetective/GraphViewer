/*import React from 'react';
import { GraphCanvas } from 'reagraph';
import './GraphArea.css';

const GraphArea = ({ title }) => {
  return (
    <div className="graph-area">
      <h3>{title}</h3>
      
      <div className="graph-canvas"> 
        {<GraphCanvas
          sizingType="none"
          edgeArrowPosition="none"
          cameraMode="rotate"
          nodes={[
            {
              id: 'n-1',
              label: '1'
            },
            {
              id: 'n-2',
              label: '2'
            }
          ]}
          edges={[
            {
              id: '1-2',
              source: 'n-1',
              target: 'n-2',
              label: 'Edge 1-2'
            }
          ]}
        />}
      </div>
    </div>
  );
};

export default GraphArea;*/
import './GraphArea.css';
import React, { useState } from 'react';
import { darkTheme, GraphCanvas, directionalLight } from 'reagraph';

const GraphArea = ({ title }) => {
  const [graphData, setGraphData] = useState({ nodes: [], edges: [] });

  const handleFileUpload = async (event) => {
    const file = event.target.files[0];
    if (file) {
      const text = await file.text();
      const parsedGraph = parseGraphData(text);
      setGraphData(parsedGraph);
    }
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
  
  
  

  return (
    
      
      <div className="graph-area">
        <h3>{title}</h3>
        <input type="file" accept=".txt" onChange={handleFileUpload} />
          <div className="graph-canvas">
            <GraphCanvas
            nodes={graphData.nodes}
            edges={graphData.edges}
            theme={darkTheme}
            layoutType="forceDirected3d"
            
              sizingType="none"
              edgeArrowPosition="none" // No arrows for undirected graph
              cameraMode="rotate"
              
            >

              <directionalLight position={[0, 5, -4]} intensity={1} />
            </GraphCanvas>
        </div>
      </div>
    
  );
};

export default GraphArea;



