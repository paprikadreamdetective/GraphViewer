/*import React, { useState } from "react";
import axios from "axios";

function GraphForm(){
  const [adjacencyList, setAdjacencyList] = useState("");
  const [startNode, setStartNode] = useState("");
  const [endNode, setEndNode] = useState("");
  const [result, setResult] = useState("");

  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      const response = await axios.post("http://localhost:5000/process-graph", {
        adjacency_list: JSON.parse(adjacencyList),
        start_node: startNode,
        end_node: endNode,
      });
      setResult(response.data.output);
    } catch (error) {
      console.error(error);
      setResult("Error processing graph.");
    }
  };

  return (
    <div>
        <h1>Graph Input</h1>
        <form onSubmit={handleSubmit}>
            <div>
            <label>Adjacency List (JSON format):</label>
            <textarea
                rows="5"
                value={adjacencyList}
                onChange={(e) => setAdjacencyList(e.target.value)}
                placeholder='{"a": ["b", "c"], "b": ["a", "d"], "c": ["a"], "d": ["b"]}'
                required
            ></textarea>
            </div>
            <div>
                <label>Start Node:</label>
                <input
                    type="text"
                    value={startNode}
                    onChange={(e) => setStartNode(e.target.value)}
                    required
                />
            </div>
            <div>
            <label>End Node:</label>
                <input
                    type="text"
                    value={endNode}
                    onChange={(e) => setEndNode(e.target.value)}
                    required
                />
            </div>
            <button type="submit">Submit</button>
        </form>
        <h2>Result:</h2>
        <pre>{result}</pre>
    </div>
  );
};

export default GraphForm;*/
import React, { useState } from "react";
import axios from "axios";
//import './GraphForm.css';

function GraphForm() {
  const [nodes, setNodes] = useState([{ node: "", neighbors: [] }]);
  const [startNode, setStartNode] = useState("");
  const [endNode, setEndNode] = useState("");
  const [result, setResult] = useState("");
  const [isEditing, setIsEditing] = useState(true);

  // Agregar un nodo
  const handleAddNode = () => {
    setNodes([...nodes, { node: "", neighbors: [] }]);
  };

  // Eliminar un nodo
  const handleRemoveNode = (index) => {
    const updatedNodes = [...nodes];
    updatedNodes.splice(index, 1);
    setNodes(updatedNodes);
  };

  // Manejar cambio en el nodo o su lista de vecinos
  const handleNodeChange = (index, key, value) => {
    const updatedNodes = [...nodes];
    if (key === "node") {
      updatedNodes[index].node = value;
    } else if (key === "neighbors") {
      updatedNodes[index].neighbors = value.split(",").map(neighbor => neighbor.trim());
    }
    setNodes(updatedNodes);
  };

  // Manejar envío del formulario
  const handleSubmit = async (e) => {
    e.preventDefault();
    try {
      // Convertimos el grafo a formato JSON
      const adjacencyList = {};
      nodes.forEach((node) => {
        if (node.node) {
          adjacencyList[node.node] = node.neighbors;
        }
      });

      // Enviar el grafo al backend
      const response = await axios.post("http://localhost:5000/process-graph", {
        adjacency_list: adjacencyList,
        start_node: startNode,
        end_node: endNode,
      });

      setResult(response.data.output);
      setIsEditing(false);  // Cambiar a modo visualización después de enviar
    } catch (error) {
      console.error(error);
      setResult("Error processing graph.");
    }
  };

  const renderAdjacencyList = () => {
    return (
      <div className="adjacency-list">
        {nodes.map((node, index) => (
          <div key={index}>
            <strong>{node.node}:</strong> {node.neighbors.join(", ")}
          </div>
        ))}
      </div>
    );
  };

  return (
    <>
    {/*<div className="form-container">
      <h1>Graph Input</h1>

      <form onSubmit={handleSubmit}>
        <div className="node-section">
          <h2>Nodes</h2>
          {nodes.map((node, index) => (
            <div key={index} className="node-input">
              <label>Node {index + 1}:</label>
              <input
                type="text"
                value={node.node}
                onChange={(e) => handleNodeChange(index, "node", e.target.value)}
                placeholder="Node name"
                required
              />
              <label>Neighbors (comma separated):</label>
              <input
                type="text"
                value={node.neighbors.join(", ")}
                onChange={(e) => handleNodeChange(index, "neighbors", e.target.value)}
                placeholder="Neighbor1, Neighbor2"
                required
              />
              <button type="button" onClick={() => handleRemoveNode(index)}>
                Remove Node
              </button>
            </div>
          ))}
          <button type="button" onClick={handleAddNode}>Add Node</button>
        </div>

        <div className="start-end-nodes">
          <div>
            <label>Start Node:</label>
            <input
              type="text"
              value={startNode}
              onChange={(e) => setStartNode(e.target.value)}
              placeholder="Start node"
              required
            />
          </div>
          <div>
            <label>End Node:</label>
            <input
              type="text"
              value={endNode}
              onChange={(e) => setEndNode(e.target.value)}
              placeholder="End node"
              required
            />
          </div>
        </div>

        <button type="submit">Submit</button>
      </form>

      <h2>Result:</h2>
      <pre>{result}</pre>
    </div>*/}

    <div className="form-container">
      <h1>Graph Input</h1>
      {/*isEditing ? (}*/}
        <form onSubmit={handleSubmit}>
          <div className="node-section">
            <h2>Nodes</h2>
            {nodes.map((node, index) => (
              <div key={index} className="node-input">
                <label>Node {index + 1}:</label>
                <input
                  type="text"
                  value={node.node}
                  onChange={(e) => handleNodeChange(index, "node", e.target.value)}
                  placeholder="Node name"
                  required
                />
                <label>Neighbors (comma separated):</label>
                <input
                  type="text"
                  value={node.neighbors.join(", ")}
                  onChange={(e) => handleNodeChange(index, "neighbors", e.target.value)}
                  placeholder="Neighbor1, Neighbor2"
                  required
                />
                <button type="button" onClick={() => handleRemoveNode(index)}>
                  Remove Node
                </button>
              </div>
            ))}
            <button type="button" onClick={handleAddNode}>Add Node</button>
          </div>

          <div className="start-end-nodes">
            <div>
              <label>Start Node:</label>
              <input
                type="text"
                value={startNode}
                onChange={(e) => setStartNode(e.target.value)}
                placeholder="Start node"
                required
              />
            </div>
            <div>
              <label>End Node:</label>
              <input
                type="text"
                value={endNode}
                onChange={(e) => setEndNode(e.target.value)}
                placeholder="End node"
                required
              />
            </div>
          </div>

          <button type="submit">Submit</button>
        </form>
      {/*) : (*/}
        <div>
          <h2>Adjacency List</h2>
          {renderAdjacencyList()}
          <h2>Result:</h2>
          <pre>{result}</pre>
        </div>
      {/*)}*/}
    </div>
    </>
  );
}

export default GraphForm;


