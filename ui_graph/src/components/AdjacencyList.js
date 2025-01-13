import React from "react";

function AdjacencyList({ nodes }) {
  return (
    <div className="adjacency-list">
      <h2>Adjacency List</h2>
      {nodes.map((node, index) => (
        <div key={index}>
          <strong>{node.node}:</strong> {node.neighbors.join(", ")}
        </div>
      ))}
    </div>
  );
}

export default AdjacencyList;
