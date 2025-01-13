import React from "react";

function NodeInput({ node, index, handleNodeChange, handleRemoveNode }) {
  return (
    <div className="node-input">
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
  );
}

export default NodeInput;
