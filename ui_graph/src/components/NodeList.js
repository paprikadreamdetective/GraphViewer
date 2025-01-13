import React from "react";
import NodeInput from "./NodeInput";

function NodeList({ nodes, setNodes }) {
  const handleAddNode = () => {
    setNodes([...nodes, { node: "", neighbors: [] }]);
  };

  const handleRemoveNode = (index) => {
    const updatedNodes = [...nodes];
    updatedNodes.splice(index, 1);
    setNodes(updatedNodes);
  };

  const handleNodeChange = (index, key, value) => {
    const updatedNodes = [...nodes];
    if (key === "node") {
      updatedNodes[index].node = value;
    } else if (key === "neighbors") {
      updatedNodes[index].neighbors = value.split(",").map((neighbor) => neighbor.trim());
    }
    setNodes(updatedNodes);
  };

  return (
    <div className="node-section">
      <h2>Nodes</h2>
      {nodes.map((node, index) => (
        <NodeInput
          key={index}
          node={node}
          index={index}
          handleNodeChange={handleNodeChange}
          handleRemoveNode={handleRemoveNode}
        />
      ))}
      <button type="button" onClick={handleAddNode}>
        Add Node
      </button>
    </div>
  );
}

export default NodeList;
