import React from "react";

function StartEndInput({ startNode, setStartNode, endNode, setEndNode }) {
  return (
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
  );
}

export default StartEndInput;
