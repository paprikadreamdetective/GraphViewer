import React from "react";
/*import './StartEndInput.css';*/
function StartEndInput({ startNode, setStartNode, endNode, setEndNode }) {
  return (
      <div>
        <label>Start Node:</label>
        <input
          type="text"
          value={startNode}
          onChange={(e) => setStartNode(e.target.value)}
          placeholder="Start node"
          required
        />
      
      
        <label>End Node:</label>
        <input
          type="text"
          value={endNode}
          onChange={(e) => setEndNode(e.target.value)}
          placeholder="End node"
          required
        />
     
    </div>
  );
}

export default StartEndInput;
