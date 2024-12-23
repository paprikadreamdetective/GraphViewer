import React from 'react';
import './GraphArea.css';

const GraphArea = ({ title }) => {
  return (
    <div className="graph-area">
      <h3>{title}</h3>
      {/* Aquí puedes renderizar el grafo */}
    </div>
  );
};

export default GraphArea;
