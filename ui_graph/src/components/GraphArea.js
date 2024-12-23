import React from 'react';
import { GraphCanvas } from 'reagraph';
import './GraphArea.css';

const GraphArea = ({ title }) => {
  return (
    <div className="graph-area">
      <h3>{title}</h3>
      {/* Aquí puedes renderizar el grafo */}
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

export default GraphArea;
