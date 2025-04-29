// src/pages/GraphViewer.js
import React from 'react';
import { Outlet } from 'react-router-dom';

function GraphViewer() {
  return (
    <>    {/*<div className="graph-viewer-container">
      
            </div>*/}
    <Outlet/>
    </>

  );
}

export default GraphViewer;