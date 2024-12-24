//import logo from './logo.svg';
import './App.css';
import React from 'react';
//import { GraphCanvas } from 'reagraph';

import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import Sidebar from './components/Sidebar';
import GraphArea from './components/GraphArea';
import './App.css';

function App() {
  return (
    <Router>
      <div className="app-container">
        <Sidebar />
          <Routes>
            <Route path="/dfs" element={<GraphArea title="Depth-First-Search" />}/>
            <Route path="/bfs" element={<GraphArea title="Breadth-First-Search" />} />
            <Route path="/bestfs" element={<GraphArea title="Best-First-Search" />} />
          </Routes>
        
      </div>
    </Router>
  );
}

export default App;

/*function App() {
  return (
    <div className="App">
      <GraphCanvas
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
  />
    </div>
  );
}

export default App;*/
