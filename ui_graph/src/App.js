import './App.css';
import React from 'react';
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

