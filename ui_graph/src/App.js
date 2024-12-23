import './App.css';
import React from 'react';
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import Sidebar from './components/Sidebar';
import GraphArea from './components/GraphArea';
import './App.css';
import graph from './graph/grafo.txt' 
import graph_dfs from './graph/grafo_dfs.txt' 

function App() {
  return (
    <Router>
      <div className="app-container">
        <Sidebar />
          <Routes>
            <Route path="/dfs" element={<GraphArea title="Depth-First-Search" graph={graph_dfs} delayAnimation={1000} />}/>
            <Route path="/bfs" element={<GraphArea title="Breadth-First-Search" graph={graph} delayAnimation={1000} />} />
            <Route path="/bestfs" element={<GraphArea title="Best-First-Search" graph={graph} visitedPaths={''} delayAnimation={250}/>} />
          </Routes>
        
      </div>
    </Router>
  );
}

export default App;

