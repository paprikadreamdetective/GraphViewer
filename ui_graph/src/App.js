import './App.css';
import React from 'react';
import { BrowserRouter as Router, Route, Routes } from 'react-router-dom';
import Sidebar from './components/Sidebar';
import GraphArea from './components/GraphArea';
import GraphForm from './components/GraphForm';

import './App.css';
import graph from './graph/grafo.txt'; 
import graph_dfs from './graph/grafo_dfs.txt' ;
import graph_bfs from './graph/grafo_bfs.txt' ;

// rutas recorridas: importamos txt:

import dfs_path from './graph/rutas_recorridas_dfs.txt';
import bfs_path from './graph/rutas_recorridas_bfs.txt';
import best_first_search_path from './graph/caminos.txt';
import random_bfs_path from './graph/rutas_recorridas_bfs_random.txt';
import cormen_bfs_path from './graph/movements.txt';


function App() {
  return (
    <Router>
      <div className="app-container">
        <Sidebar />
          <Routes>
            <Route path="/dfs" element={<GraphArea title="Depth-First-Search" graph={graph_dfs} visitedPaths={dfs_path} delayAnimation={1000} />}/>
            <Route path="/bfs" element={<GraphArea title="Breadth-First-Search" graph={graph_dfs} visitedPaths={bfs_path} delayAnimation={250} />} />
            <Route path="/bestfs" element={<GraphArea title="Best-First-Search" graph={graph_dfs} visitedPaths={best_first_search_path} delayAnimation={250}/>} />
            <Route path="/bfsrandom" element={<GraphArea title="Random-Breadth-First-Search" graph={graph_dfs} visitedPaths={random_bfs_path} delayAnimation={250}/>} />
            <Route path="/bfscormen" element={<GraphArea title="Cormen-Breadth-First-Search" graph={graph_dfs} visitedPaths={cormen_bfs_path} delayAnimation={1000}/>} />
            <Route path="/graphform" element={<GraphForm/>} />
          </Routes>
        
      </div>
    </Router>
  );
}

export default App;

