import React from 'react';
import { Link } from 'react-router-dom';
import './Sidebar.css';

const Sidebar = () => {
  return (
    <div className="sidebar">
      <h2>Algorithms</h2>
      <hr />
      <nav>
        <ul>
          <li><Link to="/dfs">Depth-First-Search</Link></li>
          <li><Link to="/bfs">Breadth-First-Search</Link></li>
          <li><Link to="/bestfs">Best-First-Search</Link></li>
        </ul>
      </nav>
    </div>
  );
};

export default Sidebar;
