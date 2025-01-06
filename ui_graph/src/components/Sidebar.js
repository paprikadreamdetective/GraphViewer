import React, { useState } from 'react';
import { Link } from 'react-router-dom';
import './Sidebar.css';

const Sidebar = () => {
  const [selected, setSelected] = useState('');

  const handleSelect = (algorithm) => {
    setSelected(algorithm);
  };

  return (
    <div className="sidebar">
      <h2>Algorithms</h2>
      <hr />
      <nav>
        <ul>
          <li className={selected === 'dfs' ? 'selected' : ''}
            onClick={() => handleSelect('dfs')}>
            <Link to="/dfs">Depth-First-Search</Link>
          </li>
          <li className={selected === 'bfs' ? 'selected' : ''}
            onClick={() => handleSelect('bfs')}>
            <Link to="/bfs">Breadth-First-Search</Link>
          </li>
          <li className={selected === 'bestfs' ? 'selected' : ''}
            onClick={() => handleSelect('bestfs')}>
            <Link to="/bestfs">Best-First-Search</Link>
          </li>
        </ul>
      </nav>
    </div>
  );
};

export default Sidebar;
