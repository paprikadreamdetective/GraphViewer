import React, { useState } from 'react';
import { Link, useLocation } from 'react-router-dom';
import './Sidebar.css';

const Sidebar = () => {
  //const [selected, setSelected] = useState('');
  const location = useLocation(); // Obtener la ruta actual
  const [selected, setSelected] = useState(location.pathname);
  const handleSelect = (algorithm) => {
    setSelected(algorithm);
  };

  return (
    <div className="sidebar">
      <h2>Algorithms</h2>
      <hr />
      <nav>
        <ul>
          <li className={selected === 'graphform' ? 'selected' : ''}
            onClick={() => handleSelect('graphform')}>
            <Link to="/graphform">Graph Settings</Link>
          </li>
          <li className={selected === 'dfs' ? 'selected' : ''}
            onClick={() => handleSelect('dfs')}>
            <Link to="/graphs/dfs">Depth-First-Search</Link>
          </li>
          <li className={selected === 'bfs' ? 'selected' : ''}
            onClick={() => handleSelect('bfs')}>
            <Link to="/graphs/bfs">Breadth-First-Search</Link>
          </li>
          <li className={selected === 'bestfs' ? 'selected' : ''}
            onClick={() => handleSelect('bestfs')}>
            <Link to="/graphs/bestfs">Best-First-Search</Link>
          </li>
          <li className={selected === 'bfsrandom' ? 'selected' : ''}
            onClick={() => handleSelect('bfsrandom')}>
            <Link to="/graphs/bfsrandom">Random-Breadth-First-Search</Link>
          </li>
          <li className={selected === 'bfscormen' ? 'selected' : ''}
            onClick={() => handleSelect('bfscormen')}>
            <Link to="/graphs/bfscormen">Cormen-Breadth-First-Search</Link>
          </li>
        </ul>
      </nav>
    </div>
  );
};

export default Sidebar;
