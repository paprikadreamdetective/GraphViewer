// src/pages/Home.js
import React from 'react';
import { Link } from 'react-router-dom';
import './Home.css';

function Home() {
  return (
    <div className="home-container">
      <h1>Bienvenido a GraphViewer</h1>
      <p>Visualiza y explora algoritmos de búsqueda en grafos.</p>
      <Link to="/login">
        <button>Iniciar Sesión</button>
      </Link>
    </div>
  );
}

export default Home;