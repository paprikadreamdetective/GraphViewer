// src/services/auth.js
// Mock de autenticación
const fakeAuth = {
    isAuthenticated: false,
    login(username, password) {
      return new Promise((resolve, reject) => {
        if (username === 'admin' && password === '12345') {
          this.isAuthenticated = true;
          resolve();
        } else {
          reject(new Error('Credenciales inválidas'));
        }
      });
    },
    logout() {
      this.isAuthenticated = false;
    },
    getAuthStatus() {
      return this.isAuthenticated;
    },
  };
  
  export const login = async (username, password) => {
    await fakeAuth.login(username, password);
  };
  
  export const logout = () => {
    fakeAuth.logout();
  };
  
  export const isAuthenticated = () => {
    return fakeAuth.getAuthStatus();
  };