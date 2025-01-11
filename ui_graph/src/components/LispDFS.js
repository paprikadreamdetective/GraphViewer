import React, { useState } from 'react';
import { Interpreter } from 'lips';

const LispDFS = () => {
  const [startNode, setStartNode] = useState('a');
  const [endNode, setEndNode] = useState('s');
  const [output, setOutput] = useState('');
  const [error, setError] = useState('');

  // Código Lisp como string
  const lispCode = `
    (defun extend (path)
      (mapcar
        #'(lambda (new-node) (cons new-node path))
        (remove-if #'(lambda (neighbor) (member neighbor path)) (get (first path) 'neighbors))
      )
    )

    (defun depth-first-search (start finish &optional (queue (list (list start))))
      (cond
        ((endp queue) nil)
        ((eq finish (first (first queue))) (reverse (first queue)))
        (t
          (depth-first-search start finish
            (append (extend (first queue)) (rest queue)))))
    )

    (setf (get 'a 'neighbors) '(b c d e)
          (get 'b 'neighbors) '(a c g f)
          (get 'c 'neighbors) '(a d f b)
          (get 'd 'neighbors) '(a e g f c)
          (get 'e 'neighbors) '(a h g d)
          (get 'f 'neighbors) '(b c d g j l)
          (get 'g 'neighbors) '(b d e h k j i f)
          (get 'h 'neighbors) '(e k g)
          (get 'i 'neighbors) '(b g j m l)
          (get 'j 'neighbors) '(f g k o n m i)
          (get 'k 'neighbors) '(h r o j g)
          (get 'l 'neighbors) '(f i m q p)
          (get 'm 'neighbors) '(i j n p l)
          (get 'n 'neighbors) '(j o q p m)
          (get 'o 'neighbors) '(j k r q n)
          (get 'p 'neighbors) '(l m n q s)
          (get 'q 'neighbors) '(l n o r s p)
          (get 'r 'neighbors) '(o k s q)
          (get 's 'neighbors) '(p q r)
    )
  `;

  const runDFS = async () => {
    const interpreter = new Interpreter();

    try {
      // Carga el código Lisp inicial
      await interpreter.eval(lispCode);

      // Ejecuta la búsqueda DFS
      const dfsResult = await interpreter.eval(
        `(depth-first-search '${startNode} '${endNode})`
      );

      setOutput(`DFS Path: ${dfsResult}`);
      setError('');
    } catch (err) {
      setError(err.message);
      setOutput('');
    }
  };

  return (
    <div style={{ padding: '20px', fontFamily: 'Arial' }}>
      <h1>DFS in React with Lisp</h1>
      <div>
        <label>Start Node: </label>
        <input
          type="text"
          value={startNode}
          onChange={(e) => setStartNode(e.target.value)}
        />
      </div>
      <div>
        <label>End Node: </label>
        <input
          type="text"
          value={endNode}
          onChange={(e) => setEndNode(e.target.value)}
        />
      </div>
      <button onClick={runDFS} style={{ marginTop: '10px' }}>
        Run DFS
      </button>
      {output && (
        <div style={{ marginTop: '20px', whiteSpace: 'pre-wrap' }}>
          <h3>Output:</h3>
          <pre>{output}</pre>
        </div>
      )}
      {error && (
        <div style={{ marginTop: '20px', color: 'red' }}>
          <h3>Error:</h3>
          <pre>{error}</pre>
        </div>
      )}
    </div>
  );
};

export default LispDFS;
