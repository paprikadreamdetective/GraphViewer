export const parseGraphData = (text) => {
    const lines = text.trim().split("\n");
    const nodes = [];
  
    lines.forEach((line) => {
      const [node, neighbors] = line.split(":");
      const id = node.trim();
      const neighborsList = neighbors.replace(/[()]/g, "").trim().split(" ");
      nodes.push({ id, neighbors: neighborsList });
    });
  
    return { nodes };
  };
  