from LispTemplates.dfs_temp import generate_lisp_code_dfs
from LispTemplates.bfs_temp import generate_lisp_code_bfs
class GraphAlgorithms():
    def __init__(self):
        pass
    def DepthFirstSearch(self, adjacency_list, start_node, end_node):
        return generate_lisp_code_dfs(adjacency_list, start_node, end_node)
    def BreadthFirstSearch(self, adjacency_list, start_node, end_node):
        return generate_lisp_code_bfs(adjacency_list, start_node, end_node)