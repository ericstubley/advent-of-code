#!/usr/bin/env python3

import networkx as nx
from networkx.algorithms.shortest_paths.generic import shortest_path as shortest_path
from networkx.algorithms.shortest_paths.generic import shortest_path_length as shortest_path_length

with open("input.txt") as f:
    data = f.readlines()

risks = [[int(r) for r in row.rstrip('\n')] for row in data]
DIMN = len(risks[0])

points = [(i, j) for i in range(DIMN) for j in range(DIMN)]
graph = nx.DiGraph(points)

for i, j in points:
    if i != 0:
        graph.add_edge((i-1, j), (i, j), weight=risks[i][j])
    if i != DIMN-1:
        graph.add_edge((i+1, j), (i, j), weight=risks[i][j])
    if j != 0:
        graph.add_edge((i, j-1), (i, j), weight=risks[i][j])
    if j != DIMN-1:
        graph.add_edge((i, j+1), (i, j), weight=risks[i][j])

path = shortest_path(graph, source=(0, 0), target=(DIMN-1, DIMN-1), weight="weight")
length = shortest_path_length(graph, source=(0, 0), target=(DIMN-1, DIMN-1), weight="weight")
print(path)
print(length)
out_path = [['.']*DIMN for j in range(DIMN)]
for i, j in points:
    if (i, j) in path:
        out_path[i][j] = str(risks[i][j])
with open("output_a.txt", 'w') as fout:
    for i in range(DIMN):
        fout.write(''.join(out_path[i]) + '\n')