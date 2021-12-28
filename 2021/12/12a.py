#!/usr/bin/env python3

import networkx as nx

def find_paths(start, end, graph):
    # return a list of paths (list of edges)
    ret = []
    for e in graph.edges(start):

        if e[1] == end:
            ret.append([e])
            continue

        rec_graph = graph.copy()
        if start.islower():
            rec_graph.remove_node(start)
        paths = find_paths(e[1], end, rec_graph)
        for p in paths:
            ret.append(p + [e])
    return ret


with open("input.txt") as f:
    data = f.readlines()

g = nx.Graph()
for x in data:
    xs = x.rstrip('\n').split('-')[0]
    xe = x.rstrip('\n').split('-')[1]
    g.add_edge(xs, xe)

paths = find_paths("start", "end", g)
print(len(paths))