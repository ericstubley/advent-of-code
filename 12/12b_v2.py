#!/usr/bin/env python3

def find_paths(node, graph, visited, revisit):
    next_visited = visited
    next_revisit = revisit

    if node == "end":
        return [[node]]
    elif node in visited and (revisit == False or node == "start"):
        return []
    elif node in visited:
        next_revisit = False
    elif node.islower():
        next_visited = visited|{node}

    paths = []
    for neighbor in graph[node]:
        paths += find_paths(neighbor, graph, next_visited, next_revisit)
    return [p + [node] for p in paths]

with open("input.txt") as f:
    data = f.readlines()


graph = dict()
for x in data:
    a = x.rstrip('\n').split('-')[0]
    b = x.rstrip('\n').split('-')[1]

    for node in [a, b]:
        if node not in graph:
            graph[node] = []
    graph[a].append(b)
    graph[b].append(a)


paths = find_paths("start", graph, set(), True)
print(len(paths))