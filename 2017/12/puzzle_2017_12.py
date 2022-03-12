#! /usr/bin/env python3

from automation.automation import *
from collections import defaultdict, deque


class DisjointSet:
    def __init__(self, points):
        self.points = [p for p in points]
        self.num_components = len(points)


    def join(self, a, b):
        if not self.in_same_component(a, b):
            a_rep, b_rep = self.representative(a), self.representative(b)
            assert self.points[a_rep] == a_rep
            assert self.points[b_rep] == b_rep
            self.points[a_rep] = b_rep
            self.num_components -= 1


    def in_same_component(self, a, b):
        return self.representative(a) == self.representative(b)


    def representative(self, a):
        if self.points[a] == a:
            return a
        else:
            rep = self.representative(self.points[a])
            self.points[a] = rep
            return rep


def parse_pipe_graph(filename):
    with open(filename) as f:
        data = f.readlines()

    graph = defaultdict(list)
    for line in data:
        split = line.split()
        v = int(split[0])
        for w in split[2:]:
            graph[v].append(int(w.rstrip(',')))
    return graph

        
def component_size(graph, vertex):
    seen = set([vertex])
    queue = deque([vertex])
    while len(queue) > 0:
        v = queue.popleft()
        for w in graph[v]:
            if w not in seen:
                seen.add(w)
                queue.append(w)
    return len(seen)


def count_components(graph):
    components = DisjointSet([i for i in range(len(graph))])
    for v in graph:
        for w in graph[v]:
            components.join(v, w)
    return components.num_components


def main_a(graph):
    answer = component_size(graph, 0)
    print(answer)
    result = submit_answer(2017, 12, 1, answer)
    print(result)


def main_b(graph):
    answer = count_components(graph)
    print(answer)
    result = submit_answer(2017, 12, 2, answer)
    print(result)


if __name__ == "__main__":
    graph = parse_pipe_graph("input.txt")
    main_a(graph)
    main_b(graph)
