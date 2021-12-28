#!/usr/bin/env python3

import itertools
import networkx as nx
import numpy as np

from scanner_lib import *

with open("input.txt") as f:
    data = f.readlines()

# put the data into a list of numpy arrays, on array per scanner
scanner_ls = []
scanner = []
for x in data:
    if x == "\n":
        scanner_ls.append(scanner)
        pass
    elif x.startswith("---"):
        scanner = []
    else:
        scanner.append([int(c) for c in x.rstrip('\n').split(',')])
scanner_ls.append(scanner)
scanner_ls = [np.array(s, dtype=int) for s in scanner_ls]


# for each scanner, make the set of absolute values of differences in 
# coordinates between pairs of beacons that it sees
# these make it quicker to test for likely beacon overlaps
coord_sets = []
for s in scanner_ls:
    coord_set = set()
    for p in s:
        for q in s:
            if not np.array_equal(p, q):
                coord_set.add(frozenset(np.abs(p-q)))    
    coord_sets.append(coord_set)

# examining these and their intersections revealed that scanners always seem
# to overlap in 12, 6, or 3 beacons
# so you can check for when they overlap at twelve, and build a graph of the
# when pairs of scanners share 12 beacons
scanner_graph = nx.Graph()
scanner_graph.add_nodes_from(range(len(scanner_ls)))

for i in range(len(coord_sets)):
    for j in range(i+1, len(coord_sets)): 
        if len(coord_sets[i] & coord_sets[j]) == 66:
            scanner_graph.add_edge(i, j)


# for each scanner, compute a path from 0 to that scanner and by composing
# the edge data the position and orientation of n relative to 0
# using that, compute the position of each beacon seen by scanner n relative
# to scanner 0
beacons_rel_0 = set()
for n in range(0, len(scanner_ls)):
    total_dir = np.array([0, 0, 0])
    total_ori = np.identity(3, dtype=int)

    path = nx.shortest_path(scanner_graph, 0, n)
    for i in range(1, len(path)):
        u = path[i-1]
        v = path[i]

        # let's try recomputing the direction and orientation in this order
        e_dir, e_ori = relative_position(scanner_ls[u], scanner_ls[v])
        # reverse =  False if u < v else True

        # e_dir = scanner_graph.edges[u, v]["direction"]
        # e_ori = scanner_graph.edges[u, v]["orientation"]
        # if reverse:
        #     e_dir = np.matmul(e_ori, -1*e_dir)
        #     e_ori = np.linalg.inv(e_ori).astype(int)

        total_dir += np.matmul(total_ori, e_dir)
        total_ori = np.matmul(total_ori, e_ori)

    for b in scanner_ls[n]:
        b_rel_0 = total_dir + np.matmul(total_ori, b)
        beacons_rel_0.add(tuple(c for c in b_rel_0))



print(len(beacons_rel_0))











