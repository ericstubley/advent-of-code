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
		if start == "start":
			rec_graph.remove_node(start)
		elif start.islower() and rec_graph.graph["twice_flag"]:
			rec_graph.remove_node(start)
		elif start.islower() and rec_graph.nodes[start]["seen"]:
			rec_graph.graph["twice_flag"] = True
			rec_graph.remove_node(start)
		elif not rec_graph.nodes[start]["seen"]:
			rec_graph.nodes[start]["seen"] = True

		if rec_graph.graph["twice_flag"] and e[1].islower() and rec_graph.nodes[e[1]]["seen"]:
			continue

		paths = find_paths(e[1], end, rec_graph)
		for p in paths:
			ret.append(p + [e])

	return ret


with open("input.txt") as f:
	data = f.readlines()

g = nx.Graph(twice_flag=False)
for x in data:
	xs = x.rstrip('\n').split('-')[0]
	xe = x.rstrip('\n').split('-')[1]

	for n in [xs, xe]:
		if n in ["start", "end"]:
			g.add_node(n, seen=True)
		else:
			g.add_node(n, seen=False)
	g.add_edge(xs, xe)


paths = find_paths("start", "end", g)
print(len(paths))