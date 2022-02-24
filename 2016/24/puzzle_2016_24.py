#! /usr/bin/env python3


from itertools import permutations


def neighbors(i, j):
    # inputs are totally surrounded by walls, so you will never walk over
    # the edges of the map; so this just returns the 4 possible moves without
    # any boundary checking
    return [(i-1, j), (i+1, j), (i, j-1), (i, j+1)]


def parse(data):
    nodes = dict()
    maze = []
    for i, line in enumerate(data):
        maze.append(line.rstrip('\n'))

        for j, c in enumerate(line):
            if c.isdigit():
                nodes[int(c)] = (i, j)
    return nodes, maze


def shortest_roads(nodes, maze):
    # given the node locations and maze, find the shortest maze path length
    # (called the "road") from node x to node y for each pair (x, y)
    roads = dict()

    for n in nodes:
        print(f"computing roads out of {n}")
        curr_level = [nodes[n]]
        curr_dist = 0
        seen = set()
        seen.add(nodes[n])


        while len(curr_level) > 0:
            next_level = []
            for i, j in curr_level:
                if maze[i][j].isdigit() and maze[i][j] != str(n):
                    roads[(n, int(maze[i][j]))] = curr_dist

                for di, dj in neighbors(i, j):
                    if maze[di][dj] != '#' and (di, dj) not in seen:
                        next_level.append((di, dj))
                        seen.add((di, dj))

            curr_level = next_level
            curr_dist += 1
    return roads


def path_length(path, roads):
    # compute the length of the given path along the roads
    length = 0
    for i in range(1, len(path)):
        length += roads[(path[i-1], path[i])]
    return length


def shortest_path(num_nodes, roads, loop=False):
    # given the weighted graph structure from roads, brute force all possible
    # path permutations and find the shortest possible
    min_length = 1 << 64  # just something bigger than an actual path
    min_path = None
    node_ls = list(range(1, num_nodes))
    for sigma in permutations(node_ls):
        path = [0] + list(sigma)
        if loop:
            path += [0]
        length = path_length(path, roads)

        if length < min_length:
            min_length = length
            min_path = path
    return min_length, min_path


def main_a(num_nodes, roads):
    print(shortest_path(num_nodes, roads, loop=False))


def main_b(num_nodes, roads):
    print(shortest_path(num_nodes, roads, loop=True))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    nodes, maze = parse(data)
    roads = shortest_roads(nodes, maze)

    main_a(len(nodes), roads)
    main_b(len(nodes), roads)
