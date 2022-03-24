#! /usr/bin/env python3

from collections import defaultdict
from automation.automation import submit_answer


def parse_graph(filename):
    with open(filename) as f:
        data = f.readlines()

    vertices, edges = set(), defaultdict(list)
    for line in data:
        x, y = map(int, line.rstrip('\n').split('/'))
        vertices.add(x)
        vertices.add(y)
        # check to make sure there's no double edges
        assert y not in edges[x]
        assert x not in edges[y]
        edges[x].append(y)
        edges[y].append(x)

    return frozenset(vertices), edges


def maximum_strength(vertices, edges):
    memo_dict = dict()

    def path_helper(v, used_edges):
        if (v, used_edges) not in memo_dict:
            value = 0

            for w in edges[v]:
                edge = (min(v, w), max(v, w))
                if edge not in used_edges:
                    walk = v + w + path_helper(w, used_edges.union({edge}))
                    value = max(value, walk)

            memo_dict[v, used_edges] = value

        return memo_dict[v, used_edges]

    return path_helper(0, frozenset())


def maximum_length(vertices, edges):
    memo_dict = dict()

    def path_helper(v, used_edges):
        if (v, used_edges) not in memo_dict:
            value = (0, 0)

            for w in edges[v]:
                edge = (min(v, w), max(v, w))
                if edge not in used_edges:
                    length, strength = path_helper(w, used_edges.union({edge}))
                    length += 1
                    strength += v + w
                    value = max(value, (length, strength))

            memo_dict[v, used_edges] = value

        return memo_dict[v, used_edges]

    return path_helper(0, frozenset())



def main_a(vertices, edges):
    answer = maximum_strength(vertices, edges)
    print(answer)
    # result = submit_answer(2017, 24, 1, answer)
    # print(result)


def main_b(vertices, edges):
    __, answer = maximum_length(vertices, edges)
    print(answer)
    # result = submit_answer(2017, 24, 2, answer)
    # print(result)


if __name__ == "__main__":
    vertices, edges = parse_graph("input.txt")
    main_a(vertices, edges)
    main_b(vertices, edges)
