#! /usr/bin/env python3

from collections import Counter, defaultdict


def parse_data(data):
    weights, children = dict(), defaultdict(list)
    for line in data:
        split = line.split()
        name = split[0]
        weights[name] = int(split[1][1:-1])
        for child in split[3:]:
            children[name].append(child.rstrip(","))
    return weights, children


def find_root(children):
    # this set difference should have exactly one element
    # the root appears in the keys but never in the children
    root, = set(children.keys()) - set(c for cs in children.values() for c in cs)
    return root


def total_weight(name, weights, children):
    return weights[name] + sum(total_weight(c, weights, children) for c in children[name])


def find_incorrect(weights, children):
    node = find_root(children)
    while True:
        child_weights = [total_weight(c, weights, children) for c in children[node]]
        if len(set(child_weights)) > 1:
            (target, __), (failure, __) = Counter(child_weights).most_common() 
            node = children[node][child_weights.index(failure)]
        else:
            break
    return node 


def correct_weight(weights, children):
    # start at root, find the incorrect node
    bad_node = find_incorrect(weights, children)
    # find the parent of bad_node
    for node in children:
        if bad_node in children[node]:
            parent = node
            break
    # find the correct weight, then 
    bad_weight = total_weight(bad_node, weights, children)
    for sibling in children[parent]:
        sibling_weight = total_weight(sibling, weights, children)
        if sibling_weight != bad_weight:
           return weights[bad_node] + sibling_weight - bad_weight
     

if __name__ == "__main__":
    with open("input.txt") as f:
        weights, children = parse_data(f.readlines())

    print(find_root(children))
    print(find_incorrect(weights, children))
    print(correct_weight(weights, children))