#!/usr/bin/env python3

import copy

from snailfish_number import SnailfishNumber, SnailfishParser

def simplify(n):
    if n.is_simplified():
        return n
    elif n.max_depth() > 4:
        dfs_ls = n.dfs()
        for index, (node, depth) in enumerate(dfs_ls):
            if depth >= 4 and node.max_depth() == 1: 
                l, r = node.explode()
                leaf_l = next((n for n, d in dfs_ls[:index][::-1] if n.value != None), None)
                leaf_r = next((n for n, d in dfs_ls[index+3:] if n.value != None), None)
                if leaf_l != None:
                    leaf_l.value += l
                if leaf_r != None:
                    leaf_r.value += r
                break
        return simplify(n)
    else:
        dfs_ls = n.dfs()
        for index, (node, depth) in enumerate(dfs_ls):
            if node.value != None and node.value >= 10:
                node.split()
                break
        return simplify(n)

with open("input.txt") as f:
    data = f.readlines()

# convert each string to a number
numbers = []
for x in data:
    n, s = SnailfishParser(x.rstrip('\n')).parse()
    numbers.append(n)

max_norm = 0
for x in numbers:
    for y in numbers:
        if x != y:
            a = copy.deepcopy(x+y)
            a = simplify(a)
            if a.magnitude() > max_norm:
                max_norm = a.magnitude()


# magnitude
print(max_norm)
