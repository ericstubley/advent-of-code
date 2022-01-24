#!/usr/bin/env python3

from itertools import combinations
from math import prod


def ideal_first_group(weights):
    total = sum(weights) // 4
    for i in range(1, len(weights)+1):
        print(i)
        good_subsets = [s for s in combinations(weights, i) if sum(s) == total]
        if len(good_subsets) > 0:
            return find_min_quantum_entanglement(good_subsets)
    

def find_min_quantum_entanglement(subsets): 
    # given a list of sets of the same length
    # return the one with the smallest product
    return min(subsets, key=lambda x: prod(x))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    weights = tuple(int(__.rstrip('\n')) for __ in data)

    ifg = ideal_first_group(weights)
    print(ifg, prod(ifg))
