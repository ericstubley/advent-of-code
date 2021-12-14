#!/usr/bin/env python3

import numpy as np

def upper_char_to_num(c):
    return ord(c) - 65 

with open("input.txt") as f:
    data = f.readlines()

polymer_str = data[0].rstrip('\n')
raw_rules = data[2:]
rules = {}
for i in range(len(raw_rules)):
    key = raw_rules[i].rstrip('\n').split(" -> ")[0]
    val = raw_rules[i].rstrip('\n').split(" -> ")[1]
    rules[key] = val
sorted_keys = sorted(rules.keys())

# make the starting vector
sorted_keys = sorted(rules.keys())
polymer = np.zeros(len(sorted_keys), dtype=int)
for i in range(len(polymer_str) - 1):
    coord = sorted_keys.index(polymer_str[i:i+2])
    polymer[coord] += 1

# make the update matrix
polymerize = np.zeros((len(sorted_keys), len(sorted_keys)), dtype=int)
for i, k in enumerate(sorted_keys):
    val = rules[k]
    coord1, coord2 = k[0] + val, val + k[1]
    j1, j2 = sorted_keys.index(coord1), sorted_keys.index(coord2)

    polymerize[j1][i] += 1
    polymerize[j2][i] += 1

# compute counts after 40 steps
forty_step_polymerize = np.linalg.matrix_power(polymerize, 40)
forty_step_polymer = np.matmul(forty_step_polymerize, polymer)
print(sum(forty_step_polymer))

# translate from pair counts to letter counts
head = polymer_str[0]
tail = polymer_str[-1]

letter_counts = [0]*26
for i, k in enumerate(sorted_keys):
    letter_counts[upper_char_to_num(k[0])] += forty_step_polymer[i]
    letter_counts[upper_char_to_num(k[1])] += forty_step_polymer[i]
letter_counts[upper_char_to_num(head)] += 1
letter_counts[upper_char_to_num(tail)] += 1
letter_counts = [int(n/2) for n in letter_counts]

non_zero_counts = [n for n in letter_counts if n != 0]
print(letter_counts)
print(max(non_zero_counts) - min(non_zero_counts))
