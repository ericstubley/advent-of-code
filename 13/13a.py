#!/usr/bin/env python3

import numpy as np

def origami_fold(paper, axis, coord):
    folded = None
    if axis == 'x':
        folded = paper[0:coord, :]
        folded += np.flipud(paper[coord+1:2*coord+1, :])
    if axis == 'y':
        folded = paper[:, 0:coord]
        folded += np.fliplr(paper[:, coord+1:2*coord+1])
    return folded

with open("input.txt") as f:
# with open("test_input.txt") as f:
    data = f.readlines()

dots = []
folds = []
for line in data:
    if line.startswith("fold along"):
        axis = line.lstrip("fold along ").rstrip('\n').split('=')[0]
        coord = line.lstrip("fold along ").rstrip('\n').split('=')[1]
        folds.append((axis, int(coord)))
    elif line != "\n":
        x = line.rstrip('\n').split(',')[0]
        y = line.rstrip('\n').split(',')[1]
        dots.append((int(x), int(y)))

# the paper size comes from the first fold lines in each direction
paper = np.zeros((655*2 + 1, 447*2 + 1), dtype=int)
# paper = np.zeros((5*2 + 1, 7*2 + 1))
for x, y in dots:
    paper[x, y] = 1

# part a only do the first fold
paper = origami_fold(paper, folds[0][0], folds[0][1])
# print(np.transpose(paper))
print(np.sum(paper > 0))