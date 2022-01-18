#!/usr/bin/env python3

import numpy as np
from scipy.signal import convolve2d as convolve

# NEIGHBOURS = np.array([[True, True, True], [True, False, True], [True, True, True]])
NEIGHBOURS = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]])


def char_to_int(c):
    return 1 if c == '#' else 0


def grid_from_file(name):
    grid = []
    with open(name) as f:
        for line in f:
            grid.append([char_to_int(c) for c in line.rstrip('\n')])

    return np.array(grid)


def update_grid(grid):
    conv = convolve(grid, NEIGHBOURS, mode='same')

    return grid*(conv == 3) + grid*(conv == 2) + (grid == 0)*(conv == 3)


if __name__ == "__main__":
    grid = grid_from_file("input.txt")

    for i in range(100):
        grid = update_grid(grid)

    print(grid.sum())