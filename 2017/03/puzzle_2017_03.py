#! /usr/bin/env python3

from collections import defaultdict
from itertools import product
from math import ceil, floor, sqrt

NS_VEC = {0: 1, 1: -1}
EW_VEC = {0: -1, 1: 1}


def ulam_coordinates(n):
    # find the closest square above and work backwards
    k = ceil(sqrt(n))
    parity = k % 2
    offset = k**2 - n
    x, y = square_coordinates(k)
    leg_1 = min(offset, k-1)
    leg_2 = offset - leg_1
    return x - leg_1 * EW_VEC[parity], y - leg_2* NS_VEC[parity]


def square_coordinates(k):
    if k % 2 == 0:
        return (2 - k)//2, k//2
    else:
        return (k - 1)//2, (1 - k)//2


def ulam_in_ny(n):
    x, y = ulam_coordinates(n)
    return abs(x) + abs(y)


def neighborhood(x, y):
    return ((x + dx, y + dy) for dx, dy in product([-1, 0, 1], repeat=2))


def ulam_lies(limit):
    # because it's like FIBonacci on an ULAM grid
    grid = defaultdict(int)
    grid[(0, 0)] = 1
    n, current_value = 0, 0 
    while current_value < limit:
        n += 1
        x, y = ulam_coordinates(n)
        current_value = sum(grid[(a, b)] for a, b in neighborhood(x, y))
        grid[(x, y)] = current_value
    return current_value


def main_a():
    print(ulam_in_ny(265149))


def main_b():
    print(ulam_lies(265149))


if __name__ == "__main__":
    main_a()
    main_b()
