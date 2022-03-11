#! /usr/bin/env python3

from automation.automation import *
from itertools import product


def sign(x):
    if x == 0:
        return 0
    elif x < 0:
        return -1 
    elif x > 0:
        return 1


class HexVector:
    def __init__(self, n, nw):
        self.n = n
        self.nw = nw


    def __add__(self, other):
        return HexVector(self.n + other.n, self.nw + other.nw) 


    def __str__(self):
        return f"{self.n}*N + {self.nw}*NW"

    @property
    def distance(self):
        if sign(self.n) == sign(self.nw):
            return abs(self.n) + abs(self.nw) 
        else:
            return max(abs(self.n), abs(self.nw)) 


N, NE, SE = HexVector(1, 0), HexVector(1, -1), HexVector(0, -1),
S, SW, NW = HexVector(-1, 0), HexVector(-1, 1), HexVector(0, 1)
dir_to_vec = {'n': N, 'ne': NE, 'se': SE, 's': S, 'sw': SW, 'nw': NW}


def sum_path(path):
    return sum((dir_to_vec[d] for d in path), start=HexVector(0, 0))


def max_path(path):
    curr = HexVector(0, 0)
    max_dist = 0
    for d in path:
        curr += dir_to_vec[d]
        max_dist = max(max_dist, curr.distance)
    return max_dist


def hex_distance(path):
    final_vector = sum_path(path)
    return final_vector.distance


def main_a(path):
    answer = hex_distance(path)
    print(sum_path(path))
    print(answer)
    result = submit_answer(2017, 11, 1, answer)
    print(result)


def main_b(path):
    answer = max_path(path)
    print(answer)
    result = submit_answer(2017, 11, 2, answer)
    print(result)


if __name__ == "__main__":
    with open("input.txt") as f:
        path = f.read().rstrip('\n').split(',')

    main_a(path)
    main_b(path)
