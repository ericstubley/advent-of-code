#!/usr/bin/env python3

from itertools import permutations


def happiness(seating, preferences):
    h = 0
    for i in range(len(seating)):
        left = seating[i]
        right = seating[0] if i == len(seating) - 1 else seating[i+1]

        h += preferences[left][right]
        h += preferences[right][left]
    return h


def find_optimal(preferences):
    names = list(preferences.keys())

    optimal = 0  # not actually a good choice
    for p in permutations(names):
        h = happiness(p, preferences)
        if h > optimal:
            optimal = h
    return optimal


def process_input(lines):
    preferences = {}

    for line in lines:
        split = line.rstrip('\n').split(' ')

        name_1 = split[0]
        name_2 = split[10][:-1]  # it ends in a period
        sign = -1 if split[2] == "lose" else 1
        amount = int(split[3])

        if name_1 in preferences:
            preferences[name_1][name_2] = sign*amount
        else:
            preferences[name_1] = {name_2: sign*amount}
    return preferences


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    preferences = process_input(data)

    print(find_optimal(preferences))
