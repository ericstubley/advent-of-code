#!/usr/bin/env python3

from itertools import permutations


def parse_data(data):
    places = set()
    distances = dict()

    for line in data:
        s1, s2 = line.split(" = ")
        p1, p2 = s1.split(" to ")

        places.add(p1)
        places.add(p2)

        dist = int(s2)
        distances[(p1, p2)] = dist
        distances[(p2, p1)] = dist

    return places, distances


def longest_path(places, distances):
    path_lengths = [path_sum(__, distances) for __ in permutations(places)]
    return max(path_lengths)


def path_sum(path, distances):
    distance = 0
    for i in range(len(path) - 1):
        distance += distances[(path[i], path[i+1])]
    return distance


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    data = [__.rstrip('\n') for __ in data]
    places, distances = parse_data(data)

    print(longest_path(places, distances))
