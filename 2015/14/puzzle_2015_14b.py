#!/usr/bin/env python3

from reindeer import *


def scores_after(herd, time):
    scores = dict()
    for r in herd:
        scores[r.name] = 0

    for t in range(1, time+1):
        winning_distance = max([r.distance_travelled(t) for r in herd])
        winning_reindeer = [r for r in herd if r.distance_travelled(t) == winning_distance]

        for r in winning_reindeer:
            scores[r.name] += 1
    return scores


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    herd = []
    for line in data:
        split = line.rstrip('\n').split(' ')
        name = split[0]
        speed = int(split[3])
        endurance = int(split[6])
        recovery = int(split[13])

        herd.append(Reindeer(name, speed, endurance, recovery))

    final_scores = scores_after(herd, 2503)
    winning_score = max([final_scores[r.name] for r in herd])
    print(winning_score)
