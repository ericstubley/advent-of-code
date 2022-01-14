#!/usr/bin/env python3

from reindeer import *

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
   
    winning_distance = max([r.distance_travelled(2503) for r in herd])
    print(winning_distance)
