#!/usr/bin/env python3

from functools import cache


@cache
def number_of_configurations(target, containers):
    if target == 0:
        return 1
    if len(containers) == 0:
        return 0

    ret = 0
    if containers[0] <= target:
        ret += number_of_configurations(target - containers[0], containers[1:])
    ret += number_of_configurations(target, containers[1:])
    return ret


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    containers = [int(__.rstrip('\n')) for __ in data]
    containers.sort()
    containers.reverse()
    containers = tuple(containers)


    print(number_of_configurations(150, containers))
