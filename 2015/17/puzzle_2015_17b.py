#!/usr/bin/env python3

from functools import cache


@cache
def container_stats(target, containers):
    if target == 0:
        return [[]]
    elif len(containers) == 0:
        return None

    ret = []
    if containers[0] <= target:
        cs = container_stats(target - containers[0], containers[1:])
        if cs is not None:
            for __ in cs:
                ret.append(__ + [containers[0]]) 


    cs = container_stats(target, containers[1:])
    if cs is not None:
        for __ in cs:
            ret.append(__)

    return ret
        

if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    containers = [int(__.rstrip('\n')) for __ in data]
    containers.sort()
    containers.reverse()
    containers = tuple(containers)


    cs = container_stats(150, containers)
    m = min([len(__) for __ in cs])
    min_options = [__ for __ in cs if len(__) == m]
    print(m, len(min_options))
