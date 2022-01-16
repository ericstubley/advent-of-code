#!/usr/bin/env python3

from aunt import *


ATTRIBUTES = {"children": 3,
"cats": 7,
"samoyeds": 2,
"pomeranians": 3,
"akitas": 0,
"vizslas": 0,
"goldfish": 5,
"trees": 3,
"cars": 2,
"perfumes": 1}


def is_viable(aunt):
    viable = True
    for a in ATTRIBUTES:
        if a in aunt.attributes and aunt.attributes[a] != ATTRIBUTES[a]:
            viable = False
            break
    return viable


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    aunts = [Aunt(line) for line in data]

    for aunt in aunts:
        if is_viable(aunt):
            print(aunt.number)
