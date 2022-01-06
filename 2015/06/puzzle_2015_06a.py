#!/usr/bin/env python3

import numpy as np


def turn_off(lights):
    return np.zeros(np.shape(lights), dtype=bool)


def turn_on(lights):
    return np.ones(np.shape(lights), dtype=bool)


def toggle(lights):
    return np.logical_not(lights)


def get_range_from_text(string):
    split_string = string.split(" ")
    p1_string, p2_string = split_string[0], split_string[2]

    x1, y1 = [int(__) for __ in p1_string.split(",")]
    x2, y2 = [int(__)+1 for __ in p2_string.split(",")]

    return x1, y1, x2, y2



if __name__ == "__main__":
    with open("input.txt") as f:
        instructions = f.readlines()

    lights = np.zeros((1000, 1000), dtype=bool)

    for i in instructions:
        if i.startswith("turn off"):
            s = i.lstrip("turn off ").rstrip('\n')
            x1, y1, x2, y2 = get_range_from_text(s)

            lights[x1:x2, y1:y2] = turn_off(lights[x1:x2, y1:y2])

        elif i.startswith("turn on"):
            s = i.lstrip("turn on ")    
            x1, y1, x2, y2 = get_range_from_text(s)

            lights[x1:x2, y1:y2] = turn_on(lights[x1:x2, y1:y2])

        elif i.startswith("toggle"):
            s = i.lstrip("toggle ")    
            x1, y1, x2, y2 = get_range_from_text(s)

            lights[x1:x2, y1:y2] = toggle(lights[x1:x2, y1:y2])

    print(np.sum(lights))