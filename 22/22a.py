#!/usr/bin/env python3

import numpy as np

with open("input.txt") as f:
    data = f.readlines()

# parse the input
instructions = []
for d in data:
    instruction = dict()

    value, coords = d.rstrip('\n').split(' ')
    instruction['value'] = True if value == 'on' else False

    coords = coords.split(',')
    for c in coords:
        axis = c[0]
        axis_min, axis_max = c[2:].split('..')
        instruction[axis] = (int(axis_min), int(axis_max))
        assert int(axis_min) <= int(axis_max)

    instructions.append(instruction)


cubes = np.zeros((101, 101, 101), dtype=bool)

for i in instructions:
    bound_x = i['x'][0] <= 50 and i['x'][1] >= -50
    bound_y = i['y'][0] <= 50 and i['y'][1] >= -50
    bound_z = i['z'][0] <= 50 and i['z'][1] >= -50
    if not (bound_x and bound_y and bound_z):
        continue

    # si stands for slice_index
    si = [i['x'][0], i['x'][1]+1, i['y'][0], i['y'][1]+1, i['z'][0], i['z'][1]+1]
    si = [__+50 for __ in si]

    cubes[si[0]:si[1], si[2]:si[3], si[4]:si[5]] = i['value']

print(cubes.sum())