#!/usr/bin/env python3

import itertools

from cuboid import *

with open("input.txt") as f:
    data = f.readlines()

# parse the input
instructions = []
for d in data:
    instruction = dict()

    value, coords = d.rstrip('\n').split(' ')
    instruction["value"] = True if value == 'on' else False

    coords = coords.split(',')
    c_ls = []
    for c in coords:
        axis_min, axis_max = c[2:].split('..')
        c_ls.append(int(axis_min))
        c_ls.append(int(axis_max))

    instruction["cuboid"] = Cuboid(c_ls[0], c_ls[1], c_ls[2], c_ls[3], c_ls[4], c_ls[5])
    instructions.append(instruction)

# maintain this as a list of DISJOINT cuboids which are on
cuboids = []

for index, i in enumerate(instructions):
    c = i["cuboid"]

    disjoint = [__ for __ in cuboids if not c.intersects(__)]
    intersect = [__ for __ in cuboids if c.intersects(__)]
    updated = []

    # no matter what the pieces from subdivisions of intersect which don't
    # touch c will end up as on. keep a disjoint list of the subdivided
    # pieces which are entirely inside c
    for d in intersect:
        subdivisions = d.subdivide(c)
        for e in subdivisions:
            if not c.intersects(e):
                updated.append(e)

    # we've found all the pieces that are disjoint from c and kept them
    # so if we're turning c off we've done thought. If we're turning c on
    # all that is needed is to add c, since it's already disjoint from
    # the rest of updated 
    if i["value"] == True:
        updated.append(c)

    # the list of on cuboids are those disjoint from c and those which have
    # been updated by this process. use extend rather than any other technique
    # for efficiency reasons
    disjoint.extend(updated)
    cuboids = disjoint


total_on = sum([c.num_points() for c in cuboids])
print(total_on)