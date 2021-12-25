#!/usr/bin/env python3

with open("input.txt") as f:
    data = f.readlines()

seafloor = [list(__.rstrip('\n')) for __ in data]
seafloor_past = None
height, width = len(seafloor), len(seafloor[0])
steps = 0


while True:
    movement = False

    # move the east facing sea cucumbers
    seafloor_past = [[__ for __ in row] for row in seafloor]
    for i, row in enumerate(seafloor_past):
        for j, sc in enumerate(seafloor_past[i]):
            east = sc == '>'
            empty = seafloor_past[i][(j+1)%width] == '.'
            if east and empty:
                seafloor[i][j] = '.'
                seafloor[i][(j+1)%width] = '>'
                movement = True


    # move the south facing sea cucumbers
    seafloor_past = [[__ for __ in row] for row in seafloor]
    for i, row in enumerate(seafloor_past):
        for j, sc in enumerate(seafloor_past[i]):
            south = sc == 'v'
            empty = seafloor_past[(i+1)%height][j] == '.'
            if south and empty:
                seafloor[i][j] = '.'
                seafloor[(i+1)%height][j] = 'v'
                movement = True


    steps += 1
    if not movement:
        break

print(steps)