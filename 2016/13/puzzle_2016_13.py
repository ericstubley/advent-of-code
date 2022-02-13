#! /usr/bin/env python3

from collections import deque


NEIGHBOR_DIRS = [(1, 0), (-1, 0), (0, 1), (0, -1)]


class GridSquare:
    def __init__(self, x, y, wall=True, visited=False, distance=None):
        self.x = x
        self.y = y
        self.wall = wall
        self.visited = visited
        self.distance = distance


def is_wall(x, y, favourite):
    val = x*x + 3*x + 2*x*y + y + y*y
    val += favourite

    count = 0
    while val > 0:
        if (val&1) == 1:
            count += 1
        val >>= 1

    return (count&1) == 1


def min_steps(goal_x, goal_y, favourite):
    # starting from 1, 1, BFS out to x, y
    grid = dict()

    curr_dist = 0
    found = False
    curr_level = list()
    curr_level.append((1, 1))

    while len(curr_level) > 0:
        next_level = list()
        for x, y in curr_level:
            # most of the time we can just keep going
            if (x, y) in grid and (grid[(x, y)].wall is True or grid[(x, y)].visited is True):
                continue
            if x < 0 or y < 0:
                continue

            # make sure we add to grid
            if (x, y) not in grid:
                wall = is_wall(x, y, favourite)
                if wall:
                    grid[(x, y)] = GridSquare(x, y)
                else:
                    grid[(x, y)] = GridSquare(x, y, wall, True, curr_dist)


            # check for target
            if x == goal_x and y == goal_y:
                assert grid[(x, y)].wall == False
                found = True
                break

            # if it's not a wall keep discovering in all dirs
            if grid[(x, y)].wall == False:
                for d_x, d_y in NEIGHBOR_DIRS:
                    next_level.append((x+d_x, y+d_y))


        if found:
            break

        curr_dist += 1
        curr_level = next_level

    return curr_dist


def locations_found(limit, favourite):
    # same idea, BFS out from 1, 1; tracking
    grid = dict()

    locations = 0
    curr_level = list()
    curr_level.append((1, 1))

    for curr_dist in range(limit+1):
        next_level = list()
        for x, y in curr_level:
            # most of the time we can just keep going
            if (x, y) in grid and (grid[(x, y)].wall is True or grid[(x, y)].visited is True):
                continue
            if x < 0 or y < 0:
                continue

            # make sure we add to grid
            if (x, y) not in grid:
                wall = is_wall(x, y, favourite)
                if wall:
                    grid[(x, y)] = GridSquare(x, y)
                else:
                    locations += 1
                    grid[(x, y)] = GridSquare(x, y, wall, True, curr_dist)


            # if it's not a wall keep discovering in all dirs
            if grid[(x, y)].wall == False:
                for d_x, d_y in NEIGHBOR_DIRS:
                    next_level.append((x+d_x, y+d_y))

        curr_level = next_level

    return locations



def main_a(goal_x, goal_y, favourite):
    print(min_steps(goal_x, goal_y, favourite))


def main_b(limit, favourite):
    print(locations_found(limit, favourite))


if __name__ == "__main__":
    favourite = 1352
    limit = 50
    goal_x, goal_y = 31, 39
    main_a(goal_x, goal_y, favourite)
    main_b(limit, favourite)
