#! /usr/bin/env python3

from enum import Enum
# from automation.automation import submit_answer


class Direction(Enum):
    U = 0
    R = 1
    D = 2
    L = 3

    def right(self):
        return Direction((self.value + 1) % 4) 

    def left(self):
        return Direction((self.value - 1) % 4)

    def opposite(self):
        return Direction((self.value + 2) % 4)

    def vector(self):
        return DIR_TO_VEC[self]


DIR_TO_VEC = {Direction.U: (-1, 0),
    Direction.R: (0, 1),
    Direction.D: (1, 0),
    Direction.L: (0, -1)}


class State(Enum):
    CLEAN = 0
    WEAK = 1
    INFECT = 2
    FLAG = 3

    def transition(self):
        return State((self.value + 1) % 4)


def parse_grid(filename):
    with open(filename) as f:
        data = f.readlines()

    grid = dict()
    for i, line in enumerate(data):
        for j, c in enumerate(line.rstrip('\n')):
            grid[i, j] = State.INFECT if c == '#' else State.CLEAN
    return grid


def burst(grid, x, y, d, intelligent=False):
    if (x, y) not in grid:
        grid[x, y] = State.CLEAN

    if grid[x, y] == State.CLEAN:
        d = d.left()
    elif grid[x, y] == State.INFECT:
        d = d.right()
    elif grid[x, y] == State.FLAG:
        d = d.opposite()
    elif grid[x, y] == State.WEAK:
        d = d

    grid[x, y] = grid[x, y].transition()
    if not intelligent:
        grid[x, y] = grid[x, y].transition()
    infect = 1 if grid[x, y] == State.INFECT else 0

    dx, dy = d.vector()
    x, y = x + dx, y + dy

    return x, y, d, infect


def evolve(grid, x, y, time, intelligent=False):
    d = Direction.U
    count = 0
    for t in range(time):
        if intelligent and t % 100000 == 0:
            print(f"\r{t//100000} of 100", end="")
        x, y, d, infect = burst(grid, x, y, d, intelligent)
        count += infect
    print("\r", end="")

    return x, y, d, count


def main_a(grid):
    __, __, __, answer = evolve(grid, 12, 12, 10000)
    print(answer)
    # result = submit_answer(2017, 22, 1, answer)
    # print(result)


def main_b(grid):
    __, __, __, answer = evolve(grid, 12, 12, 10000000, intelligent=True)
    print(answer)
    # result = submit_answer(2017, 22, 2, answer)
    # print(result)


if __name__ == "__main__":
    grid = parse_grid("input.txt")
    main_a(grid)
    grid = parse_grid("input.txt")
    main_b(grid)
