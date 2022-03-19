#! /usr/bin/env python3

from enum import Enum
from automation.automation import submit_answer


class Direction(Enum):
    U = (-1, 0)
    D = (1, 0)
    L = (0, -1)
    R = (0, 1)
    C = (0, 0)
    END = None

SYMBOLS = {Direction.U: '|', 
    Direction.D: '|',
    Direction.L: '-',
    Direction.R: '-',
    Direction.C: '+',
    Direction.END: ' '}

REVERSE = {Direction.U: Direction.D,
    Direction.D: Direction.U,
    Direction.L: Direction.R,
    Direction.R: Direction.L}


def parse_path(filename):
    with open(filename) as f:
        path = []
        for line in f:
            path.append(line.rstrip('\n'))
    return path


def find_start(path):
    return 0, path[0].index(SYMBOLS[Direction.D])


def move(path, x, y, direction):
    assert direction != Direction.C
    assert direction != Direction.END

    dx, dy = direction.value
    nx, ny = x + dx, y + dy

    if path[nx][ny] == "+":
        # compute new direction
        options = set([Direction.U, Direction.D, Direction.L, Direction.R])
        options.remove(REVERSE[direction])

        for d in options:
            dx, dy = d.value
            tx, ty = nx + dx, ny + dy
            if path[tx][ty].isalpha() or path[tx][ty] == SYMBOLS[d]:
                ndirection = d
                break

    elif path[nx][ny].isalpha() or path[nx][ny] == "-" or path[nx][ny] == "|":
        ndirection = direction
    else:
        ndirection = Direction.END

    return nx, ny, ndirection


def walk_path(path):
    x, y = find_start(path)
    direction = Direction.D

    letters = []
    steps = 0
    while direction != Direction.END:
        x, y, direction = move(path, x, y, direction)
        if path[x][y].isalpha():
            letters.append(path[x][y])
        steps += 1

    return "".join(letters), steps


def main_a(path):
    answer, __ = walk_path(path)
    print(answer)
    # result = submit_answer(2017, 19, 1, answer)
    # print(result)


def main_b(path):
    __, answer = walk_path(path)
    print(answer)
    # result = submit_answer(2017, 19, 2, answer)
    # print(result)


if __name__ == "__main__":
    path = parse_path("input.txt")
    main_a(path)
    main_b(path)
