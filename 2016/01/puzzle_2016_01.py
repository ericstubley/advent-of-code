#! /usr/bin/env python3

LEFT_TURN = {'N': 'W', 'W': 'S', 'S': 'E', 'E': 'N'}
RIGHT_TURN = {'N': 'E', 'E': 'S', 'S': 'W', 'W': 'N'}
DIR_TO_VECTOR = {'N': (0, 1), 'E': (1, 0), 'S': (0, -1), 'W': (-1, 0)}


def parse_instructions(string):
    ls = string.rstrip("\n").split(", ")
    ls = [(s[0], int(s[1:])) for s in ls]
    return ls


def total_distance(instructions):
    x, y, d = 0, 0, 'N'
    for turn, dist in instructions:
        if turn == 'L':
            d = LEFT_TURN[d]
        else:
            d = RIGHT_TURN[d]

        if d == 'N':
            y += dist
        elif d == 'E':
            x += dist
        elif d == 'S':
            y -= dist
        elif d == 'W':
            x -= dist

    return abs(x) + abs(y)


def first_visit_twice(instructions):
    x, y, d = 0, 0, 'N'
    visited = set()
    visited.add((0, 0))
    for turn, dist in instructions:
        if turn == 'L':
            d = LEFT_TURN[d]
        else:
            d = RIGHT_TURN[d]

        dir_x, dir_y = DIR_TO_VECTOR[d]

        revisit = False
        for __ in range(1, dist+1):
            x, y = x + dir_x, y + dir_y

            if (x, y) in visited:
                revisit = True
                break
            else:
                visited.add((x, y))
        if revisit:
            break

    return abs(x) + abs(y)

def main_a():
    with open("input.txt") as f:
        data = f.read()

    instructions = parse_instructions(data)
    print(total_distance(instructions))

def main_b():
    with open("input.txt") as f:
        data = f.read()

    instructions = parse_instructions(data)
    print(first_visit_twice(instructions))

if __name__ == "__main__":
    main_a()
    main_b()
