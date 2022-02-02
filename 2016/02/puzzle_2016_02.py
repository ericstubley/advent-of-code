#! /usr/bin/env python3

DIR_TO_VEC = {'U': (0, 1), 'D': (0, -1), 'L': (-1, 0), 'R': (1, 0)}
KEYPAD_A = {(-1, 1): '1',
    (0, 1): '2',
    (1, 1): '3',
    (-1, 0): '4',
    (0, 0): '5',
    (1, 0): '6',
    (-1, -1): '7',
    (0, -1): '8',
    (1, -1): '9'}
KEYPAD_B = {(0, 0): '5',
    (1, 0): '6',
    (2, 0): '7',
    (3, 0): '8',
    (4, 0): '9',
    (1, 1): '2',
    (2, 1): '3',
    (3, 1): '4',
    (2, 2): '1',
    (1, -1): 'A',
    (2, -1): 'B',
    (3, -1): 'C',
    (2, -2): 'D'}


def parse(data):
    return [__.rstrip('\n') for __ in data]


def get_code_from_keypad(instructions, keypad):
    x, y = 0, 0
    ret = []
    for i in instructions:
        for c in i:
            dx, dy = DIR_TO_VEC[c]
            if (x + dx, y + dy) in keypad:
                x, y = x + dx, y + dy
        ret.append(keypad[(x, y)])
    return "".join(ret)


def main_a(instructions):
    print(get_code_from_keypad(instructions, KEYPAD_A))


def main_b(instructions):
    print(get_code_from_keypad(instructions, KEYPAD_B))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    instructions = parse(data)
    main_a(instructions)
    main_b(instructions)
