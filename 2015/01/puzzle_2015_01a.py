#!/usr/bin/env python3

def final_floor(instructions):
    floor = 0
    for i in instructions:
        if i == '(':
            floor += 1
        elif i == ')':
            floor -= 1
    return floor


if __name__ == "__main__":
    with open("input.txt") as f:
        instructions = f.read()

    print(final_floor(instructions))
