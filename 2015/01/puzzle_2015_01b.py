#!/usr/bin/env python3

def first_basement_visit(instructions):
    floor = 0
    fbv = None
    for pos, i in enumerate(instructions):
        if i == '(':
            floor += 1
        elif i == ')':
            floor -= 1

        if floor < 0:
            # need to add 1 because puzzle wants indexing start at 1
            fbv = pos + 1
            break
    return fbv


if __name__ == "__main__":
    with open("input.txt") as f:
        instructions = f.read()

    print(first_basement_visit(instructions))
