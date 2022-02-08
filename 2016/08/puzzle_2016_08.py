#! /usr/bin/env python3

import numpy as np


DISPLAY_SIZE = (6, 50)


def parse(data):
    instructions = []
    for line in data:
        split = line.rstrip("\n").split(" ")
        if split[0] == "rect":
            temp = split[1].split("x")
            height, width = int(temp[1]), int(temp[0]) 
            instructions.append(("rect", height, width))
        elif split[0] == "rotate":
            r_or_c = split[1]
            index = int(split[2][2:])
            distance = int(split[4])
            instructions.append(("rotate", r_or_c, index, distance))
    return instructions


def operation(instruction, display):
    if instruction[0] == "rect":
        rect(instruction[1], instruction[2], display)
    elif instruction[0] == "rotate" and instruction[1] == "row":
        rotate_row(instruction[2], instruction[3], display)
    elif instruction[0] == "rotate" and instruction[1] == "column":
        rotate_col(instruction[2], instruction[3], display)


def rect(height, width, display):
    display[0:height, 0:width] = True


def rotate_row(index, distance, display):
    display[index, :] = np.roll(display[index, :], distance)


def rotate_col(index, distance, display):
    display[:, index] = np.roll(display[:, index], distance)


def main_a(instructions):
    display = np.zeros(DISPLAY_SIZE, dtype=bool)
    for i in instructions:
        operation(i, display)

    print(display.sum()) 


def main_b(instructions):
    display = np.zeros(DISPLAY_SIZE, dtype=bool)
    for i in instructions:
        operation(i, display)

    for row in display:
        print("".join([" " if x == False else "#" for x in row]))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    instructions = parse(data)

    main_a(instructions)
    main_b(instructions)
