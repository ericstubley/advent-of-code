#!/usr/bin/env python3

from collections import namedtuple


Instruction = namedtuple("Instruction", ["type", "register", "offset"])


def parse_instructions(raw_instructions):
    ret = []
    for line in raw_instructions:
        split = line.rstrip('\n').split(' ')

        instruction_type = split[0]
        if instruction_type == "jmp":
            offset = int(split[1])
            ret.append(Instruction(instruction_type, None, offset))
        elif instruction_type in ["jie", "jio"]:
            register = split[1][:-1]
            offset = int(split[2])
            ret.append(Instruction(instruction_type, register, offset))
        else:
            register = split[1]
            ret.append(Instruction(instruction_type, register, None))

    return ret

def run_program(instructions):
    a, b = 1, 0
    pointer = 0
    while pointer < len(instructions):
        i = instructions[pointer]
        if i.type == "hlf":
            if i.register == "a":
                a //= 2
            else:
                b //= 2
            pointer += 1

        elif i.type == "tpl":
            if i.register == "a":
                a *= 3
            else:
                b *= 3
            pointer += 1

        elif i.type == "inc":
            if i.register == "a":
                a += 1
            else:
                b += 1
            pointer += 1

        elif i.type == "jmp":
            pointer += i.offset

        elif i.type == "jie":
            if i.register == "a" and a%2 == 0:
                pointer += i.offset
            elif i.register == "b" and b%2 == 0:
                pointer += i.offset
            else:
                pointer += 1

        elif i.type == "jio":
            if i.register == "a" and a == 1:
                pointer += i.offset
            elif i.register == "b" and b == 1:
                pointer += i.offset
            else:
                pointer += 1

    return a, b


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    instructions = parse_instructions(data)

    a, b = run_program(instructions)

    print(a, b)
