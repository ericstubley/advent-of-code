#! /usr/bin/env python3

from copy import deepcopy


REG_TO_NUM = {'a': 0, 'b': 1, 'c': 2, 'd': 3}

def parse(data):
    instructions = []
    for line in data:
        split = line.rstrip('\n').split(' ')
        if split[0] in ["inc", "dec", "out"]:
            args = [1]
        elif split[0] in ["cpy", "jnz"]:
            args = [1, 2]
        else:
            print(f"Something bad happened while parsing {line}")
            assert False

        i = [split[0]]
        for arg in args:
            if split[arg].isalpha():
                i.append(True)
                i.append(REG_TO_NUM[split[arg]])
            else:
                i.append(False)
                i.append(int(split[arg]))
        instructions.append(i)

    return instructions


def run_instructions(instructions, initial, output=False):
    index = 0
    assert len(initial) == 4
    registers = [x for x in initial]
    while 0 <= index and index < len(instructions):
        if output:
            print(registers)
        i = instructions[index]

        if i[0] == "inc":
            if i[1] is True:
                registers[i[2]] += 1
        elif i[0] == "dec":
            if i[1] is True:
                registers[i[2]] -= 1
        elif i[0] == "out":
            if i[1] is True:
                print(registers[i[2]])
            else:
                print(i[2])
        elif i[0] == "cpy":
            if i[3] is True:
                registers[i[4]] = registers[i[2]] if i[1] else i[2]
        elif i[0] == "jnz":
            cond_value = registers[i[2]] if i[1] else i[2]
            if cond_value != 0:
                jump_value = registers[i[4]] if i[3] else i[4]
                # add jump_value - 1 so that the index += 1 corrects
                index += jump_value - 1
        index += 1

    return registers


def main_a(instructions):
    x = 0b101010101010
    print(run_instructions(instructions, [x - 7*365, 0, 0, 0]))


def main_b(instructions):
    print(run_instructions(instructions, [12, 0, 0, 0]))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    instructions = parse(data)

    # feed a new copy of instructions because it's going to get modified
    main_a(deepcopy(instructions))
    # main_b(deepcopy(instructions))