#! /usr/bin/env python3

REG_TO_NUM = {'a': 0, 'b': 1, 'c': 2, 'd': 3}


def parse(data):
    instructions = []
    for line in data:
        split = line.rstrip('\n').split(' ')
        if split[0] == "cpy":
            if split[1][0].isdigit():
                instructions.append(
                    ("cpy_n", int(split[1]), REG_TO_NUM[split[2]]))
            else:
                instructions.append(
                    ("cpy_v", REG_TO_NUM[split[1]], REG_TO_NUM[split[2]]))
        elif split[0] == "inc":
            instructions.append(("inc", REG_TO_NUM[split[1]]))
        elif split[0] == "dec":
            instructions.append(("dec", REG_TO_NUM[split[1]]))
        else:  # split[0] == "jnz"
            if split[1][0].isdigit():
                instructions.append(
                    ("jnz_n", int(split[1]), int(split[2])))
            else:
                instructions.append(
                    ("jnz_v", REG_TO_NUM[split[1]], int(split[2])))


    return instructions


def run_instructions(instructions, initial):
    index = 0
    assert len(initial) == 4
    registers = initial
    while index < len(instructions):
        i = instructions[index]
        if i[0] == "cpy_n":
            registers[i[2]] = i[1]
            index += 1
        elif i[0] == "cpy_v":
            registers[i[2]] = registers[i[1]]
            index += 1
        elif i[0] == "inc":
            registers[i[1]] += 1
            index += 1
        elif i[0] == "dec":
            registers[i[1]] -= 1
            index += 1
        else:  # split[0] == "jnz" 
            comp = i[1] if i[0] == "jnz_n" else registers[i[1]]
            if comp != 0:
                index += i[2]
            else:
                index += 1

    return registers


def main_a(instructions):
    print(run_instructions(instructions, [0, 0, 0, 0]))


def main_b(instructions):
    print(run_instructions(instructions, [0, 0, 1, 0]))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    instructions = parse(data)
    main_a(instructions)
    main_b(instructions)
