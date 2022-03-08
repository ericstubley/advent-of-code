#! /usr/bin/env python3

from collections import defaultdict, namedtuple


Instruction = namedtuple(typename="Instruction", 
        field_names=["register", "value", "comp_register", "comp_function"])


def parse_instructions(filename):
    instructions = []
    with open(filename) as f:
        for line in f:
            split = line.split()
            register = split[0]
            value = int(split[2]) if split[1] == "inc" else -1 * int(split[2])
            comp_register = split[4]
            comp_value = int(split[6]) 
            comp_function = make_comparison_function(split[5], comp_value)
            instructions.append(Instruction(register, value, comp_register, comp_function))
    return instructions


def make_comparison_function(operator, value):
    if operator == '<':
        return lambda x: x < value
    elif operator == '>':
        return lambda x: x > value
    elif operator == '<=':
        return lambda x: x <= value
    elif operator == '>=':
        return lambda x: x >= value
    elif operator == '==':
        return lambda x: x == value
    elif operator == '!=':
        return lambda x: x != value
    else:
        assert False


def run_instructions(instructions, report=False):
    registers = defaultdict(int)
    highest = 0
    for i in instructions:
        if i.comp_function(registers[i.comp_register]):
            registers[i.register] += i.value
            if report:
                highest = max(highest, registers[i.register])
    if report:
        return registers, highest
    else:
        return registers


def max_register(instructions):
    # be careful if the returned value is negative!
    # there may or may not be registers which didn't end up in the dict but
    # still have a value of 0
    registers = run_instructions(instructions)
    return max(registers.values())


def main_a(instructions):
    print(max_register(instructions))


def main_b(instructions):
    __, highest = run_instructions(instructions, report=True)
    print(highest)


if __name__ == "__main__":
    instructions = parse_instructions("input.txt")
    main_a(instructions)
    main_b(instructions)
