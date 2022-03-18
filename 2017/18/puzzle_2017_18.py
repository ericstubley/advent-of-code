#! /usr/bin/env python3

from collections import defaultdict, deque
from enum import Enum
# from automation.automation import submit_answer


class Instruction(Enum):
    SND = "snd"
    SET = "set"
    ADD = "add"
    MUL = "mul"
    MOD = "mod"
    RCV = "rcv"
    JGZ = "jgz"


def parse_instructions(filename):
    with open(filename) as f:
        data = f.readlines()

    instructions = []
    for line in data:
        app = []
        split = line.split()
        app.append(Instruction(split[0]))
        for token in split[1:]:
            if token.isalpha():
                app.append(token)
            else:
                app.append(int(token))
        instructions.append(tuple(app))
    return instructions


def first_recover(instructions):
    registers = defaultdict(int)

    sound = None
    idx = 0
    while idx < len(instructions):
        i = instructions[idx]
        enum = i[0]
        x = i[1]
        if len(i) > 2:
            y = registers[i[2]] if isinstance(i[2], str) else i[2]

        if enum == Instruction.SND:
            sound = registers[x] if isinstance(x, str) else x
        elif enum == Instruction.SET:
            registers[x] = y
        elif enum == Instruction.ADD:
            registers[x] += y
        elif enum == Instruction.MUL:
            registers[x] = registers[x] * y
        elif enum == Instruction.MOD:
            registers[x] = registers[x] % y 
        elif enum == Instruction.RCV:
            val = registers[x] if isinstance(x, str) else x
            if val != 0:
                break
        elif enum == Instruction.JGZ:
            val = registers[x] if isinstance(x, str) else x
            if val > 0:
                idx += y - 1

        idx += 1

    return sound


def number_of_sends(instructions):
    registers = {0: defaultdict(int), 1: defaultdict(int)}
    registers[0]['p'], registers[1]['p'] = 0, 1
    idxs = {0: 0, 1: 0}
    queues = {0: deque(), 1: deque()}
    waits = {0: False, 1: False}
    sents = {0: 0, 1: 0}
    terminates = {0: False,  1: False}
    count = 0


    while not all(terminates.values()):
        count += 1
        if count % 1000000 == 0:
            # print(f"\r{count}, {sents[0]}, {sents[1]}, {len(queues[0])}, {len(queues[1])}", end="")
            print(queues[0])
            print(queues[1])
        if all(waits.values()):
            terminates[0], terminates[1] = True, True
            continue
        if waits[0] and terminates[1]:
            terminates[0], terminates[1] = True, True
            continue
        if waits[1] and terminates[0]:
            terminates[0], terminates[1] = True, True
            continue

        for p in [0,1]: 
            i = instructions[idxs[p]]
            enum = i[0]
            x = i[1]
            if len(i) > 2:
                y = registers[p][i[2]] if isinstance(i[2], str) else i[2]

            if enum == Instruction.SND:
                val = registers[p][x] if isinstance(x, str) else x
                queues[1-p].append(val)
                sents[p] += 1
                waits[1-p] = False
            elif enum == Instruction.SET:
                registers[p][x] = y
            elif enum == Instruction.ADD:
                registers[p][x] += y
            elif enum == Instruction.MUL:
                registers[p][x] = registers[p][x] * y
            elif enum == Instruction.MOD:
                registers[p][x] = registers[p][x] % y 
            elif enum == Instruction.RCV:
                if waits[p] or len(queues[p]) == 0:
                    waits[p] = True
                    idxs[p] -= 1
                else:
                    val = queues[p].popleft()
                    registers[p][x] = val
            elif enum == Instruction.JGZ:
                val = registers[p][x] if isinstance(x, str) else x
                if val > 0:
                    idxs[p] += y - 1

            idxs[p] += 1

            if idxs[p] >= len(instructions):
                terminates[p] = True

    return sents[1]







        



def main_a(instructions):
    answer = first_recover(instructions)
    print(answer)
    # result = submit_answer(2017, 18, 1, answer)
    # print(result)


def main_b(instructions):
    answer = number_of_sends(instructions)
    print(answer)
    # result = submit_answer(2017, 18, 2, answer)
    # print(result)


if __name__ == "__main__":
    instructions = parse_instructions("input.txt")
    main_a(instructions)
    main_b(instructions)
