#! /usr/bin/env python3

from bot import *


def parse(data):
    bots, outputs, inputs = dict(), dict(), list()
    for line in data:
        split = line.rstrip("\n").split(" ")
        if split[0] == "value":
            inputs.append((int(split[1]), int(split[5])))
        elif split[0] == "bot":
            # handle low and high
            handoffs = []
            for i in [5, 10]:
                num = int(split[i+1])
                if split[i] == "output":
                    if num not in outputs:
                        outputs[num] = Output(num)
                    handoffs.append(outputs[num])
                else:
                    if num not in bots:
                        bots[num] = Bot(num) 
                    handoffs.append(bots[num])

            # handle actor 
            actor_num = int(split[1])
            if actor_num not in bots:
                bots[actor_num] = Bot(actor_num)
            bots[actor_num].low_output = handoffs[0]
            bots[actor_num].high_output = handoffs[1]

        else:
            assert False
    return bots, outputs, inputs


def take_inputs(inputs, bots, outputs):
    for val, bot_num in inputs:
        bots[bot_num].add_value(val)


def main_a(bots, outputs, inputs):
    # the bot will print itself when it compares 17 and 61
    take_inputs(inputs, bots, outputs)


def main_b(bots, outputs, inputs):
    print(outputs[0].value * outputs[1].value * outputs[2].value)


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    bots, outputs, inputs = parse(data)
    main_a(bots, outputs, inputs)
    main_b(bots, outputs, inputs)
