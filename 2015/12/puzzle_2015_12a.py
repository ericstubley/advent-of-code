#!/usr/bin/env python3

def number_sum(string):

    total = 0
    curr = []

    for c in string:
        if curr == [] and (c == '-' or c.isdigit()):
            curr.append(c)
        elif curr != [] and c.isdigit():
            curr.append(c)
        elif curr != [] and not c.isdigit():
            number = int("".join(curr))
            total += number
            curr = []

    return total

if __name__ == "__main__":
    with open("input.txt") as f:
        json_data = f.read()

    print(number_sum(json_data))