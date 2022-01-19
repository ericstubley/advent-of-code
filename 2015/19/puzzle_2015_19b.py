#!/usr/bin/env python3

from collections import defaultdict

TOKENS = ['Al', 'Ar', 'B', 'C', 'Ca', 'F', 'H', 'Mg', 'N', 'O', 'P', 'Rn', 'Si', 'Th', 'Ti', 'Y']
assert len(TOKENS) == 16

if __name__ == "__main__":

    with open("medicine.txt") as f:
        medicine = f.read()

    # this test example should give 3 steps
    # medicine = "CRnFYFYFArCaF"

    # find how many of each token there are
    token_counts = defaultdict(int)
    for t in TOKENS:
        token_counts[t] += medicine.count(t)
    token_counts['C'] = 1

    # number of steps is number of tokens - all the Rn/Ar - twice the Y + 1
    total_tokens = sum([token_counts[__] for __ in token_counts])
    num_steps = total_tokens
    num_steps -= token_counts['Rn']
    num_steps -= token_counts['Ar']
    num_steps -= 2*token_counts['Y']
    num_steps -= 1

    print(num_steps)