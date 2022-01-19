#!/usr/bin/env python3

from collections import defaultdict


def generate_molecules(string, rules):
    molecules = set()

    for i in range(len(string)):
        if string[i] in rules:
            for r in rules[string[i]]:
                molecules.add(operation(string, i, string[i], r)) 
        if i < len(string)-1 and string[i:i+2] in rules: 
            for r in rules[string[i:i+2]]:
                molecules.add(operation(string, i, string[i:i+2], r)) 

    return molecules

def operation(string, index, before, after):
    ret = [string[0:index], after, string[index+len(before):]]
    return "".join(ret)



if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    rules = defaultdict(list)
    for line in data:
        if line == "\n":
            break

        split = line.rstrip('\n').split(" ")
        rules[split[0]].append(split[2])


    string = data[-1].rstrip('\n')


    print(len(generate_molecules(string, rules)))