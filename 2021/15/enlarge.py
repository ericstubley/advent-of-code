#!/usr/bin/env python3

def wrap(n):
    if n >= 10:
        return n%9
    else:
        return n

with open("input.txt") as f:
    data = f.readlines()

risks = [[int(r) for r in row.rstrip('\n')] for row in data]
DIMN = len(risks[0])
large_risks = []

for row in risks:
    large_row = []
    for i in range(5):
        large_row += [wrap(r+i) for r in row]
    large_risks.append(large_row)

for j in range(1, 5):
    for i in range(DIMN):
        down_row = [wrap(r+j) for r in large_risks[i]]
        large_risks.append(down_row)

with open("input_enlarged.txt", 'w') as fout:
    for i in range(DIMN*5):
        fout.write(''.join([str(r) for r in large_risks[i]]) + '\n') 