#!/usr/bin/env python3

with open("input.txt") as f:
	data = f.readlines()

inputs, outputs = [], []
for x in data:
	# the first 10 elements are the inputs, the 11th is '|', the last 4 are outputs
	strings = x.rstrip('\n').split(' ')
	inputs.append(strings[0:10])
	outputs.append(strings[11:15])

num_unique_digits = len([x for line in outputs for x in line if len(x) in [2, 3, 4, 7]])
print(num_unique_digits)