#!/usr/bin/env python3

# get the input
with open("input.txt", 'r') as f:
	depths = f.readlines()

# convert from strings to integers
depths = [int(x) for x in depths]

# count the indices where depth jumps up
count = len([i for i in range(1,len(depths)) if depths[i] > depths[i-1]])
print(count)