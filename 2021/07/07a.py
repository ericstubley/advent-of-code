#!/usr/bin/env python3

with open("input.txt", 'r') as f:
	data = f.readlines()

# there are 1000 crabs
crabs = [int(x) for x in data[0].rstrip('n').split(',')]

crabs.sort()
# list length is even, but well-designed so that elements 499 and 500 are both 313
cost = sum([abs(x-313) for x in crabs])
print(cost)