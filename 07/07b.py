#!/usr/bin/env python3

def cost(p, ls):
	return sum([int(abs(x - p)*(abs(x-p) + 1)/2) for x in ls])

with open("input.txt", 'r') as f:
	data = f.readlines()

# there are 1000 crabs
crabs = [int(x) for x in data[0].rstrip('n').split(',')]


total_crab_distances = sum(crabs)
average_crab = total_crab_distances/len(crabs)
# 461.611
print(average_crab)

# 461 is the right position
# in general you probably want some code to pick between floor(average_crab) and ceil(average_crab)
cost_461 = cost(461, crabs)
cost_462 = cost(462, crabs)

print(cost_461, cost_462)