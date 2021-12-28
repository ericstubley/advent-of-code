#!/usr/bin/env python3

with open("input.txt", 'r') as f:
	data = f.readlines()

raw_fish = [int(x) for x in data[0].rstrip('\n').split(',')]

fish = [raw_fish.count(i) for i in range(9)]

# you can fix the pattern of having to reset new_fish twice by putting it at the start of the loop
for t in range(256):
	new_fish = [0]*9
	new_fish[6] = fish[0]
	new_fish[8] = fish[0]
	for i in range(1, 9):
		new_fish[i-1] += fish[i]
	fish = new_fish

total_fish = sum(fish)
print(total_fish)
