#!/usr/bin/env python3

with open("input.txt", 'r') as f:
	data = f.readlines()

raw_fish = [int(x) for x in data[0].rstrip('\n').split(',')]

fish = [raw_fish.count(i) for i in range(9)]

new_fish = [0]*9
for t in range(80):
	new_fish[6] = fish[0]
	new_fish[8] = fish[0]
	for i in range(1, 9):
		new_fish[i-1] += fish[i]

	fish = new_fish
	new_fish = [0]*9

total_fish = sum(fish)
print(total_fish)