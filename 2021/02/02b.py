#!/usr/bin/env python3

with open("input.txt", 'r') as f:
	data = f.readlines()

# separate out data into tuples of direction and distance
data = [(x.split(" ")[0], int(x.split(" ")[1].rstrip('\n'))) for x in data]

aim, tot_h, tot_v = 0, 0, 0
for x in data:
	if x[0] == "down":
		aim += x[1]
	elif x[0] == "up":
		aim -= x[1]
	elif x[0] == "forward":
		tot_h += x[1]
		tot_v += aim*x[1]

print("distances: h={}, v={}".format(tot_h, tot_v))
print("product of coordinates: {}".format(tot_h * tot_v))
