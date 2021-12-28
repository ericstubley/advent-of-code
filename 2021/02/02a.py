#!/usr/bin/env python3

with open("input.txt", 'r') as f:
	data = f.readlines()

# separate out data into tuples of direction and distance
data = [(x.split(" ")[0], x.split(" ")[1].rstrip('\n')) for x in data]

dists = dict()
dists["forward"], dists["down"], dists["up"] = 0, 0, 0
for x in data:
	dists[x[0]] += int(x[1])

tot_f, tot_d, tot_u = dists["forward"], dists["down"], dists["up"]

print("distances: f={}, d={}, u={}".format(tot_f, tot_d, tot_u))
print("product of coordinates: {}".format(tot_f * (tot_d - tot_u)))