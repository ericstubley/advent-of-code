#!/usr/bin/env python3

DIMN = 100

def neighbours(point):
	i, j = point[0], point[1]
	n_ls = []
	if i != 0:
		n_ls.append((i-1, j))
	if i != DIMN-1:
		n_ls.append((i+1, j))
	if j != 0:
		n_ls.append((i, j-1))
	if j != DIMN-1:
		n_ls.append((i, j+1))
	return n_ls

def find_basin(point, basin, smoke):
	ret_basin = basin | set([point])

	for n in neighbours(point):
		if smoke[n[0]][n[1]] != 9 and n not in ret_basin:
			ret_basin |= find_basin(n, ret_basin, smoke)

	return ret_basin

with open("input.txt") as f:
	data = f.readlines()

smoke = []
for x in data:
	smoke.append([int(d) for d in x.rstrip('\n')])

low_points = []
for i, row in enumerate(smoke):
	for j, s in enumerate(row):
		# for loops can have else clauses!
		# if the loop breaks you don't else, if it terminates you do
		for n in neighbours((i, j)):
			if s >= smoke[n[0]][n[1]]:
				break
		else:
			low_points.append((i, j))

basin_sizes = []
for point in low_points:
	basin = find_basin(point, set([point]), smoke)
	basin_sizes.append(len(basin))

basin_sizes.sort(reverse=True)
top_three_product = basin_sizes[0] * basin_sizes[1] * basin_sizes[2]

print(top_three_product)
