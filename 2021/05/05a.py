#!/usr/bin/env python3

from line_segment import *

with open("input.txt", 'r') as f:
	data = f.readlines()

lines = []
for s in data:
	coords = s.rstrip('\n').split(" -> ")
	start = tuple(int(c) for c in coords[0].split(','))
	end = tuple(int(c) for c in coords[1].split(','))

	if start[1] == end[1]:
		lines.append(HorizontalLineSegment(start, end))
	elif start[0] == end[0]:
		lines.append(VerticalLineSegment(start, end))
	else:
		lines.append(LineSegment(start, end))

# grid = [[0]*1000]*1000 has very much not the expected behaviour!
grid = [[0]*1000 for x in range(1000)]

for l in lines:
	if l.is_horizontal() or l.is_vertical():
		for p in l:
			grid[p[0]][p[1]] += 1

count = sum([c >= 2 for row in grid for c in row])
print(count)
