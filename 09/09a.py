#!/usr/bin/env python3

with open("input.txt") as f:
	data = f.readlines()

smoke = []
for x in data:
	smoke.append([int(d) for d in x.rstrip('\n')])

total_risk = 0
for i, row in enumerate(smoke):
	for j, s in enumerate(row):
		# test each of the four directions for s NOT being a low point
		if i != 0 and s >= smoke[i-1][j]:
			pass
		elif i != len(smoke)-1 and s >= smoke[i+1][j]:
			pass
		elif j != 0 and s >= smoke[i][j-1]:
			pass
		elif j != len(row)-1 and s >= smoke[i][j+1]:
			pass
		else:
			total_risk += s+1	

print(total_risk)