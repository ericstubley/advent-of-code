#!/usr/bin/env python3

# this is the problem where you start using numpy
import numpy as np


NEIGHBORS = np.array([[1, 1, 1], [1, 0, 1], [1, 1, 1]])

# single line to read in as array from file
octo_grid = np.genfromtxt("input.txt", 
		dtype=int, delimiter=1)

flash_count = 0
for t in range(0, 100):
	octo_grid += 1

	while True in (octo_grid > 9):
		flash_mask = octo_grid > 9
		# add to the count
		flash_count += flash_mask.sum()

		# increase everything non-zero and not already > 9 by number of adjacent flash_masks
		# for each grid element, if in [1,...,8], increase by number of flash_mask neighbors
		# for each flash_mask, add to each neighbour
		zero_mask = (np.pad(octo_grid, 1) == 0)

		adjacent = np.zeros(np.shape(zero_mask), dtype=int)
		for x, y in np.ndindex(np.shape(octo_grid)):
			if flash_mask[x][y] == True:
				adjacent[x:x+3, y:y+3] += NEIGHBORS
		adjacent *= ~zero_mask
		octo_grid += adjacent[1:-1, 1:-1]


		# set all the flash_mask things to 0
		octo_grid *= ~flash_mask

print(flash_count)