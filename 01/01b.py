#!/usr/bin/env python3
WINDOW_SIZE = 3

# get the input
with open("input.txt", 'r') as f:
	depths = f.readlines()

# convert from strings to integers
depths = [int(x) for x in depths]

# make a new list which has the depth windows
# indexing check: WINDOW_SIZE=1 should just go over the whole list
# the list slice [0:1] is just a single element
depth_windows = []
for i in range(len(depths) - WINDOW_SIZE+1):
	window_sum = sum(depths[i:i+WINDOW_SIZE])
	depth_windows.append(window_sum)

# count the indices where depth window jumps up
count = len([i for i in range(1,len(depth_windows)) if depth_windows[i] > depth_windows[i-1]])
print(count)