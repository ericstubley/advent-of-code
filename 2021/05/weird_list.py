# got unexpected behaviour from the following
grid = [[0]*10]*10

grid[0][0] = 1

for row in grid:
	print(row)

# I expected to just have the top left corner be a 1
# instead you get the entire first column as 1s

# lists are objects, and objects are stored by reference
# so the inner list [0]*10 is a list with 10 copies of the int 0
# in particular each list
# but each element of the outer [...]*10 is a copy of the original list
# the original list being a pointer means you just copy the pointer
# so the new pointers are just different pointers to the same list

# so as you could have predicted, it makes a lot of sense at the end of the day!