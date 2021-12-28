#!/usr/bin/env python3

def bit_ls_to_decimal(ls):
	# hack to manually compute decimal from list of bits
	return sum([(2**i)*ls[-i-1] for i in range(len(ls))])

def most_common_digit(ls):
	# find the most common entry in the list of 0s and 1s
	# this defaults to 1 if the list is equally split
	total = sum(ls)
	if total >= len(ls)/2:
		return 1
	else:
		return 0


if __name__ == "__main__":

	# open the input and split into a list of lists of digits
	with open("input.txt", 'r') as f:
		raw_data = f.readlines()

	data = []
	for x in raw_data:
		data.append([int(d) for d in x.rstrip('\n')])

	# LOOOOOL YOU FLIPPED CO2 AND OXYGEN BUT IT DIDN'T MATTER
	# find the co2_data by filtering the list until it has one thing left
	co2_data = data
	loc = 0
	while len(co2_data) > 1:
		test_digit = most_common_digit([x[loc] for x in co2_data])
		co2_data = [x for x in co2_data if x[loc] == test_digit]
		loc += 1	
	co2 = co2_data[0]

	# same thing for data except use 1 - most_common_digit
	oxy_data = data
	loc = 0
	while len(oxy_data) > 1:
		test_digit = 1 - most_common_digit([x[loc] for x in oxy_data])
		oxy_data = [x for x in oxy_data if x[loc] == test_digit]
		loc += 1
	oxy = oxy_data[0]

	decimal_co2 = bit_ls_to_decimal(co2)
	decimal_oxy = bit_ls_to_decimal(oxy)

	print("{}, {}, {}".format(decimal_co2, decimal_oxy, \
		decimal_co2*decimal_oxy))