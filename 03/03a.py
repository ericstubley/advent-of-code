#!/usr/bin/env python3

# open the input and split into a list of lists of digits
with open("input.txt", 'r') as f:
	raw_data = f.readlines()

data = []
for x in raw_data:
	data.append([int(d) for d in x.rstrip('\n')])

# compute the gamma rate by summing the i-th digits
# gamma_i is one if that is greater than half the number of inputs
gamma = []
string_length = len(data[0])
for i in range(string_length):
	total_i = sum([x[i] for x in data])
	if total_i > (len(data)/2):
		gamma.append(1)	
	else:
		gamma.append(0)

# epsilon is just the bit flip of gamma
epsilon = [1-x for x in gamma]

# convert to decimal and print
# this is a total hack and there should be a better way
decimal_gamma = sum([(2**i)*gamma[-i-1] for i in range(string_length)])
decimal_epsilon = sum([(2**i)*epsilon[-i-1] for i in range(string_length)])

print("{}, {}".format(gamma, epsilon))
print("{}".format(decimal_gamma*decimal_epsilon))