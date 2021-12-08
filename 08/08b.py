#!/usr/bin/env python3
import bitarray
from bidict import bidict

CHARS = ['a', 'b', 'c', 'd', 'e', 'f', 'g']

def string_to_bitarray(s):
	# given a string with at most one of the characters a through f
	# return a bitarray where a == first bit
	ba = bitarray.bitarray()
	for i in range(7):
		ba.append(CHARS[i] in s)
	# return a frozen version so it is hashable
	return bitarray.frozenbitarray(ba)

if __name__ == "__main__":
	with open("input.txt") as f:
		data = f.readlines()

	inputs, outputs = [], []
	for x in data:
		# the first 10 elements are the inputs, the 11th is '|', the last 4 are outputs
		strings = x.rstrip('\n').split(' ')
		inputs.append([string_to_bitarray(s) for s in strings[0:10]])
		outputs.append([string_to_bitarray(s) for s in strings[11:15]])

	running_sum = 0
	for i in range(len(inputs)):
		bijection = bidict()

		bijection[1] = [x for x in inputs[i] if x.count(1) == 2][0]
		bijection[4] = [x for x in inputs[i] if x.count(1) == 4][0]
		bijection[7] = [x for x in inputs[i] if x.count(1) == 3][0]
		bijection[8] = [x for x in inputs[i] if x.count(1) == 7][0]

		length_5s = [x for x in inputs[i] if x.count(1) == 5]
		length_6s = [x for x in inputs[i] if x.count(1) == 6]

		bd_mask = bijection[1] ^ bijection[4]	
		# 5 is the only one which has the segments of (4 xor 1)
		# 3 is the one which has the segments of 1 and isn't 5
		# 2 is the remaining one
		bijection[5] = [x for x in length_5s if x == (x | bd_mask)][0]
		bijection[3] = [x for x in length_5s if x == (x | bijection[1]) and x != bijection[5]][0]
		bijection[2] = [x for x in length_5s if x != bijection[5] and x != bijection[3]][0]

		# 9 is the only one which shares the segments of 4
		# 6 is the one which has the segments of 5 and isn't 9
		# 0 is the remaining one
		bijection[9] = [x for x in length_6s if x == (x | bijection[4])][0]
		bijection[6] = [x for x in length_6s if x == (x | bijection[5]) and x != bijection[9]][0]
		bijection[0] = [x for x in length_6s if x != bijection[9] and x != bijection[6]][0]

		for j in range(4):
			running_sum += bijection.inverse[outputs[i][j]] * (10**(3-j))

	print(running_sum)