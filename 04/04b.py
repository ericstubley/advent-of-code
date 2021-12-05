#!/usr/bin/env python3

from bingo import *

with open("input.txt", 'r') as f:
	data = f.readlines()

draws = data[0].rstrip('\n').split(',')
draws = [int(x) for x in draws]

boards = []
board = []
for i in range(2, len(data)):
	if data[i] == "\n":
		boards.append(Board(flatten(board)))
		board = []

	else:
		row = data[i].rstrip('\n').split(' ')
		row = [int(x) for x in row if x != '']
		board.append(row)


times, finals = [], []
for b in boards:
	time, final = b.win(draws)
	times.append(time)
	finals.append(final)

last = max(times)
which = times.index(last)
score = boards[which].score(draws)
print(score)
