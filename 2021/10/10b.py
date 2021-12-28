#!/usr/bin/env python3

PAREN_MATCHING = {'(':')', '[':']', '{':'}', '<':'>'}
PAREN_TO_SCORE = {'(':1, '[':2, '{':3, '<':4}


def is_incomplete(line):
	paren_stack = []
	for p in line:
		if p in PAREN_MATCHING:
			paren_stack.append(p)
		elif p == PAREN_MATCHING[paren_stack[-1]]:
			paren_stack.pop()
		else:
			return False
	return paren_stack


with open("input.txt") as f:
	data = f.readlines()

code = [x.rstrip('\n') for x in data]

scores = []
for line in code:
	final_stack = is_incomplete(line)
	if final_stack:
		running_score = 0
		while final_stack:
			running_score *= 5
			running_score += PAREN_TO_SCORE[final_stack.pop()]
		scores.append(running_score)

print(scores[int(len(scores)/2)])
