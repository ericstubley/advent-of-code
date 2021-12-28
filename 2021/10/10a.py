#!/usr/bin/env python3

PAREN_MATCHING = {'(':')', '[':']', '{':'}', '<':'>'}
PAREN_TO_SCORE = {')':3, ']':57, '}':1197, '>':25137}

with open("input.txt") as f:
	data = f.readlines()

code = [x.rstrip('\n') for x in data]

score = 0
for line in code:
	paren_stack = []
	for p in line:
		if p in PAREN_MATCHING:
			paren_stack.append(p)
		elif p == PAREN_MATCHING[paren_stack[-1]]:
			paren_stack.pop()
		else:
			score += PAREN_TO_SCORE[p]
			break

print(score)