#!/usr/bin/env python3

p1_position, p2_position = 7, 2
p1_score, p2_score = 0, 0
num_rolls = 0
turn = 1


while max(p1_score, p2_score) < 1000:
    to_add = 3*num_rolls + 6
    num_rolls += 3

    if turn == 1:
        p1_position = (p1_position + to_add) % 10
        p1_score += p1_position if p1_position != 0 else 10
        turn = 2
    else: # turn == 2
        p2_position = (p2_position + to_add) % 10
        p2_score += p2_position if p2_position != 0 else 10
        turn = 1
print(min(p1_score, p2_score)*num_rolls)