#!/usr/bin/env python3

from functools import cache

WEIGHTS = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

@cache
def simulate_universe(p1, p2, s1, s2, turn):
    # pass positions, scores as 2 element lists for less repetition of code
    if max(s1, s2) >= 21:
        return (1, 0) if s1 > s2 else (0, 1)

    w1, w2 = 0, 0
    for roll, weight in WEIGHTS:
        new_p1, new_p2 = p1, p2
        new_s1, new_s2 = s1, s2
        new_turn = None
        if turn == 1:
            new_p1 = (p1 + roll) % 10
            new_s1 += new_p1 if new_p1 != 0 else 10
            new_turn = 2
        else: # turn == 2
            new_p2 = (p2 + roll) % 10
            new_s2 += new_p2 if new_p2 != 0 else 10
            new_turn = 1

        path_w1, path_w2 = simulate_universe(new_p1, new_p2, new_s1, new_s2, new_turn)

        w1 += weight*path_w1
        w2 += weight*path_w2

    return (w1, w2)




p1_position, p2_position = 7, 2
p1_wins, p2_wins = simulate_universe(p1_position, p2_position, 0, 0, 1)
print(p1_wins, p2_wins)
print(max(p1_wins, p2_wins))