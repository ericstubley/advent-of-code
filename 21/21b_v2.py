#!/usr/bin/env python3

""" advent of code 2021 day 21

rules of the game:
- the two players move around a circular track with spaces labelled 1-10
- each turn you do some dice roll and then move that many spaces
- after moving you add your current space's value to your score
- for the dice roll you take the die in use, roll three times, add the rolls
- for part b, you roll a quantum d3; each die roll trifurcates the universe
- first player to 21 points wins in that universe

with starting positions 7 and 2, the goal is to find which players wins in 
more universes, and in how many universes they won

strategy: use a cached recursive solution to enumerate all possible universes.
caching is effective here because all you're doing is taking the positions of
the 2 players (10^2 = 100 total options) and their scores (21^2 = 441) as the
possible inputs, so you just need to store 44100 many values. small
simplification made in the fact that at each step you roll 3 times, but rather
than making a recursive call for each roll you figure out how many times each
possible total 3-9 happens among the 3^3 = 27 possibilities

"""

from functools import cache

WEIGHTS = {3:1, 4:3, 5:6, 6:7, 7:6, 8:3, 9:1}
WINNING_SCORE = 21

@cache
def worldtrack_win_count(curr_pos, curr_score, next_pos, next_score):
    # next_score is always the one that just got updated
    if next_score >= WINNING_SCORE:
        return 1, 0

    curr_wins, next_wins = 0, 0
    for total_roll in WEIGHTS:
        weight = WEIGHTS[total_roll] 

        new_pos = curr_pos + total_roll 
        # make sure the new position is in 1 through 10
        if new_pos > 10:
            new_pos -= 10
        new_score = curr_score + new_pos

        # recursive call to worldtrack_win_count flips roles of curr and next
        # because the current player flips
        next_track_wins, curr_track_wins = worldtrack_win_count(
                next_pos, next_score, new_pos, new_score)

        curr_wins += weight*curr_track_wins
        next_wins += weight*next_track_wins

    return curr_wins, next_wins

p1_pos, p2_pos = 7, 2
p1_wins, p2_wins = worldtrack_win_count(p1_pos, 0, p2_pos, 0)

winner = "1" if p1_wins > p2_wins else "2"
won_games = max(p1_wins, p2_wins)
total_games = p1_wins + p2_wins
output = "Player {} wins more games: {} out of {}".format(winner,
        won_games, total_games)

print(output)