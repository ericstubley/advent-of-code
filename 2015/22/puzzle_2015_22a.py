#!/usr/bin/env python3

from functools import cache
from spell import *

@cache
def min_mana_to_win(gs):
    if gs.player_hp <= 0 or gs.boss_hp <= 0:
        return (gs.boss_hp <= 0, 0)

    if gs.turn == "boss":
        return min_mana_to_win(advance_turn(gs, None))

    else:
        winning_outcomes = []
        for s in SPELLS:
            if s.is_castable(gs):
                outcome, spent = min_mana_to_win(advance_turn(gs, s))
                if outcome == True:
                    winning_outcomes.append((outcome, spent + s.cost))

        if winning_outcomes != []:
            ret = (True, min([x[1] for x in winning_outcomes]))
        else:
            ret = (False, 0)
        return ret


def advance_turn(gs, action):
    # do the ongoing effects
    for s in SPELLS:
        gs = s.effect(gs)

    if gs.turn == "player":
        assert action.is_castable(gs)
        post_cast = action.cast(gs)
        return post_cast._replace(turn="boss")

    else:  # gs.turn == "boss" 
        new_player_hp = gs.player_hp - gs.damage
        return gs._replace(player_hp=new_player_hp, turn="player")


if __name__ == "__main__":
    print(min_mana_to_win(INITIAL_GAME_STATE))
