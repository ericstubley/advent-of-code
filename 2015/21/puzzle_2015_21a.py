#!/usr/bin/env python3

from itertools import combinations, product
from character import *


WEAPONS = [{"name": "Dagger", "cost": 8, "damage": 4, "armor": 0}, 
{"name": "Shortsword", "cost": 10, "damage": 5, "armor": 0}, 
{"name": "Warhammer", "cost": 25, "damage": 6, "armor": 0}, 
{"name": "Longsword", "cost": 40, "damage": 7, "armor": 0}, 
{"name": "Greataxe", "cost": 74, "damage": 8, "armor": 0}] 

ARMORS = [{"name": "Nothing", "cost": 0, "damage": 0, "armor": 0},
{"name": "Leather", "cost": 13, "damage": 0, "armor": 1}, 
{"name": "Chainmail", "cost": 31, "damage": 0, "armor": 2}, 
{"name": "Splintmail", "cost": 53, "damage": 0, "armor": 3}, 
{"name": "Bandedmail", "cost": 75, "damage": 0, "armor": 4}, 
{"name": "Platemail", "cost": 102, "damage": 0, "armor": 5}]

RINGS = [{"name": "Damage +1", "cost": 25, "damage": 1, "armor": 0}, 
{"name": "Damage +2", "cost": 50, "damage": 2, "armor": 0}, 
{"name": "Damage +3", "cost": 100, "damage": 3, "armor": 0}, 
{"name": "Defense +1", "cost": 20, "damage": 0, "armor": 1}, 
{"name": "Defense +2", "cost": 40, "damage": 0, "armor": 2}, 
{"name": "Defense +3", "cost": 80, "damage": 0, "armor": 3}] 


if __name__ == "__main__":

    winning_characters = []
    boss = Character(100, 8, 2)

    for w, a in product(WEAPONS, ARMORS):
        # no rings
        c = Character(100, 0, 0)
        c.equip(w)
        c.equip(a)
        if c.beats(boss):
            winning_characters.append(c)
            if c.spent == 91:
                print(w["name"], a["name"])

        # 1 ring
        for r in RINGS:
            c = Character(100, 0, 0)
            c.equip(w)
            c.equip(a)
            c.equip(r)
            if c.beats(boss):
                winning_characters.append(c)
                if c.spent == 91:
                    print(w["name"], a["name"], r["name"])
        
        # 2 rings
        for r1, r2 in combinations(RINGS, 2):
            c = Character(100, 0, 0)
            c.equip(w)
            c.equip(a)
            c.equip(r1)
            c.equip(r2)
            if c.beats(boss):
                winning_characters.append(c)
                if c.spent == 91:
                    print(w["name"], a["name"], r1["name"], r2["name"])


    print(min([__.spent for __ in winning_characters]))
