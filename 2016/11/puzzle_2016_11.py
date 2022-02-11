#! /usr/bin/env python3

from collections import deque, namedtuple
from itertools import combinations, product

GameState = namedtuple("GameState",
        field_names=["elevator", "n", "chips", "generators"])
IsomorphismState = namedtuple("IsomorphismState",
        field_names=["elevator", "n", "objects"])


def is_valid(gs):
    # indices of chips which aren't paired up
    unsafe_chips = [i for i in range(gs.n) if gs.chips[i] != gs.generators[i]]
    # floors which have generators on them
    unsafe_floors = set([gs.generators[i] for i in range(gs.n)])
    for i in unsafe_chips:
        if gs.chips[i] in unsafe_floors:
            return False
    return True


def get_isomorphism_class(gs):
    objects = tuple(sorted(zip(gs.chips, gs.generators)))
    return IsomorphismState(elevator=gs.elevator, n=gs.n, objects=objects)


def valid_moves(gs):
    curr_chips = [i for i in range(gs.n) if gs.chips[i] == gs.elevator]
    curr_gens = [i for i in range(gs.n) if gs.generators[i] == gs.elevator]

    if gs.elevator == 1:
        new_floors = [2]
    elif gs.elevator == 2:
        new_floors = [1, 3]
    elif gs.elevator == 3:
        new_floors = [2, 4]
    else:  # gs.elevator == 4
        new_floors = [3]

    chip_changes, gen_changes = [], []
    # single chip
    for c in curr_chips:
        chip_changes.append([c])
        gen_changes.append([])

    # two chips
    for c, d in combinations(curr_chips, 2):
        chip_changes.append([c, d])
        gen_changes.append([])

    # single gen
    for g in curr_gens:
        chip_changes.append([])
        gen_changes.append([g])

    # two gens
    for g, h in combinations(curr_gens, 2):
        chip_changes.append([])
        gen_changes.append([g, h])

    # chip gen pair
    for c in curr_chips:
        if c in curr_gens:
            chip_changes.append([c])
            gen_changes.append([c])

    move_list = []
    object_changes = zip(chip_changes, gen_changes)
    for e, o in product(new_floors, object_changes):
        cc, gc = o
        
        new_chips = list(gs.chips)
        for c in cc:
            new_chips[c] = e
        new_chips = tuple(new_chips)
        new_gens = list(gs.generators)
        for g in gc:
            new_gens[g] = e
        new_gens = tuple(new_gens)

        new_gs = GameState(elevator=e, n=gs.n, chips=new_chips, generators=new_gens)
        if is_valid(new_gs):
            move_list.append(new_gs)

    return move_list


def min_moves(start_state, final_state):

    curr = start_state
    curr_dist = 0
    distances = dict()

    curr_level = list()
    curr_level.append(curr)

    final_state_iso = get_isomorphism_class(final_state)

    reached_final = False
    while not reached_final:
        next_level = list()
        for gs in curr_level:

            iso_state = get_isomorphism_class(gs)

            if iso_state == final_state_iso:
                reached_final = True
                break


            if iso_state in distances:
                continue

            distances[iso_state] = curr_dist
            for new_gs in valid_moves(gs):
                next_level.append(new_gs)
        if not reached_final:
            curr_level = next_level
            curr_dist += 1

    return curr_dist


def main_a(start_state, final_state):
    print(min_moves(start_state, final_state))


def main_b(start_state, final_state):
    print(min_moves(start_state, final_state))


if __name__ == "__main__":

    # strontium, plutonium, thulium, ruthenium, curium
    start_state_a = GameState(elevator=1, n=5, chips=(1, 1, 3, 2, 2), generators=(1, 1, 2, 2, 2))
    final_state_a = GameState(elevator=4, n=5, chips=(4, 4, 4, 4, 4), generators=(4, 4, 4, 4, 4))

    # strontium, plutonium, thulium, ruthenium, curium, elerium, dilithium
    start_state_b = GameState(elevator=1, n=7, chips=(1, 1, 3, 2, 2, 1, 1), generators=(1, 1, 2, 2, 2, 1, 1))
    final_state_b = GameState(elevator=4, n=7, chips=(4, 4, 4, 4, 4, 4, 4), generators=(4, 4, 4, 4, 4, 4, 4))

    main_a(start_state_a, final_state_a)
    main_b(start_state_b, final_state_b)
