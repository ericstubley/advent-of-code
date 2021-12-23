#!/usr/bin/env python3

ENERGY = {'A':1, 'B':10, 'C':100, 'D':1000}
ANNEXES = [(0, 0), (1, 0), (9, 0), (10, 0)]
HALLS = [(3, 0), (5, 0), (7, 0)]
INTERMEDIATE = ANNEXES[0:2] + HALLS + ANNEXES[2:4]
ROOMS_A = [(2, 1), (2, 2), (2, 3), (2, 4)]
ROOMS_B = [(4, 1), (4, 2), (4, 3), (4, 4)]
ROOMS_C = [(6, 1), (6, 2), (6, 3), (6, 4)]
ROOMS_D = [(8, 1), (8, 2), (8, 3), (8, 4)]
ROOMS = ROOMS_A + ROOMS_B + ROOMS_C + ROOMS_D
COLUMNS = {'A':2, 'B':4, 'C':6, 'D':8}
FINAL_STRING = "AAAABBBBCCCCDDDD||......."
COST_DICT = {FINAL_STRING:0}


def state_string(state):
    s = ''
    for r in ROOMS:
        c = state[r] if state[r] != None else '.'
        s += c
    s += "||"
    for i in INTERMEDIATE:
        c = state[i] if state[i] != None else '.'
        s += c
    return(s)

def possible_moves(state, position):
    # return a list of the possible positions that the amphipod in the given
    # position could move to, by calling the relevant function 
    final_room = final_move_possible(state, position)

    # if you can move to the final room always do just that
    if final_room != False and final_room != position:
        return [final_room]
    # if you're already at your final room don't move anywhere
    elif final_room != False and final_room == position:
        return []
    # return a list of the possible ANNEX and HALL moves
    elif position in ROOMS:
        ret_ls = []
        for p in INTERMEDIATE:
            if path_clear(state, position, p):
                ret_ls.append(p)
        return ret_ls
    # otherwise you're in an INTERMEDIATE place and can't move to final one
    else:
        return []

def final_move_possible(state, position):
    # if amphipod at position can move to its final position return that
    # otherwise return False
    amphipod_type = state[position]
    c = COLUMNS[amphipod_type]

    # if you're in the correct spot already just return that
    # correct spot when correct column and everything below is correct
    if position[0] == c:
        for y in range(position[1], 5):
            if state[(c, y)] != amphipod_type:
                break
        else:
            return position

    # if movement needs to happen, check first to see if the room you want to
    # move to is ready for you to do that
    room_clear = True
    for y in range(1, 5):
        if state[(c, y)] != amphipod_type and state[(c, y)] != None:
            room_clear = False 
            break
    if not room_clear:
        return False

    # if the final room is ready get it's coordinates
    d = max([i for i in range(1, 5) if state[(c, i)] == None])
    final_room = (c, d)

    # if the path to the final room is clear return that, otherwise False
    if path_clear(state, position, final_room):
        return final_room
    else:
        return False

def path_clear(state, position_1, position_2):
    # if you're asking to not move anywhere
    if position_1 == position_2:
        return True

    if state[position_2] != None:
        return False

    # check if the spots above you are clear
    up_clear = True
    for y in range(1, position_1[1]):
        if state[(position_1[0], y)] != None:
           up_clear = False 
           break

    # check if the spots you need to move across are clear
    across_clear = True
    sign = 1 if position_1[0] < position_2[0] else -1
    for x in range(position_1[0] + sign, position_2[0], sign):
        if (x, 0) in INTERMEDIATE and state[(x, 0)] != None:
            across_clear = False
            break

    # check that you're going down into the right room column, and that you're
    # going down the right depth with everything below you correct
    down_clear = True
    amphipod_type = state[position_1]
    c = COLUMNS[amphipod_type]
    # if you're not going down don't do anything
    if position_2[1] == 0:
        pass
    # if you are going down and the wrong column return False
    elif position_2[1] > 0 and position_2[0] != c:
        down_clear = False
    # else you're going down the right column; check that it's a-okay
    else:
        for y in range(1, position_2[1]):
            if state[(c, y)] != None:
                down_clear = False
                break
        for y in range(position_2[1]+1, 5):
            if state[(c, y)] != amphipod_type:
                down_clear = False
                break

    return up_clear and across_clear and down_clear

def move_cost(state, position_1, position_2):
    assert position_1 != position_2
    total_x = abs(position_1[0] - position_2[0])
    total_y = position_1[1] + position_2[1]
    cost = ENERGY[state[position_1]]
    return cost*(total_x + total_y)

def is_finished(state):
    return state_string(state) == FINAL_STRING

def min_cost(state, depth):
    s = state_string(state)
    # print("  "*depth, s)
    if s in COST_DICT:
    #     print(s, COST_DICT[s], "from_dict")
        return COST_DICT[s]
    # if is_finished(state):
    #     return 0

    moves = []
    for p in state:
        if state[p] != None:
            p_moves = possible_moves(state, p)
            for m in p_moves:
                moves.append((p, m))

    # put in a default value for when there are no possible moves
    energy_costs = [10000000]
    for p, m in moves:
        cost = move_cost(state, p, m)
        new_state = state.copy()
        new_state[m] = state[p]
        new_state[p] = None
        energy_costs.append(cost + min_cost(new_state, depth+1))

    # if we got this far s is not in COST_DICT
    COST_DICT[s] = min(energy_costs)

    # print("  "*depth, s, min(energy_costs))
    return min(energy_costs)



# set up the initial state
state = dict()
for i in INTERMEDIATE:
    state[i] = None

# actual input
initial = ['C', 'D', 'D', 'B', 'B', 'C', 'B', 'D', 'D', 'B', 'A', 'A', 'A', 'A', 'C', 'C']
# test input
# initial = ['B', 'D', 'D', 'A', 'C', 'C', 'B', 'D', 'B', 'B', 'A', 'C', 'D', 'A', 'C', 'A']
# even simpler test input
# initial = ['B', 'A', 'A', 'A', 'C', 'D', 'B', 'B', 'B', 'C', 'C', 'C', 'D', 'A', 'D', 'D']
# even even simpler test input
# initial = ['B', 'A', 'A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C', 'C', 'D', 'D', 'D', 'D']
for i, r in enumerate(ROOMS):
    state[r] = initial[i]

print(min_cost(state, 0))

with open("output.txt", 'w') as f:
    for s in COST_DICT:
        f.write("{}, {}\n".format(s, COST_DICT[s]))