#!/usr/bin/env python3

from math import ceil, floor, sqrt

# TARGET_X_MIN = 20
# TARGET_X_MAX = 30
# TARGET_Y_MIN = -10
# TARGET_Y_MAX = -5
TARGET_X_MIN = 70
TARGET_X_MAX = 125
TARGET_Y_MIN = -159
TARGET_Y_MAX = -121

def total_y(y_0, t):
    return y_0*t - t*(t-1)/2

def total_x(x_0, t):
    t_cap = t if t <= x_0 else x_0
    return x_0*t_cap - t_cap*(t_cap-1)/2

def quadratic_pos_branch(initial, constant):
    a = 0.5
    b = -(initial + 0.5)
    c = constant
    try:
        return (-b + sqrt(b**2 - 4*a*c))/(2*a)
    except ValueError:
        pass


def quadratic_neg_branch(initial, constant):
    a = 0.5
    b = -(initial + 0.5)
    c = constant
    try:
        return (-b - sqrt(b**2 - 4*a*c))/(2*a)
    except ValueError:
        pass

# for each possible y_0 value make the set of times at which it lands in the
# target zone. You can precompute bounds on the set of possible y_0 since:
# - TARGET_Y_MIN as y_0 hits only on the first step
# - -TARGET_Y_MIN -1 as y_0 is the highest possible (from part a)
possible_y = dict()
for y_0 in range(TARGET_Y_MIN, -TARGET_Y_MIN):
    first_hit = ceil(quadratic_pos_branch(y_0, TARGET_Y_MAX))
    last_hit = floor(quadratic_pos_branch(y_0, TARGET_Y_MIN))
    possible_y[y_0] = frozenset([t for t in range(first_hit, last_hit+1)])
    '''print("y_0={}, 1st_t={}, 1st_pos={}, last_t={}, last_pos={}".format(
            y_0, first_hit, total_y(y_0, first_hit), last_hit, 
            total_y(y_0, last_hit)))'''


# for each possible x_0 value make the set of times at which it lands in the
# target zone. You can precompute bounds on the set of possible x_0 since:
# - TARGET_X_MAX as x_0 hits only on the first step
# - you need the stable point x_0(x_0+1)/2 to be at least TARGET_X_MIN
possible_x = dict()
x_stable_lower = ceil(quadratic_pos_branch(-1, -TARGET_X_MIN))
x_stable_upper = floor(quadratic_pos_branch(-1, -TARGET_X_MAX))
for x_0 in range(x_stable_lower, TARGET_X_MAX+1):
    first_hit = ceil(quadratic_neg_branch(x_0, TARGET_X_MIN))
    last_hit = quadratic_neg_branch(x_0, TARGET_X_MAX)
    if last_hit:
        last_hit = min(x_0, floor(last_hit))
    else:
        last_hit = x_0
    possible_x[x_0] = frozenset([t for t in range(first_hit, last_hit+1)])

# for each x_0, y_0 pair test if the sets of times overlap to count the total
# also for x_0 which stabilize inside the target zone, check to see if any
# of the y_0 will work for after the x has stabilized
possible_velocities = set()
for y_0 in possible_y:
    for x_0 in possible_x:
        # if there's an actual overlapping time
        if not possible_x[x_0].isdisjoint(possible_y[y_0]):
            possible_velocities.add((x_0, y_0)) 

        # if there's an overlap after x stabilizes
        if x_0 <= x_stable_upper and \
                True in [t >= x_0 for t in possible_y[y_0]]: 
            possible_velocities.add((x_0, y_0)) 

print(len(possible_velocities))