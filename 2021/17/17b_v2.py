#!/usr/bin/env python3

# goal of this v2 is a more straightforward brute force approach that won't
# involve having to think through solving any quadratics
# I'm assuming the target area is always in +x, -y quadrant

# TARGET_X_MIN = 20
# TARGET_X_MAX = 30
# TARGET_Y_MIN = -10
# TARGET_Y_MAX = -5
TARGET_X_MIN = 70
TARGET_X_MAX = 125
TARGET_Y_MIN = -159
TARGET_Y_MAX = -121

def on_target(x, y):
    if x < TARGET_X_MIN or x > TARGET_X_MAX:
        return False
    if y < TARGET_Y_MIN or y > TARGET_Y_MAX:
        return False
    return True

# crude estimates on possible x and y bounds
x_lower = 0
x_upper = TARGET_X_MAX
y_lower = TARGET_Y_MIN
y_upper = -TARGET_Y_MIN # from part 1 analytic solution
max_time = max(2*TARGET_X_MAX, -2*TARGET_Y_MIN)

# precompute the x and y tracks for more efficient comparisons down the line
# only make the tracks as long as they need to be
x_tracks = dict()
for x in range(x_lower, x_upper+1):
    x_pos = 0
    track = []
    for t in range(max_time):
        if t <= x:
            x_pos += x - t
        if x_pos > TARGET_X_MAX:
            break
        track.append(x_pos)
        if len(track) >= 2 and track[-1] == track[-2] and track[-1] < TARGET_X_MIN:
            break
    x_tracks[x] = track

y_tracks = dict()
for y in range(y_lower, y_upper+1):
    y_pos = 0
    track = []
    for t in range(max_time):
        y_pos += y-t
        if y_pos < TARGET_Y_MIN:
            break
        track.append(y_pos)
    y_tracks[y] = track

# check to see if there's a common time
possible_velocities = set()
for ix in x_tracks:
    for iy in y_tracks:
        for x, y in zip(x_tracks[ix], y_tracks[iy]):
            if on_target(x, y):
                possible_velocities.add((ix, iy))
                break
print(len(possible_velocities))