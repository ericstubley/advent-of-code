#!/usr/bin/env python3

def next_position(curr_pos, direction):
    x, y = curr_pos
    if direction == '>':
        x += 1
    elif direction == '<':
        x -= 1
    elif direction == '^':
        y += 1
    elif direction == 'v':
        y -= 1
    return (x, y)


def split_track_presents(path):
    tracker = {(0, 0): 2} 

    real_path, robo_path = path[0::2], path[1::2]

    curr_pos = (0, 0)
    for direction in real_path:
        next_pos = next_position(curr_pos, direction)
        if next_pos in tracker:
            tracker[next_pos] += 1
        else:
            tracker[next_pos] = 1
        curr_pos = next_pos

    curr_pos = (0, 0)
    for direction in robo_path:
        next_pos = next_position(curr_pos, direction)
        if next_pos in tracker:
            tracker[next_pos] += 1
        else:
            tracker[next_pos] = 1
        curr_pos = next_pos

    return tracker


def split_houses_visited(path):
    return len(split_track_presents(path))


if __name__ == "__main__":
    with open("input.txt") as f:
        path = f.read()

    path = path.rstrip('\n')
    print(split_houses_visited(path))
 