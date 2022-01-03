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


def track_presents(path):
    tracker = {(0, 0): 1} 
    curr_pos = (0, 0)
    for direction in path:
        next_pos = next_position(curr_pos, direction)
        if next_pos in tracker:
            tracker[next_pos] += 1
        else:
            tracker[next_pos] = 1
        curr_pos = next_pos
    return tracker


def houses_visited(path):
    return len(track_presents(path))


if __name__ == "__main__":
    with open("input.txt") as f:
        path = f.read()

    path = path.rstrip('\n')
    print(houses_visited(path))
 