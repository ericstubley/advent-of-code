#! /usr/bin/env python3

from hashlib import md5

OPEN_CHARS = ['b', 'c', 'd', 'e', 'f']


def path_to_position(path):
    # assume we don't go over walls
    x, y = 0, 0
    for d in path:
        if d == "U":
            y -= 1
        elif d == "D":
            y += 1
        elif d == "L":
            x -= 1
        elif d == "R":
            x += 1
    return x, y


def open_paths(password, path):
    h = md5((password + path).encode()).hexdigest()[0:4]
    x, y = path_to_position(path)
    ret = []
    if h[0] in OPEN_CHARS and y != 0:
        ret.append("U")
    if h[1] in OPEN_CHARS and y != 3:
        ret.append("D")
    if h[2] in OPEN_CHARS and x != 0:
        ret.append("L")
    if h[3] in OPEN_CHARS and x != 3:
        ret.append("R")
    return ret


def find_paths(password, shortest_only=False):
    curr_level = [""]

    paths = []
    level = 0
    while len(curr_level) > 0:
        if level % 10 == 0:
            print(f"Level {level}; exploring {len(curr_level)} options for {password}")
        next_level = []
        for p in curr_level:
            x, y = path_to_position(p)
            if x == 3 and y == 3:
                paths.append(p)
                if shortest_only:
                    break
            else:
                options = open_paths(password, p)
                for d in options:
                    next_level.append(p+d)

        curr_level = next_level
        level += 1
        if shortest_only and len(paths) != 0:
            break

    if shortest_only:
        return paths[0]
    else:
        return paths


def longest_path(password):
    longest = find_paths(password, shortest_only=False)[-1]
    return longest


def main_a(password):
    print(find_paths(password, shortest_only=True))


def main_b(password):
    print(len(longest_path(password)))


if __name__ == "__main__":
    password = "pvhmgsws"
    main_a(password)
    main_b(password)
