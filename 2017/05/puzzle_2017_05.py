#! /usr/bin/env python3

def count_jumps(rows, strange=False):
    i, count = 0, 0
    while 0 <= i and i < len(rows):
        new_i = i + rows[i]
        if strange and rows[i] >= 3:
            rows[i] -= 1
        else:
            rows[i] += 1
        count += 1
        i = new_i
    return count


def main_a(rows):
    print(count_jumps(rows))


def main_b(rows):
    print(count_jumps(rows, strange=True))


if __name__ == "__main__":
    with open("input.txt") as f:
        rows = [int(line.rstrip()) for line in f.readlines()]

    main_a([__ for __ in rows])
    main_b([__ for __ in rows])
