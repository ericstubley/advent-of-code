#! /usr/bin/env python3

from itertools import product


def parse(data, grid_width):
    # inner list is 30
    # outer list is 32
    used, available = [], []
    row_u, row_a = [], []
    for i, line in enumerate(data):
        if i in [0, 1]:
            continue
        split = line.rstrip('\n').split()
        name = split[0].split('-')
        x, y = int(name[1][1:]), int(name[2][1:])


        total = int(split[1][:-1])
        u = int(split[2][:-1])
        a = int(split[3][:-1])
        assert total == u + a
        row_u.append(u)
        row_a.append(a)


        if y == grid_width-1:
            used.append(row_u)
            available.append(row_a)
            row_u, row_a = [], []

    return used, available


def count_viable_pairs(used, available):
    count = 0
    for iu, ju in product(range(32), range(30)):
        for ia, ja in product(range(32), range(30)):
            cond1 = (iu != ia) or (ju != ja)
            cond2 = used[iu][ju] > 0
            cond3 = available[ia][ja] >= used[iu][ju]

            if cond1 and cond2 and cond3:
                count += 1
    return count


def main_a(used, available):
    print(count_viable_pairs(used, available))


def main_b():
    print("31 to get to the goal and do the first swap, 5 each for a subsequent 30 swaps for a total of 181") 


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    used, available = parse(data, 30)


    for i in range(32):
        line = []
        for j in range(30):
            if i == 31 and j == 0:
                line.append('G')
            elif i == 26 and j == 22:
                line.append('_')
            elif used[i][j] >= 100:
                line.append('#')
            else:
                line.append('.')
        print("".join(line))


    main_a(used, available)
    main_b()
