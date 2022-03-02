#! /usr/bin/env python3

from itertools import combinations


def parse(data):
    ret = []
    for line in data:
        row = list(map(int, line.split()))
        ret.append(row)
    return ret


def checksum(rows):
    return sum(max(r) - min(r) for r in rows)


def quotient_checksum(rows):
    return sum(extract_quotient(r) for r in rows)

    
def extract_quotient(row):
    for a, b in combinations(row, 2):
        q1, r1 = divmod(b, a)
        if r1 == 0:
            return q1
        q2, r2 = divmod(a, b)
        if r2 == 0:
            return q2


def main_a(rows):
    print(checksum(rows))


def main_b(rows):
    print(quotient_checksum(rows))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    rows = parse(data)
    main_a(rows)
    main_b(rows)
