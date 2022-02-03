#! /usr/bin/env python3

def parse(data):
    ret = []
    for x in data:
        ls = x.strip().split()
        ret.append((int(ls[0]), int(ls[1]), int(ls[2])))
    return ret


def column_parse(data):
    ret = []
    for i in range(0, len(data), 3):
        r1, r2, r3 = data[i], data[i+1], data[i+2]
        s1, s2, s3 = r1.split(), r2.split(), r3.split()

        ret.append((int(s1[0]), int(s2[0]), int(s3[0])))
        ret.append((int(s1[1]), int(s2[1]), int(s3[1])))
        ret.append((int(s1[2]), int(s2[2]), int(s3[2])))
    return ret


def is_valid_triangle(t):
    m = max(t)
    others = sum(t) - m
    return others > m


def main_a(triangles):
    print(len([t for t in triangles if is_valid_triangle(t)]))


def main_b(triangles):
    print(len([t for t in triangles if is_valid_triangle(t)]))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    triangles_a = parse(data)
    triangles_b = column_parse(data)

    main_a(triangles_a)
    main_b(triangles_b)
