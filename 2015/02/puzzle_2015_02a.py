#!/usr/bin/env python3


def paper_required(l, w, h):
    lw, lh, wh = l*w, l*h, w*h
    slack = min(lw, lh, wh)
    return 2*lw + 2*lh + 2*wh + slack


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    dimensions = []
    for s in data:
        dimension = tuple
        dimensions.append(tuple(int(d) for d in s.rstrip('\n').split('x')))

    total_paper = 0
    for l, w, h in dimensions:
        total_paper += paper_required(l, w, h)

    print(total_paper)
