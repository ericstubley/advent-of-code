#!/usr/bin/env python3


def ribbon_required(l, w, h):
    plw, plh, pwh = 2*(l+w), 2*(l+h), 2*(w+h)
    wrapping = min(plw, plh, pwh)
    bow = l*w*h
    return wrapping + bow


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    dimensions = []
    for s in data:
        dimension = tuple
        dimensions.append(tuple(int(d) for d in s.rstrip('\n').split('x')))

    total_ribbon = 0
    for l, w, h in dimensions:
        total_ribbon += ribbon_required(l, w, h)

    print(total_ribbon)
