#! /usr/bin/env python3

def parse(data):
    ret = []
    for line in data:
        split = line.rstrip('\n').split('-')
        lower, upper = int(split[0]), int(split[1])
        assert lower <= upper
        ret.append((lower, upper))
    return ret


def first_available(intervals):
    ordered = sorted(intervals)
    lower, upper = 0, 0
    for l, u in ordered:
        assert lower <= l
        if l <= upper+1:
            upper = max(upper, u)
        else:
            break

    return upper + 1


def num_allowed(intervals, maximum):
    ordered = sorted(intervals)
    count = 0
    lower, upper = 0, 0
    for l, u in ordered:
        assert lower <= l
        if l <= upper + 1:
            upper = max(upper, u)
        else:
            count += l - upper - 1
            lower, upper = l, u

    count += maximum - upper

    return count


def main_a(intervals):
    print(first_available(intervals))


def main_b(intervals):
    print(num_allowed(intervals, (1 << 32) - 1))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    intervals = parse(data)

    main_a(intervals)
    main_b(intervals)
