#! /usr/bin/env python3

from math import lcm
from automation.automation import submit_answer


def parse_input_to_ranges(filename):
    with open(filename) as f:
        data = f.readlines()

    ranges = []
    for line in data:
        split = line.split()
        level = int(split[0].rstrip(':'))
        scan_range = int(split[1])
        assert len(ranges) <= level
        while len(ranges) < level:
            ranges.append(0)
        ranges.append(scan_range)
    return ranges


def caught(ranges, time, delay=0):
    if time+delay == 0:
        return True
    elif ranges[time] != 0:
        return ((time + delay) % (2*ranges[time] - 2) == 0)
    else:
        return False


def trip_severity(ranges):
    severity = 0
    for t in range(len(ranges)):
        if caught(ranges, t):
            print(f"Caught at {t}")
            severity += t * ranges[t]
    return severity


def optimal_delay(ranges):
    delay = 0
    while True:
        caught_flag = False
        for t in range(len(ranges)):
            if caught(ranges, t, delay=delay):
                caught_flag = True
                break
        if not caught_flag:
            break
        delay += 1
    return delay


def main_a(ranges):
    answer = trip_severity(ranges)
    print(answer)
    # result = submit_answer(2017, 13, 1, answer)
    # print(result)


def main_b(ranges):
    answer = optimal_delay(ranges)
    print(answer)
    # result = submit_answer(2017, 13, 2, answer)
    # print(result)


if __name__ == "__main__":
    ranges = parse_input_to_ranges("input.txt")
    upper_bound = lcm(*(2*x-2 for x in ranges if x != 0))
    main_a(ranges)
    print(f"Upper bound on possible delay is {upper_bound}")
    main_b(ranges)
