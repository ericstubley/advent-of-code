#! /usr/bin/env python3

from math import floor, sqrt

from automation.automation import submit_answer


def is_prime(n):
    # not efficient, but its fine
    ret = True
    for i in range(2, floor(sqrt(n))+1):
        if n % i == 0:
            ret = False
            break
    return ret


def count_composite(start, end, modulus):
    # count the numer of composite integers between start and end, inclusive
    # which are == start mod modulus
    count = 0
    for b in range(start, end+1, modulus):
        if not is_prime(b):
            count += 1
    return count


def main_a():
    answer = 77**2
    print(answer)
    # result = submit_answer(2017, 23, 1, answer)
    # print(result)


def main_b():
    answer = count_composite(107900, 124900, 17)
    print(answer)
    # result = submit_answer(2017, 23, 2, answer)
    # print(result)


if __name__ == "__main__":
    # main_a()
    main_b()
