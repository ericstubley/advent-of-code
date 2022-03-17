#! /usr/bin/env python3

from collections import deque
# from automation.automation import submit_answer


def virtual_hurricane(time_limit, step):
    after_zero, curr = 0, 0
    for t in range(1, time_limit+1):
        if curr + step < t:
            curr = curr + step + 1
        else:
            curr = ((curr + step) % t) + 1

        if curr == 1:
            after_zero = t
    return after_zero


def hurricane(time_limit, step):
    ram = deque([0])

    curr = 0
    for t in range(1, time_limit+1):
        curr = ((curr + step) % len(ram)) + 1
        ram.insert(curr, t) 
    return list(ram)


def element_after(ls, element):
    i_element = ls.index(element)
    i_after = (i_element + 1) % len(ls)
    return ls[i_after]



def main_a():
    answer = element_after(hurricane(2017, 329), 2017)
    print(answer)
    # result = submit_answer(2017, 17, 1, answer)
    # print(result)


def main_b():
    answer = virtual_hurricane(50000000, 329)
    print(answer)
    # result = submit_answer(2017, 17, 2, answer)
    # print(result)


if __name__ == "__main__":
    main_a()
    main_b()
