#! /usr/bin/env python3

# from automation.automation import submit_answer
import time

FULL_16 = (1<<16) - 1
FULL_31 = (1<<31) - 1
SEED_A = 16807
SEED_B = 48271
START_A = 783
START_B = 325


class Generator:
    def __init__(self, seed, start, multiple=1):
        self.seed = seed
        self.start = start
        self.value = start
        self.multiple = multiple

    def __iter__(self):
        self.value = self.start
        return self

    def __next__(self):
        self.value = (self.value * self.seed) % FULL_31
        while (self.value % self.multiple) != 0:
            self.value = (self.value * self.seed) % FULL_31
        return self.value


def match_16_bits(a, b):
    return (a & FULL_16) == (b & FULL_16)


def judge(limit, gen_a, gen_b):
    count = 0
    for t in range(limit):
        if t % 100000 == 0:
            print(f"\r{t//100000} of {limit//100000}", end="")
        a, b = next(gen_a), next(gen_b)
        if match_16_bits(a, b):
            count += 1
    print("\r")
    return count


def main_a():
    gen_a = Generator(SEED_A, START_A)
    gen_b = Generator(SEED_B, START_B)
    time_1 = time.time()
    answer = judge(40000000, gen_a, gen_b)
    time_2 = time.time()
    print(answer, time_2 - time_1)
    # result = submit_answer(2017, 15, 1, answer)
    # print(result)


def main_b():
    gen_a = Generator(SEED_A, START_A, multiple=4)
    gen_b = Generator(SEED_B, START_B, multiple=8)
    time_1 = time.time()
    answer = judge(5000000, gen_a, gen_b)
    time_2 = time.time()
    print(answer, time_2 - time_1)
    # result = submit_answer(2017, 15, 2, answer)
    # print(result)


if __name__ == "__main__":

    main_a()
    main_b()
