#! /usr/bin/env python3

def next_match_sum(digits):
    s = digits[0] if digits[0] == digits[-1] else 0
    for i in range(0, len(digits) - 1):
        if digits[i] == digits[i+1]:
            s += digits[i]
    return s


def half_match_sum(digits):
    # rather than summing over the whole list, twice the sum of the first half
    assert len(digits) % 2 == 0
    half = len(digits) // 2
    return sum(2*digits[i] for i in range(0, half) if digits[i] == digits[i+half] )


def main_a(digits):
    print(next_match_sum(digits))


def main_b(digits):
    print(half_match_sum(digits))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.read()

    digits = list(map(int, data.rstrip('\n')))
    main_a(digits)
    main_b(digits)
