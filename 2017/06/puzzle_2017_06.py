#! /usr/bin/env python3

def redistribute_blocks(banks):
    to_distribute = max(banks)
    idx = banks.index(to_distribute)
    banks[idx] = 0
    idx = (idx + 1) % len(banks)

    while to_distribute > 0:
        banks[idx] += 1
        to_distribute -= 1
        idx += 1
        if not idx < len(banks):
            idx = idx % len(banks)
    return banks


def time_to_repeated_state(banks):
    observed_states = dict()
    steps = 0
    while tuple(banks) not in observed_states:
        observed_states[tuple(banks)] = steps
        banks = redistribute_blocks(banks)
        steps += 1
    return steps, observed_states[tuple(banks)]


def main_a(banks):
    print(time_to_repeated_state(banks))


def main_b(banks):
    total, first_seen = time_to_repeated_state(banks)
    print(total - first_seen)


if __name__ == "__main__":
    banks = list(map(int, "0 5 10 0 11 14 13 4 11 8 8 7 1 4 12 11".split()))
    main_a([__ for __ in banks])
    main_b([__ for __ in banks])
