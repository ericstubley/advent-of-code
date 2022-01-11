#!/usr/bin/env python3

def look_and_say(seq):
    new_seq = []
    i = 0
    while i < len(seq):
        digit = seq[i]
        count = 0
        while i < len(seq) and seq[i] == digit:
            count += 1
            i += 1

        new_seq.append(count)
        new_seq.append(digit)
    return new_seq


if __name__ == "__main__":
    start = "1321131112"
    seq = [int(__) for __ in start]

    for i in range(40):
        seq = look_and_say(seq)

    print(len(seq))