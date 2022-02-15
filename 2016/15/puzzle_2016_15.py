#! /usr/bin/env python3

def parse_congruences(data):
    ret = []
    for line in data:
        split = line.rstrip('\n').split(' ')

        modulus = int(split[3])
        disc_num = int(split[1][1:])
        position = int(split[-1][:-1])

        ret.append((modulus, disc_num + position)) 

    return ret


def crt(congruences):
    # solve the first directly assuming congruences is not empty
    t = (- congruences[0][1]) % congruences[0][0]
    modulus = congruences[0][0]

    for m, a in congruences[1:]:
        while (t + a) % m != 0:
            t += modulus
        modulus *= m
    return t


def main_a(congruences):
    print(crt(congruences))


def main_b(congruences):
    print(crt(congruences))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    congruences = parse_congruences(data)
    main_a(congruences)
    congruences.append((11, 7+0))
    main_b(congruences)
