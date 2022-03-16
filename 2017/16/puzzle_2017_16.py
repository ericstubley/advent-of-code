#! /usr/bin/env python3

from functools import partial
from automation.automation import submit_answer

def parse_steps(filename):
    with open(filename) as f:
        data = f.read()

    index_steps, char_steps = [], []
    data = data.split(',')
    for raw in data:
        if raw[0] == 's':
            k = int(raw[1:])
            index_steps.append(partial(spin, k=k))
        elif raw[0] == 'x':
            i, j = map(int, raw[1:].split('/'))
            index_steps.append(partial(exchange, i=i, j=j)) 
        elif raw[0] == 'p':
            a, b = raw[1:].split('/')
            char_steps.append(partial(partner, a=a, b=b))
        else:
            assert False
    return index_steps, char_steps


def spin(dancers, k):
    k = k % len(dancers)
    return dancers[-k:] + dancers[:-k] 


def exchange(dancers, i, j):
    i, j = min(i, j), max(i, j)
    return dancers[:i] + dancers[j] + dancers[i+1:j] + dancers[i] + dancers[j+1:]


def partner(dancers, a, b):
    return dancers.replace(a, '_').replace(b, a).replace('_', b)


def dance(dancers, steps):
    order = dancers
    for step in steps:
        order = step(order)
    return order


def repeat_dance(dancers, index_steps, char_steps, repeat):
    one_dance_index = dance(dancers, index_steps)
    sigma = extract_permutation(dancers, one_dance_index)
    one_dance_char = dance(dancers, char_steps)
    tau = extract_permutation(dancers, one_dance_char, index=False)
    repeated_sigma = power_permutation(sigma, repeat)
    repeated_tau = power_permutation(tau, repeat, index=False)
    repeated_index_dance = apply_index_permutation(repeated_sigma, dancers)
    repeated_dance = apply_char_permutation(repeated_tau, repeated_index_dance)
    return repeated_dance


def extract_permutation(start, end, index=True):
    """ return a dict i -> sigma(i), where sigma is the permutation inducing
    start -> end """
    p = dict()
    for i, c in enumerate(start):
        if index:
            p[i] = end.index(c)
        else:
            p[end[i]] = c
    return p


def power_permutation(sigma, n, index=True):
    """ compute sigma^n using the repeated exponentiation trick """
    powers = [sigma]
    for i in range(1, n.bit_length()):
        powers.append(compose_permutation(powers[-1], powers[-1]))


    ret = identity_permutation(len(sigma), index=index)
    for i in range(n.bit_length()):
        if (n >> i) % 2 == 1:
            ret = compose_permutation(powers[i], ret)
    return ret


def identity_permutation(n, index=True):
    if index:
        return dict((i, i) for i in range(n))
    else:
        return dict((chr(ord('a') + i), chr(ord('a') + i) )for i in range(n))


def compose_permutation(sigma, tau):
    composed = dict()
    for i in tau:
        composed[i] = sigma[tau[i]]
    return composed


def apply_index_permutation(sigma, dancers):
    ret = [None]*len(dancers)
    for i in range(len(dancers)):
        ret[sigma[i]] = dancers[i]
    return "".join(ret)


def apply_char_permutation(tau, dancers):
    ret = [None]*len(dancers)
    for c in tau:
        ret[dancers.index(tau[c])] = c
    return "".join(ret)


def main_a(index_steps, char_steps):
    answer = dance(dance("abcdefghijklmnop", index_steps), char_steps)
    print(answer)
    # result = submit_answer(2017, 16, 1, answer)
    # print(result)


def main_b(index_steps, char_steps):
    repeat = 10**9
    answer = repeat_dance("abcdefghijklmnop", index_steps, char_steps, repeat)
    print(answer)
    result = submit_answer(2017, 16, 2, answer)
    print(result)


if __name__ == "__main__":
    index_steps, char_steps = parse_steps("input.txt")
    main_a(index_steps, char_steps)
    main_b(index_steps, char_steps)
