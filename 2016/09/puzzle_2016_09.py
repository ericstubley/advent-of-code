#! /usr/bin/env python3

def parse_marker(data, i):
    assert data[i] == '('
    parse = []
    i += 1
    while data[i] != ')':
        parse.append(data[i])
        i += 1
    i += 1
    marker = "".join(parse).rstrip(')').lstrip('(')
    split = marker.split('x')
    chunk, repeat = int(split[0]), int(split[1])
    return chunk, repeat, i


def decompressed_length(data, recurse=False):
    i = 0
    length = 0
    while i < len(data):
        if data[i] == '(':
            # parse off the two numbers
            chunk, repeat, i = parse_marker(data, i)
            # add decompressed length
            if recurse:
                length += repeat * decompressed_length(data[i:i+chunk], True)
            else:
                length += repeat * chunk
            # advance pointer by appropriate amount
            i += chunk
        else:
            length += 1
            i += 1
    return length


def main_a(data):
    print(decompressed_length(data, False))


def main_b(data):
    print(decompressed_length(data, True))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.read()
    main_a(data)
    main_b(data)
