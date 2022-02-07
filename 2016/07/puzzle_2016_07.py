#! /usr/bin/env python3

def parse(data):
    return [__.rstrip("\n") for __ in data]


def tls_support(ip):
    ret = False
    paren_stack = []
    for i, c in enumerate(ip[:-3]):
        if c == '[':
            paren_stack.append(c)
        elif c == ']':
            paren_stack.pop()
        elif ip[i:i+4].isalpha() and ip[i] != ip[i+1] and ip[i] == ip[i+3] and ip[i+1] == ip[i+2]:
            if len(paren_stack) > 0:
                ret = False
                break
            else:
                ret = True
    return ret


def ssl_support(ip):
    ret = False
    paren_stack = []
    super_patterns, hyper_patterns = set(), set() 
    for i, c in enumerate(ip[:-2]):
        if c == '[':
            paren_stack.append(c)
        elif c == ']':
            paren_stack.pop()
        else:
            extract = ip[i:i+3]
            if extract.isalpha() and extract[0] != extract[1] and extract[0] == extract[2]:
                if len(paren_stack) > 0:
                    if invert_pattern(extract) in super_patterns:
                        ret = True
                        break
                    hyper_patterns.add(extract)
                else:
                    if invert_pattern(extract) in hyper_patterns:
                        ret = True
                        break
                    super_patterns.add(extract)
    return ret


def invert_pattern(s):
    # s is of the form ABA, return BAB
    return "".join([s[1], s[0], s[1]])


def main_a(ips):
    print(len([x for x in ips if tls_support(x)]))

def main_b(ips):
    print(len([x for x in ips if ssl_support(x)]))

if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    ips = parse(data)

    main_a(ips)
    main_b(ips)
