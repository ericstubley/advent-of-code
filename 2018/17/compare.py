#! /usr/bin/env python3

START = (0, 500)
BOUND = 1868

def parse_input(filename):
    with open(filename) as f:
        lines = f.readlines()

    grid = dict()
    for line in lines:
        t1, t2 = line.rstrip('\n').split(", ")
        t2a, t2b = t2.split("..")
        fixed = int(t1[2:])
        lower = int(t2a[2:])
        upper = int(t2b)
        if t1.startswith('x'):
            for y in range(lower, upper+1):
                grid[(y, fixed)] = '#'
        elif t1.startswith('y'):
            for x in range(lower, upper+1):
                grid[(fixed, x)] = '#'
    return grid


def count_water(grid):
    count = 0
    for k in grid:
        if grid[k] in "|~":
            count += 1
    return count


def fall(grid, p):
    y, x = p

    while y <= BOUND and grid.get((y+1, x)) == None:
        grid[(y, x)] = '|'
        y += 1

    if grid.get((y+1, x)) in ['#', '~']:
        spread(grid, (y, x))
    elif grid.get((y+1, x)) == '|':
        grid[(y, x)] = '|'

    return grid


def spread(grid, p):
    lx, le = get_edge(grid, p, -1)
    rx, re = get_edge(grid, p, 1)

    (y, x0) = p

    while le == "#" and re == "#":

        for x in range(lx, rx+1):
            grid[(y,x)] = '~'
        y -= 1
        lx, le = get_edge(grid, (y, x0), -1)
        rx, re = get_edge(grid, (y, x0), 1)


    for x in range(lx, rx+1):
        grid[(y,x)] = '|'
    if le == '.':
        fall(grid, (y, lx))
    if re == '.':
        fall(grid, (y, rx))
    return grid


def get_edge(grid, p, d):
    y, x = p

    while grid.get((y+1, x)) in ['#', '~']:
        if grid.get((y, x+d)) == '#':
            return (x, '#')
        else:
            x += d
    return (x, '.')


def write_to_file(grid, filename):
    le = min([p[1] for p in grid])
    re = max([p[1] for p in grid])
    bot = max([p[0] for p in grid])

    rows = []
    for y in range(bot+1):
        row = []
        for x in range(le, re+1):
            if (y, x) in grid:
                row.append(grid[(y, x)])
            else:
                row.append('.')
        rows.append("".join(row))

    output = "\n".join(rows)
    with open(filename, 'w') as f:
        f.write(output)


if __name__ == "__main__":
    grid = parse_input("input.txt")
    grid[START] = "+"
    print(count_water(fall(grid, START)))
    write_to_file(grid, "compare_input_grid.txt")