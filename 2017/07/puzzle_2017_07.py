#! /usr/bin/env python3

class TowerProgram:
    def __init__(self, name, weight=None, parent=None):
        self.name = name
        self.weight = weight
        self.parent = parent
        self.children = []

    def tower_weight(self):
        return self.weight + sum(c.tower_weight() for c in self.children)


def parse_data(data):
    tower = dict()
    for line in data:
        split = line.split()
        name = split[0]
        weight = int(split[1][1:-1])
        if name not in tower:
            tower[name] = TowerProgram(name=name, weight=weight)
        else:
            tower[name].weight = weight

        if len(split) > 2:
            for child in split[3:]:
                child_name = child.rstrip(",")
                if child_name not in tower:
                    tower[child_name] = TowerProgram(name=child_name)
                tower[child_name].parent = tower[name]
                tower[name].children.append(tower[child_name])
    return tower


def bottom(tower):
    node = next(iter(tower.values()))
    while node.parent != None:
        node = node.parent
    return node.name


def find_incorrect(tower):
    node = tower[bottom(tower)]
    while True:
        weights = sorted(list((c.tower_weight(), c.name) for c in node.children))
        if weights[0][0] == weights[-1][0]:
            break
        else:
            if weights[0][0] != weights[1][0]:
                node = tower[weights[0][1]]
            else:  # weights[-1][0] != weights[1][0]:
                node = tower[weights[-1][1]]
    return node.name


def correct_weight(tower):
    node = tower[find_incorrect(tower)]
    current_weight = node.tower_weight()
    for c in node.parent.children:
        if c.tower_weight() != current_weight:
            expected_weight = c.tower_weight()
            break
    diff = expected_weight - current_weight
    return node.weight + diff 



def main_a(tower):
    print(bottom(tower))


def main_b(tower):
    print(find_incorrect(tower), correct_weight(tower))


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()
    tower = parse_data(data)
    main_a(tower)
    main_b(tower)
