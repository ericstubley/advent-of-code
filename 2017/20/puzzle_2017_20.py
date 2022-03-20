#! /usr/bin/env python3

from collections import Counter, namedtuple
from automation.automation import submit_answer


class Particle:
    def __init__(self, i, p, v, a):
        self.i = i
        self.p = p
        self.v = v
        self.a = a

    def __str__(self):
        return f"{self.i}, ({self.p}), ({self.v}), ({self.a})"

    def __repr__(self):
        return f"Particle({self.i}, ({self.p}), ({self.v}), ({self.a}))"

class Vec:
    def __init__(self, x, y, z):
        self.x = x
        self.y = y
        self.z = z

    def __add__(self, other):
        return Vec(self.x + other.x, self.y + other.y, self.z + other.z)

    def __str__(self):
        return f"{self.x},{self.y},{self.z}"

    def to_tuple(self):
        return (self.x, self.y, self.z)
        
    def manhattan(self):
       return abs(self.x) + abs(self.y) + abs(self.z)  


def parse_data(filename):
    with open(filename) as f:
        raw = f.readlines()

    data = []
    for i, line in enumerate(raw):
        strs = list(map(lambda x: x.rstrip(',')[3:-1], line.split()))
        p, v, a = map(lambda x: Vec(*map(int, x.split(','))), strs)
        data.append(Particle(i, p, v, a))
    return data


def closest_long_term(data):
    a_candidate = min(data, key=lambda x: x.a.manhattan())

    updated = data
    for t in range(1000):
        updated = map(evolve_particle, updated)

    p_candidate = min(list(updated), key=lambda x: x.p.manhattan())

    assert a_candidate.i == p_candidate.i
    return a_candidate.i


def evolve_particle(particle):
    new_v = particle.v + particle.a
    new_p = particle.p + new_v
    return Particle(particle.i, new_p, new_v, particle.a)


def remove_collisions(data):
    counts = Counter()
    for particle in data:
        counts[particle.p.to_tuple()] += 1

    new_data = [particle for particle in data if counts[particle.p.to_tuple()] <= 1]
    return new_data


def long_term_count(data):
    num_particles = len(data)
    interval = 0
    updated = data

    while interval < 1000:
        updated = remove_collisions(list(map(evolve_particle, updated)))
        if len(updated) < num_particles:
            num_particles = len(updated)
            print(num_particles)
            interval = 0
        else:
            interval += 1

    return num_particles


def main_a(data):
    answer = closest_long_term(data)
    print(answer)
    # result = submit_answer(2017, 20, 1, answer)
    # print(result)


def main_b(data):
    answer = long_term_count(data)
    print(answer)
    # result = submit_answer(2017, 20, 2, answer)
    # print(result)


if __name__ == "__main__":
    data = parse_data("input.txt")
    main_a(data)
    main_b(data)
