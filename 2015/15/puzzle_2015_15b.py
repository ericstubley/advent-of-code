#!/usr/bin/env python3

from cookie import *


def process_ingredient(line):
    split = line.rstrip('\n').split(' ')
    name = split[0][:-1]
    cap = int(split[2][:-1])
    dur = int(split[4][:-1])
    fla = int(split[6][:-1])
    tex = int(split[8][:-1])
    cal = int(split[10])

    return Ingredient(name, cap, dur, fla, tex, cal)


def maximize_score_with_calories(ingredients):
    possible_amounts = generate_recipes(len(ingredients))

    ing_dict = dict()
    for i in ingredients:
        ing_dict[i.name] = i

    ret = 0
    for r in possible_amounts:
        rec_dict = dict()
        for idx, ing in enumerate(ing_dict):
            rec_dict[ing] = r[idx]

        recipe = Recipe(ing_dict, rec_dict)
        if recipe.calories() == 500:
            score = recipe.score()
            if score > ret:
                ret = score

    return ret


def generate_recipes(n):
    helper_ls = gr_helper(n-1)
    new_ls = []
    for ls in helper_ls:
        s = sum(ls)
        if s <= 100:
            ls.append(100 - sum(ls))
            new_ls.append(ls)
    return new_ls


def gr_helper(n):
    # return all tuples of length n whose are entries are 0 <= entry <= 100
    # and the sum is <= 100
    if n == 1:
        return [[x] for x in range(0, 101)]
    else:
        one_less = gr_helper(n-1)
        new_ls = []
        for ls in one_less:
            upper = 100 - sum(ls)
            for x in range(0, upper+1):
                add_x = [__ for __ in ls]
                add_x.append(x)
                new_ls.append(add_x)
        return new_ls


if __name__ == "__main__":
    with open("input.txt") as f:
        data = f.readlines()

    ingredients = list()
    for line in data:
        ingredients.append(process_ingredient(line))
        print(ingredients[-1])

    score = maximize_score_with_calories(ingredients)
    print(score)
