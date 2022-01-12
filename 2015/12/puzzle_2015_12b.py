#!/usr/bin/env python3

from santa_json import *


if __name__ == "__main__":
    with open("input.txt") as f:
        json_data = f.read()

    top_level_object = JSONParser(json_data).parse()

    print(top_level_object.red_sum())
