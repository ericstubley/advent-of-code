#! /usr/bin/env python3

import requests
# authorization_cookie is just a .py file with cookie = "the big auth cookie"
from automation.authorization_cookie import cookie


base_url = "https://adventofcode.com/{year}/day/{day}"
input_url = "https://adventofcode.com/{year}/day/{day}/input"
submit_url = "https://adventofcode.com/{year}/day/{day}/answer"
authorization_header = {"Cookie": cookie}


def run_get_request(url):
    r = requests.get(url=url, headers=authorization_header)
    if r.status_code != requests.codes.ok:
        print("SOMETHING WENT WRONG")
    r.close()
    return r.text


def run_post_request(url, data):
    r = requests.post(url=url, data=data, headers=authorization_header)
    if r.status_code != requests.codes.ok:
        print("SOMETHING WENT WRONG")
    r.close()
    return r.text


def get_puzzle(year, day):
    url = base_url.format(year=year, day=day)
    return run_get_request(url)


def get_input(year, day):
    url = input_url.format(year=year, day=day)
    return run_get_request(url)


def input_to_file(filepath, year, day):
    input_text = get_input(year, day)
    with open(filepath, 'w') as f:
        f.write(input_text)


def submit_answer(year, day, level, answer):
    confirm = input(f"Are you sure you want to submit {answer}?\n") 
    if confirm == 'y':
        url = submit_url.format(year=year, day=day)
        answer_payload = {"Name": "Value", "level": level, "answer": answer}
        return run_post_request(url, answer_payload)
    else:
        print("Not submitted")
        return
