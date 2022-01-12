class JSON:
    def is_red(self):
        return False

    def red_sum(self):
        return 0


class JSONObject(JSON):
    def __init__(self, table):
        self.table = dict([(k, table[k]) for k in table])

    def __str__(self):
        str_ls = [f"\"{k}\":{str(self.table[k])}" for k in self.table]
        string = ",".join(str_ls)

        return f"{{{string}}}"

    def red_sum(self):
        has_red = False
        for k in self.table:
            if self.table[k].is_red():
                has_red = True
                break

        if has_red:
            ret = 0
        else:
            ret = sum([self.table[__].red_sum() for __ in self.table])
        return ret


class JSONArray(JSON):
    def __init__(self, array):
        self.array = [__ for __ in array]

    def __str__(self):
        str_ls = [str(__) for __ in self.array]
        string = ",".join(str_ls)
        return f"[{string}]"

    def red_sum(self):
        return sum([__.red_sum() for __ in self.array])


class JSONNumber(JSON):
    def __init__(self, number):
        self.number = number

    def __str__(self):
        return f"{self.number}"

    def red_sum(self):
        return self.number


class JSONString(JSON):
    def __init__(self, string):
        self.string = string

    def __str__(self):
        return f"\"{self.string}\""

    def is_red(self):
        return self.string == "red"


class JSONParser:
    def __init__(self, data):
        self.data = data

    def parse(self):
        return self.next_json()

    def next_json(self):
        if self.data.startswith('{'):
            d = dict()
            while not self.data.startswith('}'):
                # there's a key and then a value
                # for us, all keys are a single character
                self.data = self.data[1:]
                key = self.data[1]
                self.data = self.data[4:]
                val = self.next_json()

                d[key] = val
            self.data = self.data[1:]

            return JSONObject(d)

        elif self.data.startswith('['):
            ls = []
            while not self.data.startswith(']'):
                self.data = self.data[1:]
                ls.append(self.next_json())
            self.data = self.data[1:]

            return JSONArray(ls)

        elif self.data.startswith('-') or self.data[0].isdigit():
            curr = [self.data[0]]
            for c in self.data[1:]:
                if c.isdigit():
                    curr.append(c)
                else:
                    break

            self.data = self.data[len(curr):]

            number = int("".join(curr))
            return JSONNumber(number)
        elif self.data.startswith('"'):
            curr = []
            for c in self.data[1:]:
                if c != '"':
                    curr.append(c)
                else:
                    break

            self.data = self.data[len(curr)+2:]

            string = "".join(curr)
            return JSONString(string)
        else:
            return
