class Bot:
    def __init__(self, number, low=None, high=None):
        self.number = number
        self.values = []
        self.low_output = None
        self.high_output = None

    def act(self):
        if len(self.values) == 2:
            print(f"BOT {self.number} ACTING")
            if 17 in self.values and 61 in self.values:
                print(f"IT'S ME, BOT {self.number}, WHO COMPARES 17 and 61!")
            self.low_output.add_value(min(self.values))
            self.high_output.add_value(max(self.values))
            self.values = []

    def add_value(self, value):
        self.values.append(value)
        self.act()


class Output:
    def __init__(self, number):
        self.number = number
        self.value = None

    def add_value(self, value):
        assert self.value is None
        self.value = value