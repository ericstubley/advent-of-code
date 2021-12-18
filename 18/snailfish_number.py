from math import ceil, floor

class SnailfishNumber:
    def __init__(self, left=None, right=None, value=None):
        self.left = left
        self.right = right
        self.value = value

    def __str__(self):
        if self.value != None:
            return str(self.value)
        else:
            return '[' + str(self.left) + ',' + str(self.right) + ']'

    def __add__(self, other):
        return SnailfishNumber(left=self, right=other) 

    def max_depth(self):
        if self.value != None:
            return 0
        else:
            return 1 + max(self.left.max_depth(), self.right.max_depth())

    def max_value(self):
        if self.value != None:
            return self.value
        else:
            return max(self.left.max_value(), self.right.max_value())

    def magnitude(self):
        if self.value != None:
            return self.value
        else:
            return 3*self.left.magnitude() + 2*self.right.magnitude()

    def is_simplified(self):
        return (self.max_depth() <= 4 and self.max_value() < 10)

    def split(self):
        # doesn't check that you should actually be splitting it
        self.left = SnailfishNumber(value=floor(self.value/2))
        self.right = SnailfishNumber(value=ceil(self.value/2))
        self.value = None

    def explode(self):
        # should only be used when left and right are values 
        # doesn't take care of adding the increments to the left and right
        # but those do get returned
        l = self.left.value
        r = self.right.value
        self.left = None
        self.right = None
        self.value = 0
        return (l, r)

    def dfs(self):
        dfs_ls = [(self, 0)]
        if self.left != None:
            dfs_ls += [(n, d+1) for n, d in self.left.dfs()]
        if self.right != None:
            dfs_ls += [(n, d+1) for n, d in self.right.dfs()]
        return dfs_ls

class SnailfishParser:
    def __init__(self, string):
        self.string = string

    def parse(self):
        left, right, value = None, None, None

        while self.string != '':
            if self.string[0] == '[':
                left, s = SnailfishParser(self.string[1:]).parse()
                self.string = s
            elif self.string[0].isdigit():
                value = int(self.string[0])
                self.string = self.string[1:]
                break
            elif self.string[0] == ',':
                right, s = SnailfishParser(self.string[1:]).parse()
                self.string = s
            elif self.string[0] == ']':
                self.string = self.string[1:]
                break

        return SnailfishNumber(left, right, value), self.string