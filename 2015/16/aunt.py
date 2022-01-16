class Aunt:
    def __init__(self, string):
        split = string.rstrip('\n').split(' ')

        self.number = int(split[1][:-1])

        self.attributes = dict()

        for i in range(2, len(split), 2):
            name = split[i][:-1]
            value = int(split[i+1].rstrip(','))
            self.attributes[name] = value