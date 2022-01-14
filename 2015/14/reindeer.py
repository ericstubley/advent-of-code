class Reindeer:
    def __init__(self, name, speed, endurance, recovery):
        self.name = name
        self.speed = speed
        self.endurance = endurance
        self.recovery = recovery

    def distance_travelled(self, time):
        distance, t = 0, 0
        flying_time = 0
        while t < time:
            if flying_time < self.endurance:
                distance += self.speed
                flying_time += 1
                t += 1
            else:
                t += self.recovery
                flying_time = 0
        return distance
