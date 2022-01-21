class Character:
    def __init__(self, hp, damage, armor):
        self.hp = hp
        self.damage = damage
        self.armor = armor
        self.spent = 0

    def beats(self, other):
        hp1, hp2 = self.hp, other.hp
        d1, d2 = self.damage, other.damage
        a1, a2 = self.armor, other.armor
        dealt1, dealt2 = max(1, d1-a2), max(1, d2-a1)

        if hp1 == hp2:
            return dealt1 >= dealt2
        else:
            while hp1 != 0 and hp2 != 0:
                hp2 -= max(1, d1-a2)
                hp1 -= max(1, d2-a1)

            return hp2 == 0

    def equip(self, item):
        self.spent += item["cost"]
        self.damage += item["damage"]
        self.armor += item["armor"]
