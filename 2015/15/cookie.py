class Ingredient:
    def __init__(self, name, cap, dur, fla, tex, cal):
        self.name = name
        self.cap = cap
        self.dur = dur
        self.fla = fla
        self.tex = tex
        self.cal = cal

    def __str__(self):
        return f"{self.name}, {self.cap}, {self.dur}, {self.fla}, {self.tex}"

class Recipe:
    def __init__(self, ingredients, recipe):
        self.ingredients = ingredients
        self.recipe = recipe

    def calories(self):
        cal_tot = 0
        for i in self.recipe:
            amount = self.recipe[i]
            ing = self.ingredients[i]

            cal_tot += amount * ing.cal
        return cal_tot

    def score(self):
        cap_tot, dur_tot, fla_tot, tex_tot = 0, 0, 0, 0

        for i in self.recipe:
            amount = self.recipe[i]
            ing = self.ingredients[i]
            cap_tot += amount * ing.cap
            dur_tot += amount * ing.dur
            fla_tot += amount * ing.fla
            tex_tot += amount * ing.tex

        cap_tot = max(0, cap_tot)
        dur_tot = max(0, dur_tot)
        fla_tot = max(0, fla_tot)
        tex_tot = max(0, tex_tot)

        return cap_tot*dur_tot*fla_tot*tex_tot