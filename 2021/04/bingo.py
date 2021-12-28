DIMN = 5

class Board:
	def __init__(self, entries):
		self.entries = entries
		self.marks = [0]*(DIMN**2)

	def win(self, draws):
		for d in draws:
			if d in self.entries:
				self.marks[self.entries.index(d)] = 1
				if self.is_winning():
					return draws.index(d), d
		return False, False

	def is_winning(self):
		return [1]*5 in self.victory_lines()	

	def score(self, draws):
		self.reset_marks()
		index, draw = self.win(draws)
		score = 0
		for i in range(DIMN**2):
			if self.marks[i] == 0:
				score += self.entries[i]
		score *= draw
		return score

	def shape_entries(self):
		shaped = []
		for i in range(DIMN):
			shaped.append(self.entries[i*DIMN:i*DIMN + DIMN])
		return shaped

	def victory_lines(self):
		sl = []
		for i in range(DIMN):
			sl.append(self.marks[i*DIMN:i*DIMN+DIMN])
		for j in range(DIMN):
			sl.append(self.marks[j:j+DIMN**2:DIMN])
		return sl

	def reset_marks(self):
		self.marks = [0]*(DIMN**2)


def flatten(ls):
	return [x for row in ls for x in row]