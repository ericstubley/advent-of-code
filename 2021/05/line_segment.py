import math

class LineSegment:
	def __init__(self, start, end):
		self.start = start
		self.end = end

	def __iter__(self):
		ps = self.primitive_steps()
		pdv = self.primitive_direction_vector()
		return ((self.start[0] + t*pdv[0], self.start[1] + t*pdv[1]) for t in range(ps+1))

	def direction_vector(self):
		return (self.end[0] - self.start[0], self.end[1] - self.start[1])

	def primitive_direction_vector(self):
		d = self.direction_vector()
		gcd = int(math.gcd(d[0], d[1]))
		pd = (int(d[0]/gcd), int(d[1]/gcd))
		return pd

	def primitive_steps(self):
		d = self.direction_vector()
		gcd = int(math.gcd(d[0], d[1]))
		return gcd

	def is_horizontal(self):
		return self.end[1] - self.start[1] == 0

	def is_vertical(self):
		return self.end[0] - self.start[0] == 0

	def is_sloped(self):
		return not (self.is_horizontal() or self.is_vertical())


class HorizontalLineSegment(LineSegment):
	def primitive_direction_vector(self):
		if self.end[0] - self.start[0] > 0:
			return (1, 0)
		else:
			return (-1, 0)

	def primitive_steps(self):
		return abs(self.end[0] - self.start[0])

	def is_horizontal(self):
		return True

	def is_vertical(self):
		return False

class VerticalLineSegment(LineSegment):
	def primitive_direction_vector(self):
		if self.end[1] - self.start[1] > 0:
			return (0, 1)
		else:
			return (0, -1)

	def primitive_steps(self):
		return abs(self.end[1] - self.start[1])

	def is_horizontal(self):
		return False

	def is_vertical(self):
		return True
