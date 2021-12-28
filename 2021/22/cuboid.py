import itertools

class Cuboid:
    def __init__(self, xl, xu, yl, yu, zl, zu):
        self.x_lower = xl
        self.x_upper = xu
        self.y_lower = yl
        self.y_upper = yu
        self.z_lower = zl
        self.z_upper = zu

    def __str__(self):
        lower = "({},{},{})".format(self.x_lower, self.y_lower, self.z_lower)
        upper = "({},{},{})".format(self.x_upper, self.y_upper, self.z_upper)
        return lower + " to " + upper

    def num_points(self):
        n = 1
        n *= (self.x_upper - self.x_lower) + 1
        n *= (self.y_upper - self.y_lower) + 1
        n *= (self.z_upper - self.z_lower) + 1
        return n

    def intersects(self, other):
        # true if self and other overlap, false otherwise
        # draw a picture of intervals to convince yourself this is correct
        xy_proj = self.z_upper >= other.z_lower and other.z_upper >= self.z_lower
        xz_proj = self.y_upper >= other.y_lower and other.y_upper >= self.y_lower
        yz_proj = self.x_upper >= other.x_lower and other.x_upper >= self.x_lower
        return xy_proj and xz_proj and yz_proj

    def contains(self, x, y, z):
        x_range = self.x_lower <= x and x <= self.x_upper
        y_range = self.y_lower <= y and y <= self.y_upper
        z_range = self.z_lower <= z and z <= self.z_upper
        return x_range and y_range and z_range

    def subdivide(self, other):
        # return a set of cuboids occupying the same volume as the union of
        # self and other, but these returned cuboids will be disjoint
        ret_ls = []
        if not self.intersects(other):
            ret_ls.append(self)
            ret_ls.append(other)
        else: # they intersect, actual work
            sxl, sxu = self.x_lower, self.x_upper
            syl, syu = self.y_lower, self.y_upper
            szl, szu = self.z_lower, self.z_upper
            oxl, oxu = other.x_lower, other.x_upper
            oyl, oyu = other.y_lower, other.y_upper
            ozl, ozu = other.z_lower, other.z_upper

            x_range, y_range, z_range = [], [], []

            # for each of x, y, z, looks at the interval overlaps and figure
            # out which of the three possible ranges are necessary
            if min(sxl, oxl) < max(sxl, oxl):
                x_range.append((min(sxl, oxl), max(sxl, oxl)-1))
            x_range.append((max(sxl, oxl), min(sxu, oxu)))
            if min(sxu, oxu) < max(sxu, oxu):
                x_range.append((min(sxu, oxu)+1, max(sxu, oxu)))
            # same code but for y
            if min(syl, oyl) < max(syl, oyl):
                y_range.append((min(syl, oyl), max(syl, oyl)-1))
            y_range.append((max(syl, oyl), min(syu, oyu)))
            if min(syu, oyu) < max(syu, oyu):
                y_range.append((min(syu, oyu)+1, max(syu, oyu)))
            # same code but for z
            if min(szl, ozl) < max(szl, ozl):
                z_range.append((min(szl, ozl), max(szl, ozl)-1))
            z_range.append((max(szl, ozl), min(szu, ozu)))
            if min(szu, ozu) < max(szu, ozu):
                z_range.append((min(szu, ozu)+1, max(szu, ozu)))

            # for each of the <= 27 possible things in a subdivision, check if 
            # it intersects with self or other to see if it should be returned
            for xr, yr, zr in itertools.product(x_range, y_range, z_range):
                c = Cuboid(xr[0], xr[1], yr[0], yr[1], zr[0], zr[1])
                if self.intersects(c) or other.intersects(c):
                    ret_ls.append(c)

        return ret_ls