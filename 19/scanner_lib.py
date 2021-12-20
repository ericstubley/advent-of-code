import itertools
import numpy as np

# could have found generators and done it that way, oh well
ORIENTATION_STRINGS = ["+0+1+2",
"+0+2-1",
"+0-1-2",
"+0-2+1",
"-0+1-2",
"-0-2-1",
"-0-1+2",
"-0+2+1",
"+1-0+2",
"+1+2+0",
"+1+0-2",
"+1-2-0",
"-1+0+2",
"-1+2-0",
"-1-0-2",
"-1-2+0",
"+2+1-0",
"+2-0-1",
"+2-1+0",
"+2+0+1",
"-2+1+0",
"-2+0-1",
"-2-1-0",
"-2-0+1"]

ORIENTATION_MATRICES = []
for s in ORIENTATION_STRINGS:
    permute = [int(x) for x in s[1::2]]
    multiple = [-1 if x=="-" else 1 for x in s[0::2]]

    i = np.identity(3, dtype=int)
    m = i[:,permute]
    m[:,0] *= multiple[0]
    m[:,1] *= multiple[1]
    m[:,2] *= multiple[2]
    ORIENTATION_MATRICES.append(m)

def relative_position(b1, b2):
    # given two lists of beacons where we're confident of much overlap, find
    # the position and orientation of scanner 2 relative to scanner 1

    # find the correct orientation by the one which matches beacon differences
    # compute once for b1, and then try all orientations for b2
    b1_diffs = set()
    for x, y in itertools.product(b1, repeat=2):
        b1_diffs.add(tuple(c for c in (x-y)))

    orientation = None
    for m in ORIENTATION_MATRICES:
        b2_m_diffs = set()
        for x, y in itertools.product(b2, repeat=2):
            b2_m_diffs.add(tuple(c for c in np.matmul(m, x-y)))

        if len(b1_diffs & b2_m_diffs) >= 66:
            orientation = m
            
    # how to compute the direction?
    # once you've got an orientation and a common beacon, the direction is
    # the b1 vector - m^(-1)*b2 vector
    # so just compute all those and take the most common, hope for the best

    direction_counts = dict()
    for v1, v2 in itertools.product(b1, b2):
        d = tuple(c for c in (v1 - np.matmul(orientation, v2)))
        if d in direction_counts:
            direction_counts[d] += 1
        else:
            direction_counts[d] = 1

    direction = None
    for d in direction_counts:
        if direction == None or direction_counts[d] > direction_counts[direction]:
            direction = d

    direction = np.array(direction)

    return direction, orientation
