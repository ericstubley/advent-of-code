#!/usr/bin/env python3

import numpy as np

def enhance(image, algorithm):
    # this assumes the array is already padded
    enhanced = np.zeros(image.shape, dtype=int)
    for i in range(image.shape[0]):
        for j in range(image.shape[1]):
            # for the edges we have to manually set
            if i == 0 or j == 0 or i == image.shape[0]-1 or j == image.shape[1]-1:
                enhanced[i, j] = algorithm[0] if image[i,j] == 0 else algorithm[511]
            # 
            else:
                local = image[i-1:i+2, j-1:j+2] 
                strung = ''.join([str(x) for x in local.flatten()])
                index = int(strung, base=2)
                enhanced[i, j] = algorithm[index]

    return enhanced


with open("input.txt") as f:
    data = f.readlines()
data = [line.rstrip('\n') for line in data]

algorithm = [1 if x=="#" else 0 for x in data[0]]

image = np.zeros((len(data)-2, len(data[2])), dtype=int)
for i, line in enumerate(data[2:]):
    image[i] = np.array([1 if x=="#" else 0 for x in data[i+2]], dtype=int)

image = np.pad(image, pad_width=3, mode='constant', constant_values=0)
image = enhance(image, algorithm)
image = np.pad(image, pad_width=3, mode='constant', constant_values=1)
image = enhance(image, algorithm)
print(np.count_nonzero(image))