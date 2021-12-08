#!/usr/bin/env python3

def find_first(seq, pred):
	# returns the next item in the sequence which satisfies the predicate
	return next(item for item in seq if pred(item))	

# python has sets! that would have been good to use
# rather than using a list comprehension and then selecting the 0th element
# define the predicate as a lambda function and then use it
# this is more readable than what you did

ls = ['abc', 'xyz', 'efg']
desired_element = find_first(ls, lambda a: 'x' in a)
print(desired_element) #'xyz'