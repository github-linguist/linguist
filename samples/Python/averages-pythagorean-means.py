from operator import mul
from functools import reduce

def amean(num):
	return sum(num)/len(num)

def gmean(num):
	return reduce(mul, num, 1)**(1/len(num))

def hmean(num):
	return len(num)/sum(1/n for n in num)

numbers = range(1,11) # 1..10
a, g, h = amean(numbers), gmean(numbers), hmean(numbers)
print(a, g, h)
assert( a >= g >= h )
