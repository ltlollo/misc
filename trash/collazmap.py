import numpy as np
import sys

def factor(x):
    factors = np.array([], dtype=int)
    i = 2
    while x > 1:
        if x % i == 0:
            x = x // i
            factors = np.append(factors, i)
        else:
            i += 1
    return factors

primes = np.array([2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71])

def coord(x):
	pf = factor(x)
	p2 = pf.searchsorted(2, side='right')
	y = p2
	if len(pf) == p2:
		x = 0
	else:
		x = pf[p2:].prod()
	return (float(x), float(y))


def printdata(m, M):
	for i in range(m,M,2):
		x, y = coord(i)
		print(x, y)
		x, y = coord(3*i+1)
		print(x, y)
		print("")


if (__name__ == '__main__'):
	m = 3
	M = 70
	if len(sys.argv) > 1:
		m = int(sys.argv[1])
	if len(sys.argv) > 2:
		M = int(sys.argv[2])
	if m > M:
		m, M = M, m
	printdata(m, M)
