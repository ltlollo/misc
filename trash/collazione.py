from functools import reduce

def a(x): return (x[1], "3*(" + x[1] + ")+(" + x[0] + ")")
def b(x): return ("2*(" + x[1] + ")+" +  x[0], x[1])
def f(x): return b(a(x))
def g(x): return a(b(x))
def l(i, f,  n): return reduce(lambda y, x: f(y), range(n), i)

i = ("n", "m")
t = 8

n, m = l(i, f, t)
print("(", n, ")/(", m, ")", sep='', end="\n\n")
n, m = l(i, g, t)
print("(", n, ")/(", m, ")", sep='')
