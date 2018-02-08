from functools import reduce

def deep_ord(l):
    if l == []: return 1
    return reduce(lambda a, b: deep_ord(a) + deep_ord(b), l)

def shallow_ord(l):
    return len(l)

class Num:
    def __init__(self): self.val = []
    def shallow_ord(self): return shallow_ord(self.val)
    def deep_ord(self): return deep_ord(self.val)
    def __add__(self, rhs): return self.val + rhs.val
    def __repr__(self): print()

a = Num()
a.val = [[], []]

print(a.shallow_ord())
print(a.deep_ord())
