import sys
sys.setrecursionlimit(10500)

DEFAULT = 1
def fact(n):
    if n < 2: return DEFAULT
    return n * fact(n-1)

fact(float(10000))

