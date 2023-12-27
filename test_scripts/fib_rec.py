import sys
sys.setrecursionlimit(7500)

k = 7000
fibo_mem = {}

def fibo(n):
    if n<3: return 1
    if n in fibo_mem: return fibo_mem[n]

    fibo_mem[n] = fibo(n-1) + fibo(n-2)
    return fibo_mem[n]

print(str(k) + "th fib is", fibo(k))

