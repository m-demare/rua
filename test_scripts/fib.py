k = float(700000)

def fibo(n):
    i = 3

    fib, last_fib = float(1), float(1)
    while i <= n:
        last_fib, fib = fib, fib + last_fib
        i = i + 1

    return fib

try:
    print(str(k) + "th fib is", fibo(k))
except:
    pass

