let k = 700000

function fibo(n){
    let i = 3

    let fib = 1, last_fib = 1
    while (i <= n){
        last_fib, fib = fib, fib + last_fib
        i = i + 1
    }
    return fib
}

console.log(k + "th fib is", fibo(k))

