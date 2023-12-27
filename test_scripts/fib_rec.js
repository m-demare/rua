let k = 7000
let fibo_mem = {}

function fibo(n){
    if (n<3) return 1
    if (fibo_mem[n]) return fibo_mem[n]

    fibo_mem[n] = fibo(n-1) + fibo(n-2)
    return fibo_mem[n]
}

console.log(k + "th fib is", fibo(k))

