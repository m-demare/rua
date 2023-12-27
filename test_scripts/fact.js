let DEFAULT = 1
function fact(n) {
    if (n < 2) return DEFAULT
    return n * fact(n-1)
}
return fact(10000)

