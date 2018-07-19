mersenneFactor = (p) ->
    limit = Math.sqrt(Math.pow(2,p) - 1)
    k = 1
    while (2*k*p - 1) < limit
        q = 2*k*p + 1
        if isPrime(q) and (q % 8 == 1 or q % 8 == 7) and trialFactor(2,p,q)
            return q
        k++
    return null

isPrime = (value) ->
    for i in [2...value]
        return false if value % i == 0
        return true  if value % i != 0

trialFactor = (base, exp, mod) ->
    square = 1
    bits = exp.toString(2).split('')
    for bit in bits
        square = Math.pow(square, 2) * (if +bit is 1 then base else 1) % mod
    return square == 1

checkMersenne = (p) ->
    factor = mersenneFactor(+p)
    console.log "M#{p} = 2^#{p}-1 is #{if factor is null then "prime" else "composite with #{factor}"}"

checkMersenne(prime) for prime in ["2","3","4","5","7","11","13","17","19","23","29","31","37","41","43","47","53","929"]
