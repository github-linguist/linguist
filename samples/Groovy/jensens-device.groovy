def sum = { i, lo, hi, term ->
    (lo..hi).sum { i.value = it; term() }
}
def obj = [:]
println (sum(obj, 1, 100, { 1 / obj.value }))
