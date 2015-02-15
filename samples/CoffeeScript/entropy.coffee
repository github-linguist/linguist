entropy = (s) ->
    freq = (s) ->
        result = {}
        for ch in s.split ""
            result[ch] ?= 0
            result[ch]++
        return result

    frq = freq s
    n = s.length
    ((frq[f]/n for f of frq).reduce ((e, p) -> e - p * Math.log(p)), 0) * Math.LOG2E

console.log "The entropy of the string '1223334444' is #{entropy '1223334444'}"
