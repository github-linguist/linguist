console.log (isFinite(s) for s in [5, "5", "-5", "5", "5e5", 0]) # all true
console.log (isFinite(s) for s in [NaN, "fred", "###"]) # all false
