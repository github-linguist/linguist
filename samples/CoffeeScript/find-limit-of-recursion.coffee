recurse = ( depth = 0 ) ->
    try
        recurse depth + 1
    catch exception
        depth

console.log "Recursion depth on this system is #{ do recurse }"
