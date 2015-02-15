try
    find text ".*string$" in "I am a string" with regexp
on error message
    return message
end try

try
    change "original" into "modified" in "I am the original string" with regexp
on error message
    return message
end try
