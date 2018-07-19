on push(StackRef, value)
    set StackRef's contents to {value} & StackRef's contents
    return StackRef
end push

on pop(StackRef)
    set R to missing value
    if StackRef's contents ≠ {} then
        set R to StackRef's contents's item 1
        set StackRef's contents to {} & rest of StackRef's contents
    end if
    return R
end pop

on isStackEmpty(StackRef)
    if StackRef's contents = {} then return true
    return false
end isStackEmpty


set theStack to {}
repeat with i from 1 to 5
    push(a reference to theStack, i)
    log result
end repeat
repeat until isStackEmpty(theStack) = true
    pop(a reference to theStack)
    log result
end repeat
