-- Forward jump
goto skip_print
print "won't print"
::skip_print::

-- Backward jump
::loop::
print "infinite loop"
goto loop

-- Labels follow the same scoping rules as local variables, but with no equivalent of upvalues
goto next
do
    ::next:: -- not visible to above goto
    print "won't print"
end
::next:: -- goto actually jumps here

-- goto cannot jump into or out of a function
::outside::
function nope () goto outside end -- error: no visible label 'outside' for <goto> at line 2

goto inside
function nope () ::inside:: end -- error: no visible label 'inside' for <goto> at line 1

-- Convenient for breaking out of nested loops
for i = 1, 10 do
    for j = 1, 10 do
        for k = 1, 10 do
            if i^2 + j^2 == k^2 then
                print(("found: i=%d j=%d k=%d"):format(i, j, k))
                goto exit
            end
        end
    end
end
print "not found"
::exit::
