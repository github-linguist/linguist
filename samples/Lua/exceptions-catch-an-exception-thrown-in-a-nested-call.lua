local baz_counter=1
function baz()
  if baz_counter==1 then
    baz_counter=baz_counter+1
    error("U0",3)--3 sends it down the call stack.
  elseif baz_counter==2 then
    error("U1",3)--3 sends it down the call stack.
  end
end

function bar()
  baz()
end

function foo()
  function callbar()
    local no_err,result = pcall(bar)
    --pcall is a protected call which catches errors.
    if not no_err then
      --If there are no errors, pcall returns true.
      if not result:match("U0") then
        --If the error is not a U0 error, rethrow it.
        error(result,2)
        --2 is the distance down the call stack to send
        --the error. We want it to go back to the callbar() call.
      end
    end
  end
  callbar()
  callbar()
end

foo()
