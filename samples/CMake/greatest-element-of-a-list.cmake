# max(var [value1 value2...]) sets var to the maximum of a list of
# integers. If list is empty, sets var to NO.
function(max var)
  set(first YES)
  set(choice NO)
  foreach(item ${ARGN})
    if(first)
      set(choice ${item})
      set(first NO)
    elseif(choice LESS ${item})
      set(choice ${item})
    endif()
  endforeach(item)
  set(${var} ${choice} PARENT_SCOPE)
endfunction(max)

set(list 33 11 44 22 66 55)
max(maximum ${list})
message(STATUS "maximum of ${list} => ${maximum}")
