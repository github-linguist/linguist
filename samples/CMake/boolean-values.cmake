foreach(var 1 42 ON yes True y Princess
            0 OFF no False n Princess-NOTFOUND)
  if(var)
    message(STATUS "${var} is true.")
  else()
    message(STATUS "${var} is false.")
  endif()
endforeach(var)
