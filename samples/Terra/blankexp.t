local a = quote
    while false do end
end
terra foo()
  return  a
end
foo()