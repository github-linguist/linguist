Stack = Origin mimic do(
  initialize = method(@elements = [])
  pop = method(@elements pop!)
  empty = method(@elements empty?)
  push = method(element, @elements push!(element))
)
