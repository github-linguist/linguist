# Returns the first and last characters of string.
function(firstlast string first last)
  # f = first character.
  string(SUBSTRING "${string}" 0 1 f)

  # g = last character.
  string(LENGTH "${string}" length)
  math(EXPR index "${length} - 1")
  string(SUBSTRING "${string}" ${index} 1 g)

  # Return both characters.
  set("${first}" "${f}" PARENT_SCOPE)
  set("${last}" "${g}" PARENT_SCOPE)
endfunction(firstlast)

firstlast("Rosetta Code" begin end)
message(STATUS "begins with ${begin}, ends with ${end}")
