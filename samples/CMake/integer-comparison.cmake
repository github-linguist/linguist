# Define A and B as integers. For example:
#   cmake -DA=3 -DB=5 -P compare.cmake

# The comparisons can take variable names, or they can take numbers.
# So these act all the same:
#   A LESS B
#   ${A} LESS ${B}
#   A LESS ${B}
#   ${A} LESS B

if(A LESS B)
  message(STATUS "${A} is less than ${B}")
endif()
if(A EQUAL B)
  message(STATUS "${A} is equal to ${B}")
endif()
if(A GREATER B)
  message(STATUS "${A} is greater than ${B}")
endif()
