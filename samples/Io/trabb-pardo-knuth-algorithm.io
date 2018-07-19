// Initialize objects to be used
in_num := File standardInput()
nums := List clone
result := Number

// Prompt the user and get numbers from standard input
"Please enter 11 numbers:" println
11 repeat(nums append(in_num readLine() asNumber()))

// Reverse the numbers received
nums reverseInPlace

// Apply the function and tell the user if the result is above
// our limit. Otherwise, tell them the result.
nums foreach(v,
  // v needs parentheses around it for abs to properly convert v to its absolute value
  result = (v) abs ** 0.5 + 5 * v ** 3
  if (result > 400,
    "Overflow!" println
  ,
     result println
  )
)
