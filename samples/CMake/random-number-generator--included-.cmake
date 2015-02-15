# Show random integer from 0 to 9999.
string(RANDOM LENGTH 4 ALPHABET 0123456789 number)
math(EXPR number "${number} + 0")  # Remove extra leading 0s.
message(STATUS ${number})
