# Check to see whether a name is defined
try: name
except NameError: print "name is undefined at first check"

# Create a name, giving it a string value
name = "Chocolate"

# Check to see whether the name is defined now.
try: name
except NameError: print "name is undefined at second check"

# Remove the definition of the name.
del name

# Check to see whether it is defined after the explicit removal.
try: name
except NameError: print "name is undefined at third check"

# Recreate the name, giving it a value of 42
name = 42

# Check to see whether the name is defined now.
try: name
except NameError: print "name is undefined at fourth check"

# Because most of the output is conditional, this serves as
# a clear indicator that the program has run to completion.
print "Done"
