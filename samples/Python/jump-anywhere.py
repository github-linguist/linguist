# Example 2: Restarting a loop:
from goto import goto, label
label .start
for i in range(1, 4):
    print i
    if i == 2:
        try:
            output = message
        except NameError:
            print "Oops - forgot to define 'message'!  Start again."
            message = "Hello world"
            goto .start
print output, "\n"
