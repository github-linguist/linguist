import fileinput

# originally, return len(a) - len(b) if positive, 0 otherwise.
# Observing that longer is used for its Boolean result,
# and that '' is False, while any other string is True,
# longer need only to return a after removing len(b) characters,
# which is done without resorting to len().
def longer(a, b):
    while a and b:
        a, b = a[1:], b[1:]
    return a

longest, lines = '', ''
for x in fileinput.input():
    if longer(x, longest):
        lines, longest = x, x
    elif not longer(longest, x):
        lines += x

print(lines, end='')
