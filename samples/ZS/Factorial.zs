! ZS-PYTHON

<n> = int(input: “n = ”)
<o> = 1

if <n> < 0:
    print: “Negative number. Not allowed”
else:
    for <i> in range(1, <n> + 1):
        <o> ×= <i>

    print(f): “<n>! = <o>”
