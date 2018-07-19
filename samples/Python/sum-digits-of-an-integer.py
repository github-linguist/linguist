def toBaseX(num, base):
    output = []
    while num:
        num, rem = divmod(num, base)
        output.append(rem)
    return output

def sumDigits(num, base=10):
    if base < 2:
        print "Error: Base must be at least 2"
        return
    return sum(toBaseX(num, base))

print sumDigits(1)
print sumDigits(12345)
print sumDigits(123045)
print sumDigits(0xfe, 16)
print sumDigits(0xf0e, 16)
