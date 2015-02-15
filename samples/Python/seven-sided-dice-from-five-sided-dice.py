from random import randint

def dice5():
    return randint(1, 5)

def dice7():
    r = dice5() + dice5() * 5 - 6
    return (r % 7) + 1 if r < 21 else dice7()
