>>> import operator
>>> class num(int):
    def __pow__(self, b):
        print "Empowered"
        return operator.__pow__(self+0, b)


>>> x = num(3)
>>> x**2
Empowered
9
>>> class num(float):
    def __pow__(self, b):
        print "Empowered"
        return operator.__pow__(self+0, b)


>>> x = num(2.5)
>>> x**2
Empowered
6.25
>>>
