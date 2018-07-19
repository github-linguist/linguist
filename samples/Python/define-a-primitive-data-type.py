>>> class num(int):
    def __init__(self, b):
        if 1 <= b <= 10:
            return int.__init__(self+0)
        else:
            raise ValueError,"Value %s should be >=0 and <= 10" % b


>>> x = num(3)
>>> x = num(11)

Traceback (most recent call last):
  File "<pyshell#394>", line 1, in <module>
    x = num(11)
  File "<pyshell#392>", line 6, in __init__
    raise ValueError,"Value %s should be >=0 and <= 10" % b
ValueError: Value 11 should be >=0 and <= 10
>>> x
3
>>> type(x)
<class '__main__.num'>
>>>
