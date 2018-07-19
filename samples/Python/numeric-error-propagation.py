from collections import namedtuple
import math

class I(namedtuple('Imprecise', 'value, delta')):
    'Imprecise type: I(value=0.0, delta=0.0)'

    __slots__ = ()

    def __new__(_cls, value=0.0, delta=0.0):
        'Defaults to 0.0 ± delta'
        return super().__new__(_cls, float(value), abs(float(delta)))

    def reciprocal(self):
        return I(1. / self.value, self.delta / (self.value**2))

    def __str__(self):
        'Shorter form of Imprecise as string'
        return 'I(%g, %g)' % self

    def __neg__(self):
        return I(-self.value, self.delta)

    def __add__(self, other):
        if type(other) == I:
            return I( self.value + other.value, (self.delta**2 + other.delta**2)**0.5 )
        try:
            c = float(other)
        except:
            return NotImplemented
        return I(self.value + c, self.delta)

    def __sub__(self, other):
        return self + (-other)

    def __radd__(self, other):
        return I.__add__(self, other)

    def __mul__(self, other):
        if type(other) == I:
            #if id(self) == id(other):
            #    return self ** 2
            a1,b1 = self
            a2,b2 = other
            f = a1 * a2
            return I( f, f * ( (b1 / a1)**2 + (b2 / a2)**2 )**0.5 )
        try:
            c = float(other)
        except:
            return NotImplemented
        return I(self.value * c, self.delta * c)

    def __pow__(self, other):
        if type(other) == I:
            return NotImplemented
        try:
            c = float(other)
        except:
            return NotImplemented
        f = self.value ** c
        return I(f, f * c * (self.delta / self.value))

    def __rmul__(self, other):
        return I.__mul__(self, other)

    def __truediv__(self, other):
        if type(other) == I:
            return self.__mul__(other.reciprocal())
        try:
            c = float(other)
        except:
            return NotImplemented
        return I(self.value / c, self.delta / c)

    def __rtruediv__(self, other):
        return other * self.reciprocal()

    __div__, __rdiv__ = __truediv__, __rtruediv__

Imprecise = I

def distance(p1, p2):
    x1, y1 = p1
    x2, y2 = p2
    return ((x1 - x2)**2 + (y1 - y2)**2)**0.5

x1 = I(100, 1.1)
x2 = I(200, 2.2)
y1 = I( 50, 1.2)
y2 = I(100, 2.3)

p1, p2 = (x1, y1), (x2, y2)
print("Distance between points\n  p1: %s\n  and p2: %s\n  = %r" % (
      p1, p2, distance(p1, p2)))
