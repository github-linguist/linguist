import cmath
class Complex(complex):
   def __repr__(self):
      rp = '%7.5f'%self.real if not self.pureImag() else ''
      ip = '%7.5fj'%self.imag if not self.pureReal() else ''
      conj = '' if (self.pureImag() or self.pureReal() or self.imag<0.0) else '+'
      return '0.0' if (self.pureImag() and self.pureReal()) else rp+conj+ip
   def pureImag(self):
      return abs( self.real) < 0.000005
   def pureReal(self):
      return abs( self.imag) < 0.000005

def croots(n):
   if n<=0:
      return None
   return (Complex(cmath.rect(1, 2*k*cmath.pi/n)) for k in range(n))
   # in pre-Python 2.6, return (Complex(cmath.exp(2j*k*cmath.pi/n)) for k in range(n))

for nr in range(2,11):
   print nr, list(croots(nr))
