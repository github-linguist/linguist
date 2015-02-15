import re
from fractions import Fraction
from pprint import pprint as pp


equationtext = '''\
  pi/4 = arctan(1/2) + arctan(1/3)
  pi/4 = 2*arctan(1/3) + arctan(1/7)
  pi/4 = 4*arctan(1/5) - arctan(1/239)
  pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
  pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
  pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
  pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
  pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
  pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
  pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
  pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
  pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
  pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
  pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
  pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
'''

def parse_eqn(equationtext=equationtext):
    eqn_re = re.compile(r"""(?mx)
    (?P<lhs> ^ \s* pi/4 \s* = \s*)?             # LHS of equation
    (?:                                         # RHS
        \s* (?P<sign> [+-])? \s*
        (?: (?P<mult> \d+) \s* \*)?
        \s* arctan\( (?P<numer> \d+) / (?P<denom> \d+)
    )""")

    found = eqn_re.findall(equationtext)
    machins, part = [], []
    for lhs, sign, mult, numer, denom in eqn_re.findall(equationtext):
        if lhs and part:
            machins.append(part)
            part = []
        part.append( ( (-1 if sign == '-' else 1) * ( int(mult) if mult else 1),
                       Fraction(int(numer), (int(denom) if denom else 1)) ) )
    machins.append(part)
    return machins


def tans(xs):
    xslen = len(xs)
    if xslen == 1:
        return tanEval(*xs[0])
    aa, bb = xs[:xslen//2], xs[xslen//2:]
    a, b = tans(aa), tans(bb)
    return (a + b) / (1 - a * b)

def tanEval(coef, f):
    if coef == 1:
        return f
    if coef < 0:
        return -tanEval(-coef, f)
    ca = coef // 2
    cb = coef - ca
    a, b = tanEval(ca, f), tanEval(cb, f)
    return (a + b) / (1 - a * b)


if __name__ == '__main__':
    machins = parse_eqn()
    #pp(machins, width=160)
    for machin, eqn in zip(machins, equationtext.split('\n')):
        ans = tans(machin)
        print('%5s: %s' % ( ('OK' if ans == 1 else 'ERROR'), eqn))
