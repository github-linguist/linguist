class BalancedTernary:
    # Represented as a list of 0, 1 or -1s, with least significant digit first.

    str2dig = {'+': 1, '-': -1, '0': 0} # immutable
    dig2str = {1: '+', -1: '-', 0: '0'} # immutable
    table = ((0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1)) # immutable

    def __init__(self, inp):
        if isinstance(inp, str):
            self.digits = [BalancedTernary.str2dig[c] for c in reversed(inp)]
        elif isinstance(inp, int):
            self.digits = self._int2ternary(inp)
        elif isinstance(inp, BalancedTernary):
            self.digits = list(inp.digits)
        elif isinstance(inp, list):
            if all(d in (0, 1, -1) for d in inp):
                self.digits = list(inp)
            else:
                raise ValueError("BalancedTernary: Wrong input digits.")
        else:
            raise TypeError("BalancedTernary: Wrong constructor input.")

    @staticmethod
    def _int2ternary(n):
        if n == 0: return []
        if (n % 3) == 0: return [0] + BalancedTernary._int2ternary(n // 3)
        if (n % 3) == 1: return [1] + BalancedTernary._int2ternary(n // 3)
        if (n % 3) == 2: return [-1] + BalancedTernary._int2ternary((n + 1) // 3)

    def to_int(self):
        return reduce(lambda y,x: x + 3 * y, reversed(self.digits), 0)

    def __repr__(self):
        if not self.digits: return "0"
        return "".join(BalancedTernary.dig2str[d] for d in reversed(self.digits))

    @staticmethod
    def _neg(digs):
        return [-d for d in digs]

    def __neg__(self):
        return BalancedTernary(BalancedTernary._neg(self.digits))

    @staticmethod
    def _add(a, b, c=0):
        if not (a and b):
            if c == 0:
                return a or b
            else:
                return BalancedTernary._add([c], a or b)
        else:
            (d, c) = BalancedTernary.table[3 + (a[0] if a else 0) + (b[0] if b else 0) + c]
            res = BalancedTernary._add(a[1:], b[1:], c)
            # trim leading zeros
            if res or d != 0:
                return [d] + res
            else:
                return res

    def __add__(self, b):
        return BalancedTernary(BalancedTernary._add(self.digits, b.digits))

    def __sub__(self, b):
        return self + (-b)

    @staticmethod
    def _mul(a, b):
        if not (a and b):
            return []
        else:
            if   a[0] == -1: x = BalancedTernary._neg(b)
            elif a[0] ==  0: x = []
            elif a[0] ==  1: x = b
            else: assert False
            y = [0] + BalancedTernary._mul(a[1:], b)
            return BalancedTernary._add(x, y)

    def __mul__(self, b):
        return BalancedTernary(BalancedTernary._mul(self.digits, b.digits))


def main():
    a = BalancedTernary("+-0++0+")
    print "a:", a.to_int(), a

    b = BalancedTernary(-436)
    print "b:", b.to_int(), b

    c = BalancedTernary("+-++-")
    print "c:", c.to_int(), c

    r = a * (b - c)
    print "a * (b - c):", r.to_int(), r

main()
