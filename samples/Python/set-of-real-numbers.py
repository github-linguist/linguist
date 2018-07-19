class Setr():
    def __init__(self, lo, hi, includelo=True, includehi=False):
        self.eqn = "(%i<%sX<%s%i)" % (lo,
                                      '=' if includelo else '',
                                      '=' if includehi else '',
                                      hi)

    def includes(self, X):
        return eval(self.eqn, locals())

    def union(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sor%s)" % (self.eqn, b.eqn)
        return ans

    def intersection(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sand%s)" % (self.eqn, b.eqn)
        return ans

    def difference(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sand not%s)" % (self.eqn, b.eqn)
        return ans

    def union(self, b):
        ans = Setr(0,0)
        ans.eqn = "(%sor%s)" % (self.eqn, b.eqn)
        return ans

    def __repr__(self):
        return "Setr%s" % self.eqn


sets = [Setr(0,1, 0,1).union(Setr(0,2, 1,0)),
        Setr(0,2, 1,0).intersection(Setr(1,2, 0,1)),
        Setr(0,3, 1,0).difference(Setr(0,1, 0,0)),
        Setr(0,3, 1,0).difference(Setr(0,1, 1,1)),
        ]
settexts = '(0, 1] ∪ [0, 2);[0, 2) ∩ (1, 2];[0, 3) − (0, 1);[0, 3) − [0, 1]'.split(';')

for s,t in zip(sets, settexts):
    print("Set %s %s. %s" % (t,
                             ', '.join("%scludes %i"
                                     % ('in' if s.includes(v) else 'ex', v)
                                     for v in range(3)),
                             s.eqn))
