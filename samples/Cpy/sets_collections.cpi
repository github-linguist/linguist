s = {3, 1, 2}
t = set([2, 3, 4])
print(sorted(s), sorted(t), sorted(s | t), sorted(s & t), sorted(s - t), sorted(s ^ t), len(s), 2 in s, 9 in s)
print(s == {1, 2, 3}, s != t, {1, 2} < s, {1, 2} <= s, s > {1}, s >= {1, 9}, {1} == {1.0}, set() == set())
print(sorted({x % 3 for x in range(10)}), set("hello") == set("olleh"), sorted(set("hello")))
s.add(10); s.add(1); s.discard(99); s.remove(10)
print(sorted(s), s.pop() in (1, 2, 3), len(s))
u = set()
u.update([1, 2], (3,), {4})
print(sorted(u), sorted(u.union([9])), sorted(u.intersection([1, 2, 9])), sorted(u.difference([1])), sorted(u.symmetric_difference({1, 8})))
print(u.issubset({1, 2, 3, 4, 5}), u.issuperset({1}), u.isdisjoint({7}), sorted(u.copy()), bool(set()), bool({0}))
u |= {100}
u -= {1}
u &= {2, 3, 100, 5}
print(sorted(u))
fs = frozenset([1, 2, 3])
print(fs == {1, 2, 3}, {fs: "x"}[frozenset([3, 2, 1])], sorted(fs | {9}), type(fs).__name__, type(s).__name__)
print(repr(set()), repr({1}), repr(frozenset()), str({"a"}), len({1, 1, 1}), sorted(sorted({(1, 2), (2, 1), (1, 2)})))
try:
    {[1]}
except TypeError as e:
    print("unhashable:", e)
try:
    set().remove(1)
except KeyError:
    print("KeyError")
print(sorted(list({1: 2, 3: 4})), {**{1: 2}, **{3: 4}}, {1: 2} | {3: 4}, sum({1, 2, 3}), max({4, 9, 2}), sorted(map(str, {2, 1})))
# dict extras
d = {"a": 1}
d |= {"b": 2}
print(d, dict.fromkeys("ab", 0), d.popitem(), d, {}.fromkeys([1, 2], "x"))
# defaultdict / Counter / OrderedDict
from collections import defaultdict, Counter, OrderedDict
dd = defaultdict(list)
for k, v in [("a", 1), ("b", 2), ("a", 3)]:
    dd[k].append(v)
print(dict(dd), dd["zz"], len(dd), "q" in dd, dd.get("nope"))
di = defaultdict(int)
for ch in "mississippi":
    di[ch] += 1
print(sorted(di.items()), isinstance(di, dict))
cn = Counter("mississippi")
print(cn.most_common(2), cn["s"], cn["zz"], sorted(cn.elements())[:3], sum(cn.values()), cn.total() if hasattr(cn, "total") else 11)
cn.update("ssss")
cn.update({"m": 10})
print(cn.most_common(3), Counter([1, 1, 2]), Counter(a=2, b=1))
od = OrderedDict()
od["x"] = 1; od["y"] = 2; od["z"] = 3
od.move_to_end("x")
print(list(od), od.popitem(), od.popitem(last=False), list(od.items()))
print(Counter("abbccc") == Counter("cccbba"), type(cn).__name__, type(dd).__name__)
words = "a b a c b a".split()
print(sorted(Counter(words).items(), key=lambda kv: (-kv[1], kv[0])))
