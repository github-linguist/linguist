# generators
def count_up(n):
    for i in range(n):
        yield i
def fib():
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b
g = count_up(3)
print(next(g), next(g), next(g), next(g, "done"), list(count_up(4)))
f = fib()
print([next(f) for _ in range(10)])
def echo():
    received = []
    while True:
        x = yield len(received)
        if x is None:
            return received
        received.append(x)
e = echo()
print(next(e), e.send("a"), e.send("b"))
try:
    e.send(None)
except StopIteration:
    print("stopped")
def sub():
    yield 1
    yield 2
    return "r"
def outer():
    r = yield from sub()
    yield r
    yield from [10, 20]
print(list(outer()))
def with_finally():
    try:
        yield 1
        yield 2
    finally:
        print("gen cleanup")
for v in with_finally():
    print("got", v)
    break
def boom():
    yield 1
    raise ValueError("inside")
try:
    for v in boom():
        print(v)
except ValueError as ex:
    print("caught", ex)
gen = (x * x for x in range(5))
print(sum(gen), list(x for x in "abc"), max(x for x in [3, 9, 2]), any(x > 3 for x in range(10)), all(x < 3 for x in range(10)))
import itertools
print(next(x for x in itertools.count(10) if x % 7 == 0))
print(list(itertools.islice(fib(), 8)), list(itertools.islice(itertools.count(5, 5), 3)))
# generator laziness
def noisy():
    for i in range(3):
        print("producing", i)
        yield i
it = noisy()
print("created")
print(next(it))
print(list(it))
# iterator protocol on classes
class Countdown:
    def __init__(self, n):
        self.n = n
    def __iter__(self):
        return self
    def __next__(self):
        if self.n <= 0:
            raise StopIteration
        self.n -= 1
        return self.n + 1
print(list(Countdown(4)), sum(Countdown(3)), [x for x in Countdown(2)])
class Bag:
    def __init__(self, *items):
        self.items = items
    def __iter__(self):
        return iter(self.items)
    def __len__(self):
        return len(self.items)
a, b, c = Bag(1, 2, 3)
print(a, b, c, sorted(Bag(3, 1, 2)), list(zip(Bag(1, 2), "xy")), 2 in Bag(1, 2), max(Bag(4, 9)))
class GenClass:
    def __iter__(self):
        for i in range(3):
            yield i * 2
print(list(GenClass()), tuple(GenClass()))
# lazy builtins
m = map(lambda x: x + 1, [1, 2, 3])
print(next(m), list(m), list(map(pow, [2, 3], [3, 2])), list(filter(None, [0, 1, "", "a"])), list(zip("ab", range(9), [True, False, None])))
z = zip([1, 2], "ab")
print(next(z), list(z), list(enumerate("ab", 5)), dict(zip("ab", [1, 2])), list(reversed(range(3))), list(reversed("abc")))
it = iter([1, 2, 3])
print(next(it), list(it), next(iter([]), "empty"))
print(sorted(iter([3, 1, 2])), list(iter(iter([1, 2, 3]).__next__, 3)))
# itertools & friends
from itertools import chain, cycle, repeat, accumulate, zip_longest, product, permutations, combinations, groupby, takewhile, dropwhile, starmap, compress, pairwise, filterfalse
print(list(chain([1], (2, 3), "a")), list(chain.from_iterable([[1, 2], [3]])), list(islice(cycle("ab"), 5)) if False else list(itertools.islice(cycle("ab"), 5)), list(repeat("x", 3)))
print(list(accumulate([1, 2, 3, 4])), list(accumulate([1, 2, 3], lambda a, b: a * b)), list(accumulate([1, 2], initial=10)), list(zip_longest("ab", [1], fillvalue="-")))
print(list(product("ab", [1, 2])), list(product([0, 1], repeat=2)), len(list(permutations(range(4)))), list(permutations("abc", 2))[:3], list(combinations("abcd", 2))[:4])
print([(k, list(g)) for k, g in groupby("aabbbc")], list(takewhile(lambda x: x < 3, range(9))), list(dropwhile(lambda x: x < 7, range(9))))
print(list(starmap(pow, [(2, 2), (3, 2)])), list(compress("abcd", [1, 0, 1, 0])), list(pairwise([1, 2, 3, 4])), list(filterfalse(lambda x: x % 2, range(6))))
import functools, operator
print(functools.reduce(operator.add, [1, 2, 3, 4]), functools.reduce(lambda a, b: a * b, range(1, 6), 10), operator.itemgetter(1)([5, 6]), operator.attrgetter("real")(3) if False else "ok")
add3 = functools.partial(lambda a, b, c: a + b + c, 1, 2)
print(add3(3), sorted(["bb", "a", "ccc"], key=operator.itemgetter(0)) if False else "ok2")
@functools.lru_cache(maxsize=None)
def fibm(n):
    return n if n < 2 else fibm(n - 1) + fibm(n - 2)
print(fibm(80), fibm.cache_info().hits > 0)
def cmp(a, b):
    return (a > b) - (a < b)
print(sorted([3, 1, 2], key=functools.cmp_to_key(lambda a, b: b - a)))
import heapq, bisect
h = []
for x in [5, 1, 8, 3, 2]:
    heapq.heappush(h, x)
print([heapq.heappop(h) for _ in range(5)], heapq.nlargest(2, [4, 9, 1, 7]), heapq.nsmallest(2, [4, 9, 1, 7]))
arr = [1, 3, 5, 7]
bisect.insort(arr, 4)
print(arr, bisect.bisect_left(arr, 5), bisect.bisect_right(arr, 5), bisect.bisect(arr, 100))
from collections import deque, namedtuple
dq = deque([1, 2, 3], maxlen=4)
dq.append(4); dq.append(5); dq.appendleft(0)
print(dq, dq.popleft(), dq.pop(), len(dq), list(dq), dq[0], dq[-1])
dq.rotate(1)
print(list(dq))
P = namedtuple("P", "x y")
p = P(1, y=2)
x, y = p
print(p, x, y, p.x + p.y, p._asdict(), p._replace(x=9), p == (1, 2), len(p), p[0], type(p).__name__, P._fields)
import string
print(string.ascii_lowercase[:5], string.digits, string.Template("$a and ${b}").substitute(a=1, b="two"))
