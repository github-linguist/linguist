try:
  from itertools import zip_longest
except:
  try:
    from itertools import izip_longest as zip_longest
  except:
    zip_longest = lambda *args: map(None, *args)

def beadsort(l):
  return map(len, columns(columns([[1] * e for e in l])))

def columns(l):
  return [filter(None, x) for x in zip_longest(*l)]

# Demonstration code:
print(beadsort([5,3,1,7,4,1,1]))
