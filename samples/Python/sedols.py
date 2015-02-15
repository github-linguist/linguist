def char2value(c):
  assert c not in 'AEIOU', "No vowels"
  return int(c, 36)

sedolweight = [1,3,1,7,3,9]

def checksum(sedol):
    tmp = sum(map(lambda ch, weight: char2value(ch) * weight,
                  sedol, sedolweight)
               )
    return str((10 - (tmp % 10)) % 10)

for sedol in '''
    710889
    B0YBKJ
    406566
    B0YBLH
    228276
    B0YBKL
    557910
    B0YBKR
    585284
    B0YBKT
    '''.split():
    print sedol + checksum(sedol)
