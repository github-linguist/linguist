>>> def printtable(data):
    for row in data:
        print ' '.join('%-5s' % ('"%s"' % cell) for cell in row)


>>> import operator
>>> def sorttable(table, ordering=None, column=0, reverse=False):
    return sorted(table, cmp=ordering, key=operator.itemgetter(column), reverse=reverse)

>>> data = [["a", "b", "c"], ["", "q", "z"], ["zap", "zip", "Zot"]]
>>> printtable(data)
"a"   "b"   "c"
""    "q"   "z"
"zap" "zip" "Zot"
>>> printtable( sorttable(data) )
""    "q"   "z"
"a"   "b"   "c"
"zap" "zip" "Zot"
>>> printtable( sorttable(data, column=2) )
"zap" "zip" "Zot"
"a"   "b"   "c"
""    "q"   "z"
>>> printtable( sorttable(data, column=1) )
"a"   "b"   "c"
""    "q"   "z"
"zap" "zip" "Zot"
>>> printtable( sorttable(data, column=1, reverse=True) )
"zap" "zip" "Zot"
""    "q"   "z"
"a"   "b"   "c"
>>> printtable( sorttable(data, ordering=lambda a,b: cmp(len(b),len(a))) )
"zap" "zip" "Zot"
"a"   "b"   "c"
""    "q"   "z"
>>>
