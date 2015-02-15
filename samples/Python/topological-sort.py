try:
    from functools import reduce
except:
    pass

data = {
    'des_system_lib':   set('std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee'.split()),
    'dw01':             set('ieee dw01 dware gtech'.split()),
    'dw02':             set('ieee dw02 dware'.split()),
    'dw03':             set('std synopsys dware dw03 dw02 dw01 ieee gtech'.split()),
    'dw04':             set('dw04 ieee dw01 dware gtech'.split()),
    'dw05':             set('dw05 ieee dware'.split()),
    'dw06':             set('dw06 ieee dware'.split()),
    'dw07':             set('ieee dware'.split()),
    'dware':            set('ieee dware'.split()),
    'gtech':            set('ieee gtech'.split()),
    'ramlib':           set('std ieee'.split()),
    'std_cell_lib':     set('ieee std_cell_lib'.split()),
    'synopsys':         set(),
    }

def toposort2(data):
    for k, v in data.items():
        v.discard(k) # Ignore self dependencies
    extra_items_in_deps = reduce(set.union, data.values()) - set(data.keys())
    data.update({item:set() for item in extra_items_in_deps})
    while True:
        ordered = set(item for item,dep in data.items() if not dep)
        if not ordered:
            break
        yield ' '.join(sorted(ordered))
        data = {item: (dep - ordered) for item,dep in data.items()
                if item not in ordered}
    assert not data, "A cyclic dependency exists amongst %r" % data

print ('\n'.join( toposort2(data) ))
