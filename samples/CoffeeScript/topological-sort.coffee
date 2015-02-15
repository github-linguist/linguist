toposort = (targets) ->
  # targets is hash of sets, where keys are parent nodes and
  # where values are sets that contain nodes that must precede the parent

  # Start by identifying obviously independent nodes
  independents = []
  do ->
    for k of targets
      if targets[k].cnt == 0
        delete targets[k]
        independents.push k

  # Note reverse dependencies for theoretical O(M+N) efficiency.
  reverse_deps = []
  do ->
    for k of targets
      for child of targets[k].v
        reverse_deps[child] ?= []
        reverse_deps[child].push k

  # Now be greedy--start with independent nodes, then start
  # breaking dependencies, and keep going as long as we still
  # have independent nodes left.
  result = []
  while independents.length > 0
    k = independents.pop()
    result.push k
    for parent in reverse_deps[k] or []
      set_remove targets[parent], k
      if targets[parent].cnt == 0
        independents.push parent
        delete targets[parent]

  # Show unresolvable dependencies
  for k of targets
    console.log "WARNING: node #{k} is part of cyclical dependency"
  result

parse_deps = ->
  # parse string data, remove self-deps, and fill in gaps
  #
  # e.g. this would transform {a: "a b c", d: "e"} to this:
  #   a: set(b, c)
  #   b: set()
  #   c: set()
  #   d: set(e)
  #   e: set()
  targets = {}
  deps = set()
  for k, v of data
    targets[k] = set()
    children = v.split(' ')
    for child in children
      continue if child == ''
      set_add targets[k], child unless child == k
      set_add deps, child

  # make sure even leaf nodes are in targets
  for dep of deps.v
    if dep not of targets
      targets[dep] = set()
  targets

set = ->
  cnt: 0
  v: {}

set_add = (s, e) ->
  return if s.v[e]
  s.cnt += 1
  s.v[e] = true

set_remove = (s, e) ->
  return if !s.v[e]
  s.cnt -= 1
  delete s.v[e]

data =
  des_system_lib:   "std synopsys std_cell_lib des_system_lib dw02 dw01 ramlib ieee"
  dw01:             "ieee dw01 dware gtech"
  dw02:             "ieee dw02 dware"
  dw03:             "std synopsys dware dw03 dw02 dw01 ieee gtech"
  dw04:             "dw04 ieee dw01 dware gtech"
  dw05:             "dw05 ieee dware"
  dw06:             "dw06 ieee dware"
  dw07:             "ieee dware"
  dware:            "ieee dware"
  gtech:            "ieee gtech"
  ramlib:           "std ieee"
  std_cell_lib:     "ieee std_cell_lib"
  synopsys:         ""


targets = parse_deps()
console.log toposort targets
