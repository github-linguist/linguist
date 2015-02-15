/** Turn a list of arrays into a list of maps with the given keys. */
def addKeys(keys, rows) {
  def res := [].diverge()
  for row in rows { res.push(__makeMap.fromColumns(keys, row)) }
  return res.snapshot()
}

def data := addKeys(
  ["name",            "id",  "salary", "dept"],
 [["Tyler Bennett",   "E10297", 32000, "D101"],
  ["John Rappl",      "E21437", 47000, "D050"],
  ["George Woltman",  "E00127", 53500, "D101"],
  ["Adam Smith",      "E63535", 18000, "D202"],
  ["Claire Buckman",  "E39876", 27800, "D202"],
  ["David McClellan", "E04242", 41500, "D101"],
  ["Rich Holcomb",    "E01234", 49500, "D202"],
  ["Nathan Adams",    "E41298", 21900, "D050"],
  ["Richard Potter",  "E43128", 15900, "D101"],
  ["David Motsinger", "E27002", 19250, "D202"],
  ["Tim Sampair",     "E03033", 27000, "D101"],
  ["Kim Arlich",      "E10001", 57000, "D190"],
  ["Timothy Grove",   "E16398", 29900, "D190"]])

def topSalaries(n, out) {
    var groups := [].asMap()
    for row in data {
        def [=> salary, => dept] | _ := row
        def top := groups.fetch(dept, fn {[]}).with([-salary, row]).sort()
        groups with= (dept, top.run(0, top.size().min(n)))
    }
    for dept => group in groups.sortKeys() {
        out.println(`Department $dept`)
        out.println(`---------------`)
        for [_, row] in group {
          out.println(`${row["id"]}  $$${row["salary"]}  ${row["name"]}`)
        }
        out.println()
    }
}
