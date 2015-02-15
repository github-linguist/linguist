# This is one of many ways to create a table.  CoffeeScript plays nice
# with any templating solution built for JavaScript, and of course you
# can build tables in the browser using DOM APIs.  This approach is just
# brute force string manipulation.

table = (header_row, rows) ->
  """
  <table>
  #{header_row}
  #{rows.join '\n'}
  </table>
  """

tr = (cells) -> "<tr>#{cells.join ''}</tr>"
th = (s) -> "<th align='right'>#{s}</th>"
td = (s) -> "<td align='right'>#{s}</td>"
rand_n = -> Math.floor Math.random() * 10000

header_cols = ['', 'X', 'Y', 'Z']
header_row = tr (th s for s in header_cols)

rows = []
for i in [1..5]
  rows.push tr [
    th(i)
    td rand_n()
    td rand_n()
    td rand_n()
  ]

html = table header_row, rows
console.log html
