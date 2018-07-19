report zcharcode
data: c value 'A', n type i.
field-symbols <n> type x.

assign c to <n> casting.
move <n> to n.
write: c, '=', n left-justified.
