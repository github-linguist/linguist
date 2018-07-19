String extend
[
   ltrim [
      ^self replacingRegex: '^\s+' with: ''.
   ]
   rtrim [
      ^self replacingRegex: '\s+$' with: ''.
   ]
   trim [
      ^self ltrim rtrim.
   ]
]

|a|
a := '      this is a string       '.

('"%1"' % {a}) displayNl.
('"%1"' % {a ltrim}) displayNl.
('"%1"' % {a rtrim}) displayNl.
('"%1"' % {a trim}) displayNl.
