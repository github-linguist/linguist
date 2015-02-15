>>> from itertools import combinations_with_replacement
>>> n, k = 'iced jam plain'.split(), 2
>>> list(combinations_with_replacement(n,k))
[('iced', 'iced'), ('iced', 'jam'), ('iced', 'plain'), ('jam', 'jam'), ('jam', 'plain'), ('plain', 'plain')]
>>> # Extra credit
>>> len(list(combinations_with_replacement(range(10), 3)))
220
>>>
