m=((1,  1,  1,   1),
   (2,  4,  8,  16),
   (3,  9, 27,  81),
   (4, 16, 64, 256),
   (5, 25,125, 625))
print(zip(*m))
# in Python 3.x, you would do:
# print(list(zip(*m)))
