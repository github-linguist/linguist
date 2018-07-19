def circle(self, x0, y0, radius, colour=black):
    f = 1 - radius
    ddf_x = 1
    ddf_y = -2 * radius
    x = 0
    y = radius
    self.set(x0, y0 + radius, colour)
    self.set(x0, y0 - radius, colour)
    self.set(x0 + radius, y0, colour)
    self.set(x0 - radius, y0, colour)

    while x < y:
        if f >= 0:
            y -= 1
            ddf_y += 2
            f += ddf_y
        x += 1
        ddf_x += 2
        f += ddf_x
        self.set(x0 + x, y0 + y, colour)
        self.set(x0 - x, y0 + y, colour)
        self.set(x0 + x, y0 - y, colour)
        self.set(x0 - x, y0 - y, colour)
        self.set(x0 + y, y0 + x, colour)
        self.set(x0 - y, y0 + x, colour)
        self.set(x0 + y, y0 - x, colour)
        self.set(x0 - y, y0 - x, colour)
Bitmap.circle = circle

bitmap = Bitmap(25,25)
bitmap.circle(x0=12, y0=12, radius=12)
bitmap.chardisplay()

'''
The origin, 0,0; is the lower left, with x increasing to the right,
and Y increasing upwards.

The program above produces the following display :

+-------------------------+
|         @@@@@@@         |
|       @@       @@       |
|     @@           @@     |
|    @               @    |
|   @                 @   |
|  @                   @  |
|  @                   @  |
| @                     @ |
| @                     @ |
|@                       @|
|@                       @|
|@                       @|
|@                       @|
|@                       @|
|@                       @|
|@                       @|
| @                     @ |
| @                     @ |
|  @                   @  |
|  @                   @  |
|   @                 @   |
|    @               @    |
|     @@           @@     |
|       @@       @@       |
|         @@@@@@@         |
+-------------------------+
'''
