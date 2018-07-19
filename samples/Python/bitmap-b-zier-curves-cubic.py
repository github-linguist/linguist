def cubicbezier(self, x0, y0, x1, y1, x2, y2, x3, y3, n=20):
    pts = []
    for i in range(n+1):
        t = i / n
        a = (1. - t)**3
        b = 3. * t * (1. - t)**2
        c = 3.0 * t**2 * (1.0 - t)
        d = t**3

        x = int(a * x0 + b * x1 + c * x2 + d * x3)
        y = int(a * y0 + b * y1 + c * y2 + d * y3)
        pts.append( (x, y) )
    for i in range(n):
        self.line(pts[i][0], pts[i][1], pts[i+1][0], pts[i+1][1])
Bitmap.cubicbezier = cubicbezier

bitmap = Bitmap(17,17)
bitmap.cubicbezier(16,1, 1,4, 3,16, 15,11)
bitmap.chardisplay()


'''
The origin, 0,0; is the lower left, with x increasing to the right,
and Y increasing upwards.

The chardisplay above produces the following output :
+-----------------+
|                 |
|                 |
|                 |
|                 |
|         @@@@    |
|      @@@    @@@ |
|     @           |
|     @           |
|     @           |
|     @           |
|      @          |
|      @          |
|       @         |
|        @        |
|         @@@@    |
|             @@@@|
|                 |
+-----------------+
'''
