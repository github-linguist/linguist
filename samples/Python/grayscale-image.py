# String masquerading as ppm file (version P3)
import io
ppmfileout = io.StringIO('')

def togreyscale(self):
    for h in range(self.height):
        for w in range(self.width):
            r, g, b = self.get(w, h)
            l = int(0.2126 * r + 0.7152 * g + 0.0722 * b)
            self.set(w, h, Colour(l, l, l))

Bitmap.togreyscale = togreyscale


# Draw something simple
bitmap = Bitmap(4, 4, white)
bitmap.fillrect(1, 0, 1, 2, Colour(127, 0, 63))
bitmap.set(3, 3, Colour(0, 127, 31))
print('Colour:')
# Write to the open 'file' handle
bitmap.writeppmp3(ppmfileout)
print(ppmfileout.getvalue())
print('Grey:')
bitmap.togreyscale()
ppmfileout = io.StringIO('')
bitmap.writeppmp3(ppmfileout)
print(ppmfileout.getvalue())


'''
The print statement above produces the following output :

Colour:
P3
# generated from Bitmap.writeppmp3
4 4
255
   255 255 255   255 255 255   255 255 255     0 127  31
   255 255 255   255 255 255   255 255 255   255 255 255
   255 255 255   127   0  63   255 255 255   255 255 255
   255 255 255   127   0  63   255 255 255   255 255 255

Grey:
P3
# generated from Bitmap.writeppmp3
4 4
254
   254 254 254   254 254 254   254 254 254    93  93  93
   254 254 254   254 254 254   254 254 254   254 254 254
   254 254 254    31  31  31   254 254 254   254 254 254
   254 254 254    31  31  31   254 254 254   254 254 254

'''
