# String masquerading as ppm file (version P3)
import io
ppmfileout = io.StringIO('')

def writeppmp3(self, f):
    self.writeppm(f, ppmformat='P3')

def writeppm(self, f, ppmformat='P6'):
    assert ppmformat in ['P3', 'P6'], 'Format wrong'
    magic = ppmformat + '\n'
    comment = '# generated from Bitmap.writeppm\n'
    maxval = max(max(max(bit) for bit in row) for row in self.map)
    assert ppmformat == 'P3' or 0 <= maxval < 256, 'R,G,B must fit in a byte'
    if ppmformat == 'P6':
        fwrite = lambda s: f.write(bytes(s, 'UTF-8'))
        maxval = 255
    else:
        fwrite = f.write
        numsize=len(str(maxval))
    fwrite(magic)
    fwrite(comment)
    fwrite('%i %i\n%i\n' % (self.width, self.height, maxval))
    for h in range(self.height-1, -1, -1):
        for w in range(self.width):
            r, g, b = self.get(w, h)
            if ppmformat == 'P3':
                fwrite('   %*i %*i %*i' % (numsize, r, numsize, g, numsize, b))
            else:
                fwrite('%c%c%c' % (r, g, b))
        if ppmformat == 'P3':
            fwrite('\n')

Bitmap.writeppmp3 = writeppmp3
Bitmap.writeppm = writeppm

# Draw something simple
bitmap = Bitmap(4, 4, black)
bitmap.fillrect(1, 0, 1, 2, white)
bitmap.set(3, 3, Colour(127, 0, 63))
# Write to the open 'file' handle
bitmap.writeppmp3(ppmfileout)
# Whats in the generated PPM file
print(ppmfileout.getvalue())

'''
The print statement above produces the following output :

P3
# generated from Bitmap.writeppmp3
4 4
255
     0   0   0     0   0   0     0   0   0   127   0  63
     0   0   0     0   0   0     0   0   0     0   0   0
     0   0   0   255 255 255     0   0   0     0   0   0
     0   0   0   255 255 255     0   0   0     0   0   0

'''

# Write a P6 file
ppmfileout = open('tmp.ppm', 'wb')
bitmap.writeppm(ppmfileout)
ppmfileout.close()
