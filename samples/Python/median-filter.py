import Image, ImageFilter
im = Image.open('image.ppm')

median = im.filter(ImageFilter.MedianFilter(3))
median.save('image2.ppm')
