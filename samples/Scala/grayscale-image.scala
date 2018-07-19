object BitmapOps {
   def luminosity(c:Color)=(0.2126*c.getRed + 0.7152*c.getGreen + 0.0722*c.getBlue+0.5).toInt

   def grayscale(bm:RgbBitmap)={
      val image=new RgbBitmap(bm.width, bm.height)
      for(x <- 0 until bm.width; y <- 0 until bm.height; l=luminosity(bm.getPixel(x,y)))
         image.setPixel(x, y, new Color(l,l,l))
      image
   }
}
