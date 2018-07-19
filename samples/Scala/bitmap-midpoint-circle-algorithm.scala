object BitmapOps {
   def midpoint(bm:RgbBitmap, x0:Int, y0:Int, radius:Int, c:Color)={
      var f=1-radius
      var ddF_x=1
      var ddF_y= -2*radius
      var x=0
      var y=radius

      bm.setPixel(x0, y0+radius, c)
      bm.setPixel(x0, y0-radius, c)
      bm.setPixel(x0+radius, y0, c)
      bm.setPixel(x0-radius, y0, c)

      while(x < y)
      {
         if(f >= 0)
         {
           y-=1
           ddF_y+=2
           f+=ddF_y
         }
         x+=1
         ddF_x+=2
         f+=ddF_x
			
         bm.setPixel(x0+x, y0+y, c)
         bm.setPixel(x0-x, y0+y, c)
         bm.setPixel(x0+x, y0-y, c)
         bm.setPixel(x0-x, y0-y, c)
         bm.setPixel(x0+y, y0+x, c)
         bm.setPixel(x0-y, y0+x, c)
         bm.setPixel(x0+y, y0-x, c)
         bm.setPixel(x0-y, y0-x, c)
      }
   }
}
