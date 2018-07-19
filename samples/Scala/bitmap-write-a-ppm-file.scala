object Pixmap {
   def save(bm:RgbBitmap, filename:String)={
      val out=new DataOutputStream(new FileOutputStream(filename))

      out.writeBytes("P6\u000a%d %d\u000a%d\u000a".format(bm.width, bm.height, 255))

      for(y <- 0 until bm.height; x <- 0 until bm.width; c=bm.getPixel(x, y)){
         out.writeByte(c.getRed)
         out.writeByte(c.getGreen)
         out.writeByte(c.getBlue)
      }
   }
}
