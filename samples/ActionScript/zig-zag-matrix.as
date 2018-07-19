package
{
   public class ZigZagMatrix extends Array
   {

      private var height:uint;
      private var width:uint;
      public var mtx:Array = [];

      public function ZigZagMatrix(size:uint)
      {
         this.height = size;
         this.width = size;

         this.mtx = [];
         for (var i:uint = 0; i < size; i++) {
            this.mtx[i] = [];
         }
         i = 1;
         var j:uint = 1;
         for (var e:uint = 0; e < size*size; e++) {
            this.mtx[i-1][j-1] = e;
            if ((i + j) % 2 == 0) {
               // Even stripes
               if (j < size) j ++;
               else       i += 2;
               if (i > 1) i --;
            } else {
               // Odd stripes
               if (i < size) i ++;
               else       j += 2;
               if (j > 1) j --;
            }
         }
      }
   }
}
