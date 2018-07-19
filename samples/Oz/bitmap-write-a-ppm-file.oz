functor
import
   Bitmap
   Open
export
   %% Read
   Write
define
   %% Omitted: Read

   proc {Write B=bitmap(array2d(width:W height:H ...)) Filename}
      F = {New Open.file init(name:Filename flags:[write create truncate binary])}

      proc {WriteColor8 color(R G B)}
	 {F write(vs:[R G B])}
      end

      fun {ToBytes C}
	 [C div 0x100  C mod 0x100]
      end

      proc {WriteColor16 color(R G B)}
	 {F write(vs:{Flatten {Map [R G B] ToBytes}})}
      end

      MaxCol = {Bitmap.maxValue B}
      MaxVal#Writer = if MaxCol =< 0xff then 0xff#WriteColor8
		      else 0xffff#WriteColor16
		      end
      Header = "P6\n"#W#" "#H#" "#MaxVal#"\n"
   in
      try
	 {F write(vs:Header)}
	 {Bitmap.forAllPixels B Writer}
      finally
	 {F close}
      end
   end
end
