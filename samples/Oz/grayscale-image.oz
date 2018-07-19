functor
import
   Array2D
export
   ToGraymap
   FromGraymap
define
   fun {ToGraymap bitmap(Arr)}
      graymap({Array2D.map Arr Luminance})
   end

   fun {Luminance Color}
      F = {Record.map Color Int.toFloat}
   in
      0.2126*F.1 + 0.7152*F.2 + 0.0722*F.3
   end

   fun {FromGraymap graymap(Arr)}
      bitmap({Array2D.map Arr ToColor})
   end

   fun {ToColor Lum}
      L = {Float.toInt Lum}
   in
      color(L L L)
   end
end
