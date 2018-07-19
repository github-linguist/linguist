FixedPoint[
 StringReplace[#,
   x : "\n" | StartOfString ~~ a : Except["\n"] ... ~~ "\n" ~~
     b : Except["\n"] ... ~~ y : "\n" | EndOfString :>
    x <> Switch[((#1 + #2) + Abs[#1 - #2])/2 &[StringLength@a,
       StringLength@b], Except[StringLength@a], b,
      Except[StringLength@b], a, _, a <> "\n" <> b] <> y] &, "a
 bb
 ccc
 ddd
 ee
 f
 ggg"]
