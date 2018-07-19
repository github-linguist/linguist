encode[text_String, key_String] :=
 Module[{textCode, keyCode},
  textCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       text], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       key], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   If[Length[textCode] < Length[keyCode],
    keyCode[[;; Length@textCode]],
    PadRight[keyCode, Length@textCode, keyCode]];
  FromCharacterCode[Mod[textCode + keyCode, 26] + 65]]

decode[text_String, key_String] :=
 Module[{textCode, keyCode},
  textCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       text], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   Cases[ToCharacterCode[
      ToUpperCase@
       key], _?(IntervalMemberQ[Interval@{65, 90}, #] &)] - 65;
  keyCode =
   If[Length[textCode] < Length[keyCode],
    keyCode[[;; Length@textCode]],
    PadRight[keyCode, Length@textCode, keyCode]];
  FromCharacterCode[Mod[textCode - keyCode, 26] + 65]]
