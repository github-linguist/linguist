Dictionary = Join[CharacterRange["a", "z"], CharacterRange["0", "9"]];
mark = 0.1; gap = 0.125; (* gap should be equal to mark. But longer gap makes audio code easier to decode *)
shortgap = 3*gap; medgap = 7*gap;
longmark = 3*mark;
MorseDictionary = {
   ".-", "-...", "-.-.", "-..",
   ".", "..-.", "--.", "....", "..",
   ".---", "-.-", ".-..", "--", "-.",
   "---", ".--.", "--.-", ".-.",
   "...", "-", "..-", "...-", ".--",
   "-..-", "-.--", "--..",
   "-----", ".----", "..---", "...--", "....-", ".....",
   "-....", "--...", "---..", "----."
   };

MorseDictionary = # <> " " & /@ MorseDictionary; (* Force short gap silence after each letter/digit *)

Tones = {
   SoundNote[None, medgap],
   SoundNote[None, shortgap],
   {SoundNote["C", mark, "Clarinet"], SoundNote[None, gap]},
   {SoundNote["C", longmark, "Clarinet"], SoundNote[None, gap]},
   {SoundNote["F#", mark, "Clarinet"], SoundNote[None, gap]}  (* Use F# short mark to denote unrecognized character *)
   };

codeRules = MapThread[Rule, {Dictionary, MorseDictionary}];
decodeRules = MapThread[Rule, {MorseDictionary, Dictionary}];
soundRules = MapThread[Rule, {{"  ", " ", ".", "-", "?"}, Tones}];
(* The order of the rules here is important. Otherwise medium gaps and short gaps get confounded *)

morseCode[s_String] := StringReplace[ToLowerCase@s, codeRules~Join~{x_ /; FreeQ[Flatten@{Dictionary, " "}, x] -> "? "}]
morseDecode[s_String] := StringReplace[s, decodeRules]
sonicMorse[s_String] := EmitSound@Sound@Flatten[Characters@morseCode@s /. soundRules]
