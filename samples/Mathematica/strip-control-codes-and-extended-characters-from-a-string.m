stripCtrl[x_]:=StringJoin[Select[Characters[x],
MemberQ[CharacterRange["!","~"]~Join~Characters[FromCharacterCode[Range[128,255]]],#]&]]

stripCtrlExt[x_]:=StringJoin[Select[Characters[x],
MemberQ[CharacterRange["!","~"],#]&]]
