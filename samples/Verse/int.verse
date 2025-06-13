# CONSTANTES OPTIMISÉES
MAX_INT64 : int = 9223372036854775807
MAX_SAFE_MULTIPLIER : int = 4611686018427387903  # MAX_INT64 / 2

(X:int).IsOdd<public>()<transacts>:logic=if(Mod[X,2]<>0) then true else false
(X:int).IsEven<public>()<transacts>:logic=if(Mod[X,2]=0) then true else false

(InInt:int).Pow<public>(InPower:float)<transacts>:float = Pow(InInt * 1.0,InPower)
(InInt:int).Pow<public>(InPower:int)<transacts>:int = Floor[Pow(InInt * 1.0,InPower * 1.0)] or Err("Integer power calculation failed")

bit_pred := type{_(Left:int, Right:int)<transacts>:logic}

# this function will return the result of applying the predicate to each bit of the input numbers
# used to implement the And, Or and Xor operators
AllBitsByPredicate(_Left:int, _Right:int, Predicate:bit_pred)<transacts>:int=
  var Left :int = _Left
  var Right :int = _Right
  var Result :int = 0
  var Multiplier :int = 1

  loop:
    # find LSB
    if(Predicate(Mod[Left, 2], Mod[Right, 2])?) { set Result += Multiplier }

    # shift right
    set Left = Floor(Left / 2) or Err("Left bit shift calculation failed")
    set Right = Floor(Right / 2) or Err("Right bit shift calculation failed")

    if(Left <= 0 and Right <= 0) { break }
    # Protection overflow améliorée
    if(Multiplier >= 4611686018427387903) { break }

    set Multiplier *= 2
  Result

<#>
  trueth table
  a b r
  0 0 0
  0 1 1
  1 0 1
  1 1 1
<#>
OrPredicate(_Left:int, _Right:int)<transacts>:logic=logic{_Left = 1 or _Right = 1}

<#>
  trueth table
  a b r
  0 0 0
  0 1 0
  1 0 0
  1 1 1
<#>

AndPredicate(_Left:int, _Right:int)<transacts>:logic=logic{_Left = 1 and _Right = 1}

<#>
  trueth table
  a b r
  0 0 0
  0 1 1
  1 0 1
  1 1 0
<#>
XorPredicate(_Left:int, _Right:int)<transacts>:logic=logic{_Left <> _Right}

# because & operator is not implemented in verse, we need to reproduce it
(InInt:int).And<public>(InInt2:int)<transacts>:int=AllBitsByPredicate(InInt, InInt2, AndPredicate)
# because | operator is not implemented in verse, we need to reproduce it
(InInt:int).Or<public>(InInt2:int)<transacts>:int=AllBitsByPredicate(InInt, InInt2, OrPredicate)
# because ^ operator is not implemented in verse, we need to reproduce it
(InInt:int).Xor<public>(InInt2:int)<transacts>:int=AllBitsByPredicate(InInt, InInt2, XorPredicate)
# because << operator is not implemented in verse, we need to reproduce it
(InInt:int).ShiftLeft<public>(InShift:int)<transacts>:int= InInt * 2.Pow(InShift)
# because >> operator is not implemented in verse, we need to reproduce it
(InInt:int).ShiftRight<public>(InShift:int)<transacts>:int= Floor(InInt / 2.Pow(InShift)) or Err("Right shift calculation failed")
# because ~ operator is not implemented in verse, we need to reproduce it
(InInt: int).Not<public>()<transacts>:int = 9223372036854775807 - InInt

# ToFloat is not implemented in verse, we need to reproduce it
(InInt: int).ToFloat<public>()<transacts>:float = InInt * 1.0

# esealy know if a bit is set by index
(InInt: int).HasBit<public>(BitIndex: int)<transacts>:logic=
  if(BitIndex < 0 or BitIndex > 63) { return false }
  logic{Mod[InInt.ShiftRight(BitIndex), 2] = 1}

# easily set bit by index
(InInt: int).SetBit<public>(BitIndex: int, Value: logic)<transacts>:int=
  Mask := 1.ShiftLeft(BitIndex)
  if(Value?) then InInt.Or(Mask) else InInt.And(Mask.Not())

# LOOKUP TABLE OPTIMISÉE - ordonnée pour performance
LOOKUP :[int]string = map:
  1 => ""
  1000 => "k"
  1000000 => "M"
  1000000000 => "B"
  1000000000000 => "T"
  1000000000000000 => "P"
  1000000000000000000 => "E"

# FONCTION COMPACTIZE AMÉLIORÉE
(InInt:int).Compactize<public>():string=
  # Gestion des cas négatifs
  if(InInt < 0):
    return "0"
  
  # Cas simple sous 1000
  if(InInt < 1000):
    return "{InInt}"
  
  # Parcourir les suffixes dans l'ordre décroissant pour efficacité
  for(k -> v : LOOKUP):
    if(InInt < k * 1000):
      if(k = 1):
        return "{InInt}"
      else:
        FloatValue := InInt.ToFloat()
        FloatK := k.ToFloat()
        
        if(InInt < k * 10):
          New := FloatValue / FloatK
          return New.StrRounded(2) + v
        else if(InInt < k * 100):
          New := FloatValue / FloatK
          return New.StrRounded(1) + v
        else:
          New := FloatValue / FloatK
          return New.StrRounded(0) + v

  return "{InInt}"





operator'+'<public>(I:int, F:float)<computes>:float = I * 1.0 + F
operator'+'<public>(F:float, I:int)<computes>:float = F + I * 1.0
operator'-'<public>(I:int, F:float)<computes>:float = I * 1.0 - F
operator'-'<public>(F:float, I:int)<computes>:float = F - I * 1.0
operator'/'<public>(I:int, F:float)<computes>:float = I * 1.0 / F
operator'/'<public>(F:float, I:int)<computes>:float = F / (I * 1.0)






  