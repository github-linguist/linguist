class Main
{
  static Int shift (Int char, Int key)
  {
    newChar := char + key
    if (char >= 'a' && char <= 'z')
    {
      if (newChar - 'a' < 0)   { newChar += 26 }
      if (newChar - 'a' >= 26) { newChar -= 26 }
    }
    else if (char >= 'A' && char <= 'Z')
    {
      if (newChar - 'A' < 0)   { newChar += 26 }
      if (newChar - 'A' >= 26) { newChar -= 26 }
    }
    else // not alphabetic, so keep as is
    {
      newChar = char
    }
    return newChar
  }

  static Str shiftStr (Str msg, Int key)
  {
    res := StrBuf()
    msg.each { res.addChar (shift(it, key)) }
    return res.toStr
  }

  static Str encode (Str msg, Int key)
  {
    return shiftStr (msg, key)
  }

  static Str decode (Str msg, Int key)
  {
    return shiftStr (msg, -key)
  }

  static Void main (Str[] args)
  {
    if (args.size == 2)
    {
      msg := args[0]
      key := Int(args[1])

      echo ("$msg with key $key")
      echo ("Encode: ${encode(msg, key)}")
      echo ("Decode: ${decode(encode(msg, key), key)}")
    }
  }
}
