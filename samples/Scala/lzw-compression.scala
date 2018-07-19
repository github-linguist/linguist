 // for now, only compress
def compress(tc:String) = {
    //initial dictionary
    val startDict = (1 to 255).map(a=>(""+a.toChar,a)).toMap
    val (fullDict, result, remain) = tc.foldLeft ((startDict, List[Int](), "")) {
      case ((dict,res,leftOver),nextChar) =>
        if (dict.contains(leftOver + nextChar)) // current substring already in dict
          (dict, res, leftOver+nextChar)
        else if (dict.size < 4096) // add to dictionary
          (dict + ((leftOver+nextChar, dict.size+1)), dict(leftOver) :: res, ""+nextChar)
        else // dictionary is full
          (dict, dict(leftOver) :: res, ""+nextChar)
    }
    if (remain.isEmpty) result.reverse else (fullDict(remain) :: result).reverse
}

// test
compress("TOBEORNOTTOBEORTOBEORNOT")
