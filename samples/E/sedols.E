def weights := [1,3,1,7,3,9]
def Digit := ('0'..'9')
def Letter := ('B'..'D'|'F'..'H'|'J'..'N'|'P'..'T'|'V'..'Z')
def sedolCharValue(c) {
  switch (c) {
    match digit :Digit { return digit - '0' }
    match letter :Letter {
      return letter - 'A'
    }
  }
}

def checksum(sedol :String) {
  require(sedol.size() == 6)
  var sum := 0
  for i => c in sedol {
    sum += weights[i] * sedolCharValue(c)
  }
  return E.toString((10 - sum %% 10) %% 10)
}

def addChecksum(sedol :String) {
  return sedol + checksum(sedol)
}

for sedol in "710889
              B0YBKJ
              406566
              B0YBLH
              228276
              B0YBKL
              557910
              B0YBKR
              585284
              B0YBKT".trim().split("\n") {
  println(addChecksum(sedol.trim()))
}
