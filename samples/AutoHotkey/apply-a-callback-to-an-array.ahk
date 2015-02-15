map("callback", "3,4,5")

callback(array){
  Loop, Parse, array, `,
    MsgBox % (2 * A_LoopField)
}

map(callback, array){
  %callback%(array)
}
