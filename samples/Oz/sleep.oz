declare
  class TextFile from Open.file Open.text end
  StdIn = {New TextFile init(name:stdin)}
  WaitTime = {String.toInt {StdIn getS($)}}
in
  {System.showInfo "Sleeping..."}
  {Delay WaitTime} %% in milliseconds
  {System.showInfo "Awake!"}
