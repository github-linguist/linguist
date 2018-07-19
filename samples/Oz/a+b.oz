declare
  class TextFile from Open.file Open.text end

  StdIn = {New TextFile init(name:stdin)}

  fun {ReadInt}
     {String.toInt {StdIn getS($)}}
  end
in
  {Show {ReadInt}+{ReadInt}}
