class Notes
{
  public static Void main (Str[] args)
  {
    notesFile := File(`notes.txt`) // the backticks make a URI
    if (args.isEmpty)
    {
      if (notesFile.exists)
      {
        notesFile.eachLine |line| { echo (line) }
      }
    }
    else
    {
      // notice the following uses a block so the 'printLine/close'
      // operations are all applied to the same output stream for notesFile
      notesFile.out(true) // 'true' to append to file
      {
        printLine ( DateTime.now.toLocale("DD-MM-YY hh:mm:ss").toStr )
        printLine ( "\t" + args.join(" ") )
        close
      }
    }
  }
}
