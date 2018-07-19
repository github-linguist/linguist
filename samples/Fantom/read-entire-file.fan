class ReadString
{
  public static Void main (Str[] args)
  {
    Str contents := File(args[0].toUri).readAllStr
    echo ("contents: $contents")
  }
}
