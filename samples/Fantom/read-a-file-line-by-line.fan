class Main
{
  Void main ()
  {
    File (`data.txt`).eachLine |Str line|
    {
      echo ("Line: $line")
    }
  }
}
