class Rename
{
  public static Void main ()
  {
    // rename file/dir in current directory
    File.rename("input.txt".toUri).rename("output.txt")
    File.rename("docs/".toUri).rename("mydocs/")
    // rename file/dir in root directory
    File.rename("/input.txt".toUri).rename("/output.txt")
    File.rename("/docs/".toUri).rename("/mydocs/")
  }
}
