class Main
{
  // remove the given key and an optional '=' from start of line
  Str removeKey (Str key, Str line)
  {
    remainder := line[key.size..-1].trim
    if (remainder.startsWith("="))
    {
      remainder = remainder.replace("=", "").trim
    }
    return remainder
  }

  Void main ()
  {
    // define the variables which need configuring
    fullname := ""
    favouritefruit := ""
    needspeeling := false
    seedsremoved := false
    Str[] otherfamily := [,]

    // loop through the file, setting variables as needed
    File(`config.dat`).eachLine |Str line|
    {
      line = line.trim
      if (line.isEmpty || line.startsWith("#") || line.startsWith(";"))
      {
        // do nothing for empty and comment lines
      }
      else if (line.upper.startsWith("FULLNAME"))
      {
        fullname = removeKey("FULLNAME", line)
      }
      else if (line.upper.startsWith("FAVOURITEFRUIT"))
      {
        favouritefruit = removeKey("FAVOURITEFRUIT", line)
      }
      else if (line.upper.startsWith("NEEDSPEELING"))
      {
        needspeeling = true
      }
      else if (line.upper.startsWith("SEEDSREMOVED"))
      {
        seedsremoved = true
      }
      else if (line.upper.startsWith("OTHERFAMILY"))
      {
        otherfamily = removeKey("OTHERFAMILY", line).split(',')
      }
    }

    // report results
    echo ("Full name is $fullname")
    echo ("Favourite fruit is $favouritefruit")
    echo ("Needs peeling is $needspeeling")
    echo ("Seeds removed is $seedsremoved")
    echo ("Other family is " + otherfamily.join(", "))
  }
}
