using xml

class XmlOutput
{
  public static Void main ()
  {
    Str[] names := ["April", "Tam O'Shanter", "Emily"]
      Str[] remarks := ["Bubbly: I'm > Tam and <= Emily",
        "Burns: \"When chapman billies leave the street ...\"",
        "Short & shrift"]

    doc := XDoc()
    root := XElem("CharacterRemarks")
    doc.add (root)

    names.each |Str name, Int i|
    {
      child := XElem("Character")
      child.addAttr("Name", name)
      child.add(XText(remarks[i]))
      root.add (child)
    }

    doc.write(Env.cur.out)
  }
}
