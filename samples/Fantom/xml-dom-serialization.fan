using xml

class XmlDom
{
  public static Void main ()
  {
    doc := XDoc()
    root := XElem("root")
    doc.add (root)

    child := XElem("element")
    child.add(XText("Some text here"))
    root.add (child)

    doc.write(Env.cur.out)
  }
}
