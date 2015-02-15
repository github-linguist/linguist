Structure characteristic
  name.s
  remark.s
EndStructure
NewList didel.characteristic()
If ReadFile(0, GetCurrentDirectory()+"names.txt")
  While Eof(0) = 0
    AddElement(didel())
    didel()\name = ReadString(0)
  Wend
  CloseFile(0)
EndIf
ResetList(didel())
FirstElement(didel())
If ReadFile(0, GetCurrentDirectory()+"remarks.txt")
  While Eof(0) = 0
    didel()\remark = ReadString(0)
    NextElement(didel())
  Wend
  CloseFile(0)
EndIf
ResetList(didel())
FirstElement(didel())
 xml = CreateXML(#PB_Any)
  mainNode = CreateXMLNode(RootXMLNode(xml))
  SetXMLNodeName(mainNode, "CharacterRemarks")
   ForEach didel()
      item = CreateXMLNode(mainNode)
      SetXMLNodeName(item, "Character")
      SetXMLAttribute(item, "name", didel()\name)
      SetXMLNodeText(item, didel()\remark)
    Next
 FormatXML(xml, #PB_XML_ReFormat |  #PB_XML_WindowsNewline | #PB_XML_ReIndent)
 SaveXML(xml, "demo.xml")
