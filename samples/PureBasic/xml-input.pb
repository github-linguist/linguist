Define studentNames.String, src$

src$ = "<Students>"
src$ + "<Student Name='April' Gender='F' DateOfBirth='1989-01-02' />"
src$ + "<Student Name='Bob' Gender='M'  DateOfBirth='1990-03-04' />"
src$ + "<Student Name='Chad' Gender='M'  DateOfBirth='1991-05-06' />"
src$ + "<Student Name='Dave' Gender='M'  DateOfBirth='1992-07-08'>"
src$ + "<Pet Type='dog' Name='Rover' />"
src$ + "</Student>"
src$ + "<Student DateOfBirth='1993-09-10' Gender='F' Name='&#x00C9;mily' />"
src$ + "</Students>"

;This procedure is generalized to match any attribute of any normal element's node name
;i.e. get_values(MainXMLNode(0),"Pet","Type",@petName.String) and displaying petName\s
;would display "dog".
Procedure get_values(*cur_node, nodeName$, attribute$, *valueResults.String)
  ;If nodeName$ and attribute$ are matched then the value
  ;will be added to the string structure pointed to by *valueResults .
  Protected result$

  While *cur_node
    If XMLNodeType(*cur_node) = #PB_XML_Normal

      result$ = GetXMLNodeName(*cur_node)
      If result$ = nodeName$
        If ExamineXMLAttributes(*cur_node)
          While NextXMLAttribute(*cur_node)
            If XMLAttributeName(*cur_node) = attribute$
              If *valueResults <> #Null
                *valueResults\s + XMLAttributeValue(*cur_node) + Chr(13) ;value + carriage-return
              EndIf
            EndIf
          Wend
        EndIf
      EndIf

    EndIf

    get_values(ChildXMLNode(*cur_node), nodeName$, attribute$, *valueResults)
    *cur_node = NextXMLNode(*cur_node)
  Wend
EndProcedure

CatchXML(0,@src$,Len(src$))

If IsXML(0)
  get_values(MainXMLNode(0), "Student", "Name",@studentNames)
  MessageRequester("Student Names", studentNames\s)
  FreeXML(0)
EndIf
