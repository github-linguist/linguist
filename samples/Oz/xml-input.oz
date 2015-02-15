declare
  [XMLParser] = {Module.link ['x-oz://system/xml/Parser.ozf']}
  Parser = {New XMLParser.parser init}

  Data =
   "<Students>"
  #"  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />"
  #"  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />"
  #"  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />"
  #"  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">"
  #"    <Pet Type=\"dog\" Name=\"Rover\" />"
  #"  </Student>"
  #"  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />"
  #"</Students>"

  fun {IsStudentElement X}
     case X of element(name:'Student' ...) then true
     else false
     end
  end

  fun {GetStudentName element(attributes:As ...)}
     [NameAttr] = {Filter As fun {$ attribute(name:N ...)} N == 'Name' end}
  in
     NameAttr.value
  end

  [StudentsDoc] = {Parser parseVS(Data $)}
  Students = {Filter StudentsDoc.children IsStudentElement}
  StudentNames = {Map Students GetStudentName}
in
  {ForAll StudentNames System.showInfo}
