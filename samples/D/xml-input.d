import kxml.xml;
char[]xmlinput =
"<Students>
  <Student Name=\"April\" Gender=\"F\" DateOfBirth=\"1989-01-02\" />
  <Student Name=\"Bob\" Gender=\"M\"  DateOfBirth=\"1990-03-04\" />
  <Student Name=\"Chad\" Gender=\"M\"  DateOfBirth=\"1991-05-06\" />
  <Student Name=\"Dave\" Gender=\"M\"  DateOfBirth=\"1992-07-08\">
    <Pet Type=\"dog\" Name=\"Rover\" />
  </Student>
  <Student DateOfBirth=\"1993-09-10\" Gender=\"F\" Name=\"&#x00C9;mily\" />
</Students>";

void main() {
        auto root = readDocument(xmlinput);
        foreach(students;root.getChildren) if (!students.isCData && students.getName == "Students") {
                // now look for student subnodes
                foreach(student;students.getChildren) if (!student.isCData && student.getName == "Student") {
                        // we found a student!
                        std.stdio.writefln("%s",student.getAttribute("Name"));
                }
                // we only want one, so break out of the loop once we find a match
                break;
        }
}
