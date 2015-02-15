package main

import (
    "encoding/xml"
    "fmt"
)

const XML_DATA = `
<Students>
   <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
   <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
   <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
   <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
      <Pet Type="dog" Name="Rover" />
   </Student>
   <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
`

type Students struct {
    Student []Student
}

type Student struct {
    Name string `xml:",attr"`
    //  Gender      string `xml:",attr"`
    //  DateOfBirth string `xml:",attr"`
    //  Pets        []Pet  `xml:"Pet"`
}

type Pet struct {
    Type string `xml:",attr"`
    Name string `xml:",attr"`
}

// xml.Unmarshal quietly skips well formed input with no corresponding
// member in the output data structure.  With Gender, DateOfBirth, and
// Pets commented out of the Student struct, as above, Student contains
// only Name, and this is the only value extracted from the input XML_DATA.
func main() {
    data := new(Students)
    err := xml.Unmarshal([]byte(XML_DATA), data)
    if err != nil {
        fmt.Println(err)
        return
    }
    for _, s := range data.Student {
        fmt.Println(s.Name)
    }
}
