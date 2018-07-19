#lang racket
(require xml xml/path)

(define input
#<<END
<Students>
  <Student Name="April" Gender="F" DateOfBirth="1989-01-02" />
  <Student Name="Bob" Gender="M"  DateOfBirth="1990-03-04" />
  <Student Name="Chad" Gender="M"  DateOfBirth="1991-05-06" />
  <Student Name="Dave" Gender="M"  DateOfBirth="1992-07-08">
    <Pet Type="dog" Name="Rover" />
  </Student>
  <Student DateOfBirth="1993-09-10" Gender="F" Name="&#x00C9;mily" />
</Students>
END
  )

(define students
  (xml->xexpr
   (document-element
    (read-xml (open-input-string input)))))

(se-path*/list '(Student #:Name) students)
