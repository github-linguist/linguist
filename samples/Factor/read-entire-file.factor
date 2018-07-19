USING: io.encodings.ascii io.encodings.binary io.files ;

! to read entire file as binary
"foo.txt" binary file-contents

! to read entire file as lines of text
"foo.txt" ascii file-lines
