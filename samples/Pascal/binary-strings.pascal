const
  greeting = 'Hello';
var
  s1: string;
  s2: ansistring;
  s3: pchar;
begin
{ Assignments }
  s1 := 'Mister Presiden';  (* typo is on purpose. See below! *)
{ Comparisons }
  if s2 > 'a' then
    writeln ('The first letter of ', s1, ' is later than a');
{ Cloning and copying }
  s2 := greeting;
{ Check if a string is empty }
  if s1 = '' then
    writeln('This string is empty!');
{ Append a byte to a string }
  s1 := s1 + 't';
{ Extract a substring from a string }
  s3 := copy(S2, 2, 4);  (* s3 receives ello *)
{ String replacement }  (* the unit StrUtils of the FreePascal rtl has AnsiReplaceStr *)
  s1 := AnsiReplaceStr('Thees ees a text weeth typos', 'ee', 'i');
{ Join strings}
  s3 := greeting + ' and how are you, ' + s1 + '?';
end.
