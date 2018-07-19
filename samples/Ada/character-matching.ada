with Ada.Strings.Fixed;  use Ada.Strings.Fixed;
with Ada.Text_IO;        use Ada.Text_IO;

procedure Match_Strings is
   S1 : constant String := "abcd";
   S2 : constant String := "abab";
   S3 : constant String := "ab";
begin
   if S1'Length >= S3'Length and then S1 (S1'First..S1'First + S3'Length - 1) = S3 then
      Put_Line (''' & S1 & "' starts with '" & S3 & ''');
   end if;
   if S2'Length >= S3'Length and then S2 (S2'Last - S3'Length + 1..S2'Last) = S3 then
      Put_Line (''' & S2 & "' ends with '" & S3 & ''');
   end if;
   Put_Line (''' & S3 & "' first appears in '" & S1 & "' at" & Integer'Image (Index (S1, S3)));
   Put_Line
   (  ''' & S3 & "' appears in '" & S2 & ''' &
      Integer'Image (Ada.Strings.Fixed.Count (S2, S3)) & " times"
   );
end Match_Strings;
