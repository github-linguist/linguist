with Ada.Text_IO,
     POSIX.Process_Identification,
     POSIX.Unsafe_Process_Primitives;

procedure Fork is
   use Ada.Text_IO,
       POSIX.Process_Identification,
       POSIX.Unsafe_Process_Primitives;
begin
   if Fork = Null_Process_ID then
      Put_Line ("This is the new process.");
   else
      Put_Line ("This is the original process.");
   end if;
exception
   when others =>
      Put_Line ("Something went wrong.");
end Fork;
