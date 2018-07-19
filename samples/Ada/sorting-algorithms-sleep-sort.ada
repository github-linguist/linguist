with Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure SleepSort is
   task type PrintTask (num : Integer);
   task body PrintTask is begin
      delay Duration (num) / 100.0;
      Ada.Text_IO.Put(num'Img);
   end PrintTask;
   type TaskAcc is access PrintTask;
   TaskList : array (1 .. Argument_Count) of TaskAcc;
begin
   for i in TaskList'Range loop
      TaskList(i) := new PrintTask(Integer'Value(Argument(i)));
   end loop;
end SleepSort;
