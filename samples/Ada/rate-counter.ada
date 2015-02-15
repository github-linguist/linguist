with System;                     use System;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Calendar;               use Ada.Calendar;
with Ada.Unchecked_Deallocation; use Ada;
with Interfaces;

procedure Rate_Counter is
   pragma Priority (Max_Priority);

   package Duration_IO is new Fixed_IO (Duration);

   Job_Nbr : constant := 6; -- adjust to your need
   subtype Job_Index is Natural range 1 .. Job_Nbr;

   task type Job (ID : Job_Index) is
      pragma Priority (Default_Priority);
      entry Start;
   end Job;

   type Job_Ptr is access Job;
   procedure Free is new Unchecked_Deallocation (Job, Job_Ptr);

   Jobs : array (Job_Index) of Job_Ptr;

   Done      : Natural                      := 0;
   Completed : array (Job_Index) of Boolean := (others => False);

   type Timings is array (Job_Index) of Calendar.Time;
   Start_T, Stop_T : Timings;

   task body Job is
      Anchor : Interfaces.Integer_32;
      pragma Volatile (Anchor); -- necessary to avoid compiler optimization.
   begin
      accept Start;

      for I in Interfaces.Integer_32'Range loop      -- the job to do
         Anchor := I;
      end loop;
   end Job;

begin
   for J in Job_Index'Range loop
      Jobs (J) := new Job (ID => J); -- create the jobs first, sync later
   end loop;
   for J in Job_Index'Range loop -- launch the jobs in parallel
      Start_T (J) := Calendar.Clock; -- get the start time
      Jobs (J).Start; -- priority settings necessary to regain control.
   end loop;
   -- Polling for the results / also possible to use a protected type.
   while not (Done = Job_Nbr) loop
      for J in Job_Index'Range loop
         if not Completed (J) and then Jobs (J)'Terminated then
            Stop_T (J) := Calendar.Clock; -- get the end time
            Put ("Job #" & Job_Index'Image (J) & " is finished. It took ");
            Duration_IO.Put (Stop_T (J) - Start_T (J), Fore => 3, Aft => 2);
            Put_Line (" seconds.");
            Completed (J) := True;
            Done          := Done + 1;
         end if;
      end loop;
      delay System.Tick; -- according to the precision of the system clock
   end loop;
   Duration_IO.Put (System.Tick, Fore => 1, Aft => 6);
   Put_Line (" seconds is the precision of System clock.");

   for J in Job_Index'Range loop
      Free (Jobs (J)); -- no GC in Ada, clean-up is explicit
   end loop;

end Rate_Counter;
