with Ada.Calendar;               use Ada.Calendar;
with Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Test_Checkpoint is

   package FR renames Ada.Numerics.Float_Random;
   No_Of_Cubicles: constant Positive := 3;
     -- That many workers can work in parallel
   No_Of_Workers: constant Positive := 6;
     -- That many workers are potentially available
     -- some will join the team when others quit the job

   type Activity_Array is array(Character) of Boolean;
     -- we want to know who is currently working

   protected Checkpoint is
      entry Deliver;
      entry Join (Label : out Character; Tolerance: out Float);
      entry Leave(Label : in Character);
   private
      Signaling     : Boolean   := False;
      Ready_Count   : Natural   := 0;
      Worker_Count  : Natural   := 0;
      Unused_Label  : Character := 'A';
      Likelyhood_To_Quit: Float := 1.0;
      Active        : Activity_Array := (others => false);
      entry Lodge;
   end Checkpoint;

   protected body Checkpoint is
      entry Join (Label : out Character; Tolerance: out Float)
      when not Signaling and Worker_Count < No_Of_Cubicles is
      begin
         Label        := Unused_Label;
         Active(Label):= True;
         Unused_Label := Character'Succ (Unused_Label);
         Worker_Count := Worker_Count + 1;
         Likelyhood_To_Quit := Likelyhood_To_Quit / 2.0;
         Tolerance    := Likelyhood_To_Quit;
      end Join;

      entry Leave(Label: in Character) when not Signaling is
      begin
         Worker_Count  := Worker_Count - 1;
         Active(Label) := False;
      end Leave;

      entry Deliver when not Signaling is
      begin
         Ready_Count := Ready_Count + 1;
         requeue Lodge;
      end Deliver;

      entry Lodge when Ready_Count = Worker_Count or Signaling is
      begin
         if Ready_Count = Worker_Count then
            Put("---Sync Point [");
            for C in Character loop
               if Active(C) then
                  Put(C);
               end if;
            end loop;
            Put_Line("]---");
         end if;
         Ready_Count := Ready_Count - 1;
         Signaling   := Ready_Count /= 0;
      end Lodge;
   end Checkpoint;

   task type Worker;

   task body Worker is
      Dice      : FR.Generator;
      Label     : Character;
      Tolerance : Float;
      Shift_End : Time := Clock + 2.0;
        -- Trade unions are hard!
   begin
      FR.Reset (Dice);
      Checkpoint.Join (Label, Tolerance);
      Put_Line(Label & " joins the team");
      loop
         Put_Line (Label & " is working");
         delay Duration (FR.Random (Dice) * 0.500);
         Put_Line (Label & " is ready");
         Checkpoint.Deliver;
         if FR.Random(Dice) < Tolerance then
            Put_Line(Label & " leaves the team");
            exit;
         elsif Clock >= Shift_End then
            Put_Line(Label & " ends shift");
            exit;
         end if;
      end loop;
      Checkpoint.Leave(Label);
   end Worker;
   Set : array (1..No_Of_Workers) of Worker;
begin
   null; -- Nothing to do here
end Test_Checkpoint;
