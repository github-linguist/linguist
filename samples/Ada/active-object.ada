with Ada.Calendar;                       use Ada.Calendar;
with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                        use Ada.Text_IO;

procedure Test_Integrator is
   type Func is access function (T : Time) return Float;

   function Zero (T : Time) return Float is
   begin
      return 0.0;
   end Zero;

   Epoch : constant Time := Clock;

   function Sine (T : Time) return Float is
   begin
      return Sin (Pi * Float (T - Epoch));
   end Sine;

   task type Integrator is
      entry Input  (Value : Func);
      entry Output (Value : out Float);
      entry Shut_Down;
   end Integrator;

   task body Integrator is
      K  : Func  := Zero'Access;
      S  : Float := 0.0;
      F0 : Float := 0.0;
      F1 : Float;
      T0 : Time  := Clock;
      T1 : Time;
   begin
      loop
         select
            accept Input (Value : Func) do
               K := Value;
            end Input;
         or accept Output (Value : out Float) do
               Value := S;
            end Output;
         or accept Shut_Down;
            exit;
         else
            T1 := Clock;
            F1 := K (T1);
            S  := S + 0.5 * (F1 + F0) * Float (T1 - T0);
            T0 := T1;
            F0 := F1;
         end select;
      end loop;
   end Integrator;

   I : Integrator;
   S : Float;
begin
   I.Input (Sine'Access);
   delay 2.0;
   I.Input (Zero'Access);
   delay 0.5;
   I.Output (S);
   Put_Line ("Integrated" & Float'Image (S) & "s");
   I.Shut_Down;
end Test_Integrator;
