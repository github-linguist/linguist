with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

procedure Evolution is

   -- only upper case characters allowed, and space, which uses '@' in
   -- internal representation (allowing subtype of Character).
   subtype DNA_Char is Character range '@' .. 'Z';

   -- DNA string is as long as target string.
   subtype DNA_String is String (1 .. 28);

   -- target string translated to DNA_Char string
   Target : constant DNA_String := "METHINKS@IT@IS@LIKE@A@WEASEL";

   -- calculate the 'closeness' to the target DNA.
   -- it returns a number >= 0 that describes how many chars are correct.
   -- can be improved much to make evolution better, but keep simple for
   -- this example.
   function Fitness (DNA : DNA_String) return Natural is
      Result : Natural := 0;
   begin
      for Position in DNA'Range loop
         if DNA (Position) = Target (Position) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Fitness;

   -- output the DNA using the mapping
   procedure Output_DNA (DNA : DNA_String; Prefix : String := "") is
      use Ada.Strings.Maps;
      Output_Map : Character_Mapping;
   begin
      Output_Map := To_Mapping
        (From => To_Sequence (To_Set (('@'))),
         To   => To_Sequence (To_Set ((' '))));
      Ada.Text_IO.Put (Prefix);
      Ada.Text_IO.Put (Ada.Strings.Fixed.Translate (DNA, Output_Map));
      Ada.Text_IO.Put_Line (", fitness: " & Integer'Image (Fitness (DNA)));
   end Output_DNA;

   -- DNA_Char is a discrete type, use Ada RNG
   package Random_Char is new Ada.Numerics.Discrete_Random (DNA_Char);
   DNA_Generator : Random_Char.Generator;

   -- need generator for floating type, too
   Float_Generator : Ada.Numerics.Float_Random.Generator;

   -- returns a mutated copy of the parent, applying the given mutation rate
   function Mutate (Parent        : DNA_String;
                    Mutation_Rate : Float)
                    return          DNA_String
   is
      Result : DNA_String := Parent;
   begin
      for Position in Result'Range loop
         if Ada.Numerics.Float_Random.Random (Float_Generator) <= Mutation_Rate
         then
            Result (Position) := Random_Char.Random (DNA_Generator);
         end if;
      end loop;
      return Result;
   end Mutate;

   -- genetic algorithm to evolve the string
   -- could be made a function returning the final string
   procedure Evolve (Child_Count   : Positive := 100;
                     Mutation_Rate : Float    := 0.2)
   is
      type Child_Array is array (1 .. Child_Count) of DNA_String;

      -- determine the fittest of the candidates
      function Fittest (Candidates : Child_Array) return DNA_String is
         The_Fittest : DNA_String := Candidates (1);
      begin
         for Candidate in Candidates'Range loop
            if Fitness (Candidates (Candidate)) > Fitness (The_Fittest)
            then
               The_Fittest := Candidates (Candidate);
            end if;
         end loop;
         return The_Fittest;
      end Fittest;

      Parent, Next_Parent : DNA_String;
      Children            : Child_Array;
      Loop_Counter        : Positive := 1;
   begin
      -- initialize Parent
      for Position in Parent'Range loop
         Parent (Position) := Random_Char.Random (DNA_Generator);
      end loop;
      Output_DNA (Parent, "First: ");
      while Parent /= Target loop
         -- mutation loop
         for Child in Children'Range loop
            Children (Child) := Mutate (Parent, Mutation_Rate);
         end loop;
         Next_Parent := Fittest (Children);
         -- don't allow weaker children as the parent
         if Fitness (Next_Parent) > Fitness (Parent) then
            Parent := Next_Parent;
         end if;
         -- output every 20th generation
         if Loop_Counter mod 20 = 0 then
            Output_DNA (Parent, Integer'Image (Loop_Counter) & ": ");
         end if;
         Loop_Counter := Loop_Counter + 1;
      end loop;
      Output_DNA (Parent, "Final (" & Integer'Image (Loop_Counter) & "): ");
   end Evolve;

begin
   -- initialize the random number generators
   Random_Char.Reset (DNA_Generator);
   Ada.Numerics.Float_Random.Reset (Float_Generator);
   -- evolve!
   Evolve;
end Evolution;
