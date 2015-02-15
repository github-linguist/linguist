with Ada.Text_IO; use Ada.Text_IO;
procedure ResistMesh is
   H, W : constant Positive := 10;
   rowA, colA : constant Positive := 2; -- row/col indexed from 1
   rowB : constant Positive := 7;
   colB : constant Positive := 8;

   type Ntype is (A, B, Free);
   type Vtype is digits 15;
   type Node is record
      volt : Vtype := 0.0;
      name : Ntype := Free;
   end record;
   type NodeMesh is array (Positive range <>, Positive range <>) of Node;
   package IIO is new Ada.Text_IO.Float_IO (Vtype);
   mesh, dmesh : NodeMesh (1 .. H, 1 .. W);
   curA, curB, diff : Vtype;

   procedure set_AB (mesh : in out NodeMesh) is begin
      mesh (rowA, colA).volt :=  1.0;  mesh (rowA, colA).name := A;
      mesh (rowB, colB).volt := -1.0;  mesh (rowB, colB).name := B;
   end set_AB;

   function sides (i : Positive; j : Positive) return Vtype is
      s : Integer := 0;
   begin
      if i /= 1 and i /= H then s := s + 2; else s := s + 1; end if;
      if j /= 1 and j /= W then s := s + 2; else s := s + 1; end if;
      return Vtype (s);
   end sides;

   procedure calc_diff (mesh : NodeMesh; dmesh : out NodeMesh;
      total : out Vtype)  is
      n : Natural;
      v : Vtype := 0.0;
   begin
      total := 0.0;
      for i in Positive range 1 .. H loop
         for j in Positive range 1 .. W loop
            n := 0;    v := 0.0;
            if  i /= 1 then v := v + mesh (i - 1, j).volt; n := n + 1; end if;
            if j /= 1 then  v := v + mesh (i, j - 1).volt; n := n + 1; end if;
            if i < H then v := v + mesh (i + 1, j).volt; n := n + 1; end if;
            if j < W then v := v + mesh (i, j + 1).volt; n := n + 1; end if;
            v := mesh (i, j).volt - v / Vtype (n);
            dmesh (i, j).volt := v;
            if mesh (i, j).name = Free then total := total + v ** 2; end if;
         end loop;
      end loop;
   end calc_diff;

begin

   loop
      set_AB (mesh);
      calc_diff (mesh, dmesh, diff);
      exit when diff < 1.0e-40;
      for i in Positive range 1 .. H loop
         for j in Positive range 1 .. W loop
            mesh (i, j).volt := mesh (i, j).volt - dmesh (i, j).volt;
         end loop;
      end loop;
   end loop;

   curA := dmesh (rowA, colA).volt * sides (rowA, colA);
   curB := dmesh (rowB, colB).volt * sides (rowB, colB);
   diff := 4.0 / (curA - curB);
   IIO.Put (diff, Exp => 0); New_Line;
end ResistMesh;
