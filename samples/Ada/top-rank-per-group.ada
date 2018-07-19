with Ada.Containers.Vectors;
with Ada.Text_IO;

procedure Top is
   type Departments is (D050, D101, D190, D202);
   type Employee_Data is record
      Name       : String (1 .. 15);
      ID         : String (1 .. 6);
      Salary     : Positive;
      Department : Departments;
   end record;

   package Employee_Vectors is new Ada.Containers.Vectors
     (Element_Type => Employee_Data, Index_Type => Positive);

   function Compare_Salary (Left, Right : Employee_Data) return Boolean is
   begin
      return Left.Salary > Right.Salary;
   end Compare_Salary;
   package Salary_Sort is new Employee_Vectors.Generic_Sorting
     ("<" => Compare_Salary);

   function Compare_Department (Left, Right : Employee_Data) return Boolean is
   begin
      return Left.Department < Right.Department;
   end Compare_Department;
   package Department_Sort is new Employee_Vectors.Generic_Sorting
     ("<" => Compare_Department);

   Example_Data : Employee_Vectors.Vector;
begin
   -- fill data
   Example_Data.Append (("Tyler Bennett  ", "E10297", 32000, D101));
   Example_Data.Append (("John Rappl     ", "E21437", 47000, D050));
   Example_Data.Append (("George Woltman ", "E00127", 53500, D101));
   Example_Data.Append (("Adam Smith     ", "E63535", 18000, D202));
   Example_Data.Append (("Claire Buckman ", "E39876", 27800, D202));
   Example_Data.Append (("David McClellan", "E04242", 41500, D101));
   Example_Data.Append (("Rich Holcomb   ", "E01234", 49500, D202));
   Example_Data.Append (("Nathan Adams   ", "E41298", 21900, D050));
   Example_Data.Append (("Richard Potter ", "E43128", 15900, D101));
   Example_Data.Append (("David Motsinger", "E27002", 19250, D202));
   Example_Data.Append (("Tim Sampair    ", "E03033", 27000, D101));
   Example_Data.Append (("Kim Arlich     ", "E10001", 57000, D190));
   Example_Data.Append (("Timothy Grove  ", "E16398", 29900, D190));
   -- sort by salary
   Salary_Sort.Sort (Example_Data);
   -- output each department
   for Department in Departments loop
      declare
         Position : Employee_Vectors.Cursor := Example_Data.First;
         Employee : Employee_Data;
      begin
         Ada.Text_IO.Put_Line ("Department " & Departments'Image (Department));
         for I in 1 .. 3 loop
            Employee := Employee_Vectors.Element (Position);
            while Employee.Department /= Department loop
               Position := Employee_Vectors.Next (Position);
               Employee := Employee_Vectors.Element (Position);
            end loop;
            Ada.Text_IO.Put_Line ("   " & Employee.Name & " | " &
                                  Employee.ID & " | " &
                                  Positive'Image (Employee.Salary));
            Position := Employee_Vectors.Next (Position);
         end loop;
      exception
         when Constraint_Error =>
            null;
      end;
   end loop;
end Top;
