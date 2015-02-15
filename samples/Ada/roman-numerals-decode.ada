Pragma Ada_2012;
Pragma Assertion_Policy( Check );

With
Unchecked_Conversion,
Ada.Text_IO;

Procedure Test_Roman_Numerals is

    -- We create an enumeration of valid characters, note that they are
    -- character-literals, this is so that we can use literal-strings,
    -- and that their size is that of Integer.
    Type Roman_Digits is ('I', 'V', 'X', 'L', 'C', 'D', 'M' )
    with Size => Integer'Size;

    -- We use a representation-clause ensure the proper integral-value
    -- of each individual character.
    For Roman_Digits use
      (
    'I' => 1,
    'V' => 5,
    'X' => 10,
    'L' => 50,
    'C' => 100,
    'D' => 500,
    'M' => 1000
      );

    -- To convert a Roman_Digit to an integer, we now only need to
    -- read its value as an integer.
    Function Convert is new Unchecked_Conversion
      (	Source => Roman_Digits,	Target => Integer );

    -- Romena_Numeral is a string of Roman_Digit.
    Type Roman_Numeral is array (Positive range <>) of Roman_Digits;

    -- The Numeral_List type is used herein only for testing
    -- and verification-data.
    Type Numeral_List is array (Positive range <>) of
      not null access Roman_Numeral;

    -- The Test_Cases subtype ensures that Test_Data and Validation_Data
    -- both contain the same number of elements, and that the indecies
    -- are the same; essentially the same as:
    --
    --    pragma Assert( Test_Data'Length = Validation_Data'Length
    --		   AND Test_Data'First = Validation_Data'First);

    subtype Test_Cases is Positive range 1..14;

    Test_Data : constant Numeral_List(Test_Cases):=
      (
       New Roman_Numeral'("III"),	-- 3
       New Roman_Numeral'("XXX"),	-- 30
       New Roman_Numeral'("CCC"),	-- 300
       New Roman_Numeral'("MMM"),	-- 3000

       New Roman_Numeral'("VII"),	-- 7
       New Roman_Numeral'("LXVI"),	-- 66
       New Roman_Numeral'("CL"),	-- 150
       New Roman_Numeral'("MCC"),	-- 1200

       New Roman_Numeral'("IV"),	-- 4
       New Roman_Numeral'("IX"),	-- 9
       New Roman_Numeral'("XC"),	-- 90

       New Roman_Numeral'("ICM"),	-- 901
       New Roman_Numeral'("CIM"),	-- 899

       New Roman_Numeral'("MDCLXVI")	-- 1666
      );

    Validation_Data : constant array(Test_Cases) of Natural:=
      (	3, 30, 300, 3000,
	7, 66, 150, 1200,
	4, 9, 90,
	901, 899,
	1666
      );


    -- In Roman numerals, the subtractive form [IV = 4] was used
    -- very infrequently, the most common form was the addidive
    -- form [IV = 6]. (Consider military logistics and squads.)

    -- SUM returns the Number, read in the additive form.
    Function Sum( Number : Roman_Numeral ) return Natural is
    begin
	Return Result : Natural:= 0 do
	    For Item of Number loop
		    Result:= Result + Convert( Item );
	    end loop;
	End Return;
    end Sum;

    -- EVAL returns Number read in the subtractive form.
    Function Eval( Number : Roman_Numeral ) return Natural is
	Current : Roman_Digits:= 'I';
    begin
	Return Result : Natural:= 0 do
	    For Item of Number loop
		if Current < Item then
		    Result:= Convert(Item) - Result;
		    Current:= Item;
		else
		    Result:= Result + Convert(Item);
		end if;
	    end loop;
	End Return;
    end Eval;

    -- Display the given Roman_Numeral via Text_IO.
    Procedure Put( S: Roman_Numeral ) is
    begin
	For Ch of S loop
	    declare
		-- The 'Image attribute returns the character inside
		-- single-quotes; so we select the character itself.
		C : Character renames Roman_Digits'Image(Ch)(2);
	    begin
		Ada.Text_IO.Put( C );
	    end;
	end loop;
    end;

    -- This displays pass/fail dependant on the parameter.
    Function PF ( Value : Boolean ) Return String is
    begin
	Return Result : String(1..4):= ( if Value then"pass"else"fail" );
    End PF;

Begin
    Ada.Text_IO.Put_Line("Starting Test:");

    for Index in Test_Data'Range loop
	declare
	    Item  : Roman_Numeral renames Test_Data(Index).all;
	    Value : constant Natural := Eval(Item);
	begin
	    Put( Item );

	    Ada.Text_IO.Put( ASCII.HT & "= ");
	    Ada.Text_IO.Put( Value'Img );
	    Ada.Text_IO.Put_Line( ASCII.HT & '[' &
			  PF( Value = Validation_Data(Index) )& ']');
	end;
    end loop;


    Ada.Text_IO.Put_Line("Testing complete.");
End Test_Roman_Numerals;
