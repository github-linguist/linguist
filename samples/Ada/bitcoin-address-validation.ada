with Ada.Exceptions, Interfaces;
with Ada.Streams;
use Ada.Exceptions, Interfaces;
use Ada.Streams;

package Bitcoin is
    subtype BT_Raw_Addr is Stream_Element_Array(1..25);
    subtype BT_Checksum is Stream_Element_Array(1..4);
    subtype BT_Addr is String(1..34);
    subtype Sha256String is String(1..64);
    Invalid_Address_Error : Exception;

    function Double_Sha256(S : Stream_Element_Array) return BT_Checksum;
    function Is_Valid(A : BT_Raw_Addr) return Boolean;
    procedure Base58_Decode(S : BT_Addr; A : out BT_Raw_Addr) ;
private
    Base58 : constant String := "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
    function Hex_Val (C, C2 : Character) return Stream_Element;
end Bitcoin;


with GNAT.SHA256, Ada.Strings.Fixed;
use  GNAT.SHA256, Ada.Strings.Fixed;

package body Bitcoin is

function Hex_Val (C, C2 : Character) return Stream_Element is
    subtype Nibble is Integer range 0..15;
    HEX : array (0..255) of Nibble := (
          48=>0, 49=>1, 50=>2, 51=>3, 52=>4, 53=>5, 54=>6, 55=>7, 56=>8, 57=>9
        , 65=>10, 66=>11, 67=>12, 68 =>13, 69 =>14, 70 =>15
        , 97=>10, 98=>11, 99=>12, 100=>13, 101=>14, 102=>15
        , Others=>0
    );
begin
    return Stream_Element(HEX(Character'Pos(C)) * 16 + HEX(Character'Pos(C2)));
end Hex_Val;

function Double_Sha256(S : Stream_Element_Array) return BT_Checksum is
    Ctx  : Context := Initial_Context;
    D : Message_Digest;
    S2 : Stream_Element_Array(1..32);
    Ctx2 : Context := Initial_Context;
    C : BT_Checksum;
begin
    Update(Ctx, S);
    D := Digest(Ctx);
    for I in S2'Range loop
        S2(I) := Hex_Val(D(Integer(I)*2-1), D(Integer(I)*2));
    end loop;
    Update(Ctx2, S2);
    D := Digest(Ctx2);
    for I in C'Range loop
        C(I) := Hex_Val(D(Integer(I)*2-1), D(Integer(I)*2));
    end loop;
    return C;

end Double_Sha256;


--------------------------------------------------------------------------------
-- Summary of Base58:                                                         --
-- We decode S into a 200 bit unsigned integer.                               --
-- We could use a BigNum library, but choose to go without.                   --
--------------------------------------------------------------------------------
procedure Base58_Decode(S : BT_Addr; A : out BT_Raw_Addr) is
begin
    A := (Others => 0);
    for I in S'Range loop
        declare
            P : Natural := Index(Base58, String(S(I..I)));
            C : Natural;
        begin
            if P = 0 then
                raise Invalid_Address_Error;
            end if;
            C := P - 1;
            for J in reverse A'Range loop
                C    := C + Natural(A(J)) * 58;
                A(J) := Stream_Element(Unsigned_32(C) and 255);         -- 0x00FF
                C    := Natural(Shift_Right(Unsigned_32(C),8) and 255); -- 0xFF00
            end loop;
            if C /= 0 then
                raise Invalid_Address_Error;
            end if;
        end;
    end loop;
end Base58_Decode;


function Is_Valid(A : BT_Raw_Addr) return Boolean is
begin
    return A(1) = 0 and A(22..25) = Double_Sha256(A(1..21));
end Is_Valid;


end Bitcoin;

with Ada.Text_IO, Bitcoin;
use Ada.Text_IO, Bitcoin;

procedure Bitcoin_Addr_Validate is
begin
    declare
        BTs : array (positive range <>) of BT_Addr := (
              "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"  -- VALID
            , "1Q1pE5vPGEEMqRcVRMbtBK842Y6Pzo6nK9"  -- VALID
            , "1AGNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62X"  -- checksum changed, original data.
            , "1ANNa15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"  -- data changed, original checksum.
            , "1A Na15ZQXAZUgFiqJ2i7Z2DPU2J6hW62i"  -- invalid chars
        );
    begin
        for I in Bts'Range loop
            declare
                A : BT_Raw_Addr;
                Valid : Boolean;
            begin
                Put(BTs(I) & " validity: ");
                Base58_Decode(BTs(I), A);
                Valid := Is_Valid(A);
                Put_Line(Boolean'Image(Valid));
            exception
                when E : Invalid_Address_Error  =>
                    Put_Line ("*** Error: Invalid BT address.");
            end;
        end loop;
    end;
end Bitcoin_Addr_Validate;
