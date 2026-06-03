unit LazComUnit;

{$mode objfpc}{$H+}

interface

uses
  ComObj, LazComLib_1_0_TLB;

type

  { TLazCom }
  TLazCom = class(TAutoObject, ILazCom)
  public
    procedure LazComMethod;safecall;
  end;

implementation

uses
  comserv;

{ TLazCom }

procedure TLazCom.LazComMethod; safecall;
begin
  WriteLn('LazComMethod called');
end;


initialization
  TAutoObjectFactory.Create(ComServer, TLazCom, CLASS_LazComCoClass,
    ciMultiInstance, tmApartment);

end.

