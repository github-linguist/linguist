Unit LazComLib_1_0_TLB;

//  Imported LazComLib on 19/11/2014 15:59:16 from D:\_development\Lazarus\LazComDll\LazComDll.tlb

{$mode delphi}{$H+}

interface

Uses
  Windows,ActiveX,Classes,Variants;
Const
  LazComLibMajorVersion = 1;
  LazComLibMinorVersion = 0;
  LazComLibLCID = 0;
  LIBID_LazComLib : TGUID = '{570A55B0-1122-49AE-A335-1F630EC4FE65}';

  IID_ILazCom : TGUID = '{247EAD48-16D6-4C1C-9265-F841BF4BD411}';
  CLASS_LazComCoClass : TGUID = '{93E11168-E984-415C-ACD6-853226D22CF9}';

//Enums

//Forward declarations

Type
 ILazCom = interface;
 ILazComDisp = dispinterface;

//Map CoClass to its default interface

 LazComCoClass = ILazCom;

//records, unions, aliases


//interface declarations

// ILazCom : 

 ILazCom = interface(IDispatch)
   ['{247EAD48-16D6-4C1C-9265-F841BF4BD411}']
    // LazComMethod :  
   procedure LazComMethod;safecall;
  end;


// ILazCom : 

 ILazComDisp = dispinterface
   ['{247EAD48-16D6-4C1C-9265-F841BF4BD411}']
    // LazComMethod :  
   procedure LazComMethod;dispid 201;
  end;

//CoClasses
  CoLazComCoClass = Class
  Public
    Class Function Create: ILazCom;
    Class Function CreateRemote(const MachineName: string): ILazCom;
  end;

implementation

uses comobj;

Class Function CoLazComCoClass.Create: ILazCom;
begin
  Result := CreateComObject(CLASS_LazComCoClass) as ILazCom;
end;

Class Function CoLazComCoClass.CreateRemote(const MachineName: string): ILazCom;
begin
  Result := CreateRemoteComObject(MachineName,CLASS_LazComCoClass) as ILazCom;
end;

end.
