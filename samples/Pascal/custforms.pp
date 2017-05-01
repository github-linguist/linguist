unit custforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

Type

  { TCustomFormDescr }

  TCustomFormDescr = Class
  private
    FAuthor: String;
    FCaption: String;
    FCategory: String;
    FDescription: String;
    FFormClass: TFormClass;
    FLazPackage: String;
    FUnitName: String;
  public
    Constructor Create(AFormClass : TFormClass; const APackage: string);
    Constructor Create(AFormClass : TFormClass; Const ACaption,ADescription,AUnit,APackage : String);
    Property FormClass : TFormClass Read FFormClass Write FFormClass;
    Property Caption : String Read FCaption Write FCaption;
    Property Description : String Read FDescription Write FDescription;
    Property UnitName : String Read FUnitName Write FUnitName;
    Property Category : String Read FCategory Write FCategory;
    Property Author : String Read FAuthor Write FAuthor;
    Property LazPackage : String Read FLazPackage Write FLazPackage;
  end;

Procedure RegisterCustomForm(Descr : TCustomFormDescr);
Procedure RegisterCustomForm(AFormClass : TFormClass; const APackage: string);
Procedure RegisterCustomForm(AFormClass : TFormClass; Const AUnitName, APackage : String);

Procedure Register;

implementation

uses ProjectIntf,NewItemIntf,contnrs;

Const
  SAppFrameWork = 'Custom forms';
  SInstanceOf   = 'Create a new instance of %s';

{ TCustomFormDescr }

constructor TCustomFormDescr.Create(AFormClass: TFormClass;
  const APackage: string);

Var
  N,U : String;

begin
  N:=AFormClass.ClassName;
  U:=N;
  If (Upcase(U[1])='T') then
    Delete(U,1,1);
  Create(AFormClass,N,Format(SInstanceOf,[N]),U,APackage);
end;

constructor TCustomFormDescr.Create(AFormClass: TFormClass;
  const ACaption, ADescription, AUnit, APackage: String);
begin
  FFormClass:=AFormClass;
  FCaption:=ACaption;
  FDescription:=ADescription;
  FUnitName:=AUnit;
  FCategory:=SAppFrameWork;
  FLazPackage:=APackage;
end;

// Registration code.

Type
  { TCustomFormFileDescriptor }
  TCustomFormFileDescriptor = Class(TFileDescPascalUnitWithResource)
  private
    FFormDescr: TCustomFormDescr;
  Public
    Constructor Create(ADescr : TCustomFormDescr);
    Property FormDescr : TCustomFormDescr Read FFormDescr;
    Function GetLocalizedName : String; override;
    Function GetLocalizedDescription : String; override;
    Function GetInterfaceUsesSection : String; override;
  end;

{ TCustomFormFileDescriptor }

constructor TCustomFormFileDescriptor.Create(ADescr: TCustomFormDescr);
begin
  Inherited Create;
  FFormDescr:=ADescr;
  ResourceClass:=FFormDescr.FFormClass;
  Name:=FFormDescr.Caption;
  RequiredPackages:=ADescr.LazPackage;
  //Writeln('TCustomFormFileDescriptor.Create RequiredPackages=',RequiredPackages);
end;

function TCustomFormFileDescriptor.GetLocalizedName: String;
begin
  Result:=FFormDescr.Caption;
end;

function TCustomFormFileDescriptor.GetLocalizedDescription: String;
begin
  Result:=FFormDescr.Description;
  If (FFormDescr.Author<>'') then
    Result:=Result+LineEnding+'By '+FFormDescr.Author;
end;

function TCustomFormFileDescriptor.GetInterfaceUsesSection: String;
begin
  Result:=inherited GetInterfaceUsesSection;
  Result:=Result+',Forms,'+FFormDescr.UnitName;
end;

Var
  CustomFormList : TObjectList;

Procedure RegisterCustomForm(Descr : TCustomFormDescr);

begin
  CustomFormList.Add(Descr);
end;

Procedure RegisterCustomForm(AFormClass : TFormClass; const APackage: string);

begin
  RegisterCustomForm(TCustomFormDescr.Create(AFormClass,APackage));
end;

Procedure RegisterCustomForm(AFormClass : TFormClass; Const AUnitName, APackage : String);

Var
  D : TCustomFormDescr;

begin
  D:=TCustomFormDescr.Create(AFormClass,APackage);
  D.UnitName:=AUnitName;
  RegisterCustomForm(D);
end;


Procedure Register;

Var
  L : TStringList;
  I : Integer;
  D : TCustomFormDescr;
  
begin
  L:=TStringList.Create;
  Try
    L.Sorted:=True;
    L.Duplicates:=dupIgnore;
    For I:=0 to CustomFormList.Count-1 do
      L.Add(TCustomFormDescr(CustomFormList[i]).Category);
    For I:=0 to L.Count-1 do
    begin
      RegisterNewItemCategory(TNewIDEItemCategory.Create(L[i]));
    end;
  Finally
    L.Free;
  end;
  For I:=0 to CustomFormList.Count-1 do
    begin
    D:=TCustomFormDescr(CustomFormList[i]);
    RegisterProjectFileDescriptor(TCustomFormFileDescriptor.Create(D),D.Category);
    end;
end;

Procedure InitCustomForms;

begin
  CustomFormList:=TObjectList.Create;
end;

Procedure DoneCustomForms;

begin
  FreeAndNil(CustomFormList);
end;

Initialization
  InitCustomForms;
Finalization
  DoneCustomForms;
end.

